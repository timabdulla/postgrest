{-|
Module      : PostgREST.App
Description : PostgREST main application

This module is in charge of mapping HTTP requests to PostgreSQL queries.
Some of its functionality includes:

- Mapping HTTP request methods to proper SQL statements. For example, a GET request is translated to executing a SELECT query in a read-only TRANSACTION.
- Producing HTTP Headers according to RFCs.
- Content Negotiation
-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.App
  ( SignalHandlerInstaller
  , SocketRunner
  , postgrest
  , run
  ) where


import Control.Monad.Except     (liftEither)
import Data.Either.Combinators  (mapLeft)
import Data.Maybe               (fromJust)
import Data.String              (IsString (..))
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort,
                                 setServerName)
import System.Posix.Types       (FileMode)

import qualified Data.HashMap.Strict        as HM
import qualified Data.Text.Encoding         as T
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp

import qualified PostgREST.Admin            as Admin
import qualified PostgREST.ApiRequest       as ApiRequest
import qualified PostgREST.ApiRequest.Types as ApiRequestTypes
import qualified PostgREST.AppState         as AppState
import qualified PostgREST.Auth             as Auth
import qualified PostgREST.Cors             as Cors
import qualified PostgREST.Error            as Error
import qualified PostgREST.Logger           as Logger
import qualified PostgREST.Plan             as Plan
import qualified PostgREST.Query            as Query
import qualified PostgREST.Response         as Response

import PostgREST.ApiRequest          (Action (..), ApiRequest (..),
                                      Mutation (..), Target (..))
import PostgREST.AppState            (AppState)
import PostgREST.Auth                (AuthResult (..))
import PostgREST.Config              (AppConfig (..))
import PostgREST.Config.PgVersion    (PgVersion (..))
import PostgREST.Error               (Error)
import PostgREST.Query               (DbHandler)
import PostgREST.Response            (ServerTimingParams (..))
import PostgREST.SchemaCache         (SchemaCache (..))
import PostgREST.SchemaCache.Routine (Routine (..))
import PostgREST.Version             (docsVersion, prettyVersion)

import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L
import qualified Network.HTTP.Types    as HTTP
import           Protolude             hiding (Handler)

type Handler = ExceptT Error

type SignalHandlerInstaller = AppState -> IO()

type SocketRunner = Warp.Settings -> Wai.Application -> FileMode -> FilePath -> IO()

run :: SignalHandlerInstaller -> Maybe SocketRunner -> AppState -> IO ()
run installHandlers maybeRunWithSocket appState = do
  conf@AppConfig{..} <- AppState.getConfig appState
  AppState.connectionWorker appState -- Loads the initial SchemaCache
  installHandlers appState
  -- reload schema cache + config on NOTIFY
  AppState.runListener conf appState

  Admin.runAdmin conf appState $ serverSettings conf

  let app = postgrest conf appState (AppState.connectionWorker appState)

  case configServerUnixSocket of
    Just socket ->
      -- run the postgrest application with user defined socket. Only for UNIX systems
      case maybeRunWithSocket of
        Just runWithSocket -> do
          AppState.logWithZTime appState $ "Listening on unix socket " <> show socket
          runWithSocket (serverSettings conf) app configServerUnixSocketMode socket
        Nothing ->
          panic "Cannot run with unix socket on non-unix platforms."
    Nothing ->
      do
        AppState.logWithZTime appState $ "Listening on port " <> show configServerPort
        Warp.runSettings (serverSettings conf) app

serverSettings :: AppConfig -> Warp.Settings
serverSettings AppConfig{..} =
  defaultSettings
    & setHost (fromString $ toS configServerHost)
    & setPort configServerPort
    & setServerName ("postgrest/" <> prettyVersion)

-- | PostgREST application
postgrest :: AppConfig -> AppState.AppState -> IO () -> Wai.Application
postgrest conf appState connWorker =
  traceHeaderMiddleware conf .
  Cors.middleware .
  Auth.middleware appState .
  Logger.middleware (configLogLevel conf) $
    -- fromJust can be used, because the auth middleware will **always** add
    -- some AuthResult to the vault.
    \req respond -> case fromJust $ Auth.getResult req of
      Left err -> respond $ Error.errorResponseFor err
      Right authResult -> do
        appConf <- AppState.getConfig appState -- the config must be read again because it can reload
        maybeSchemaCache <- AppState.getSchemaCache appState
        pgVer <- AppState.getPgVersion appState

        let
          eitherResponse :: IO (Either Error Wai.Response)
          eitherResponse =
            runExceptT $ postgrestResponse appState appConf maybeSchemaCache pgVer authResult req

        response <- either Error.errorResponseFor identity <$> eitherResponse
        -- Launch the connWorker when the connection is down.  The postgrest
        -- function can respond successfully (with a stale schema cache) before
        -- the connWorker is done.
        when (isServiceUnavailable response) connWorker
        resp <- do
          delay <- AppState.getRetryNextIn appState
          return $ addRetryHint delay response
        respond resp

postgrestResponse
  :: AppState.AppState
  -> AppConfig
  -> Maybe SchemaCache
  -> PgVersion
  -> AuthResult
  -> Wai.Request
  -> Handler IO Wai.Response
postgrestResponse appState conf@AppConfig{..} maybeSchemaCache pgVer authResult@AuthResult{..} req = do
  sCache <-
    case maybeSchemaCache of
      Just sCache ->
        return sCache
      Nothing ->
        throwError Error.NoSchemaCacheError

  body <- lift $ Wai.strictRequestBody req

  apiRequest <-
    liftEither . mapLeft Error.ApiRequestError $
      ApiRequest.userApiRequest conf req body

  let serverTimingParams = if configDbPlanEnabled then Just (ServerTimingParams { jwtDur = fromJust $ Auth.getJwtDur req }) else Nothing
  handleRequest authResult conf appState (Just authRole /= configDbAnonRole) configDbPreparedStatements pgVer apiRequest sCache serverTimingParams

runDbHandler :: AppState.AppState -> SQL.IsolationLevel -> SQL.Mode -> Bool -> Bool -> DbHandler b -> Handler IO b
runDbHandler appState isoLvl mode authenticated prepared handler = do
  dbResp <- lift $ do
    let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction
    AppState.usePool appState . transaction isoLvl mode $ runExceptT handler

  resp <-
    liftEither . mapLeft Error.PgErr $
      mapLeft (Error.PgError authenticated) dbResp

  liftEither resp

handleRequest :: AuthResult -> AppConfig -> AppState.AppState -> Bool -> Bool -> PgVersion -> ApiRequest -> SchemaCache -> Maybe ServerTimingParams -> Handler IO Wai.Response
handleRequest AuthResult{..} conf appState authenticated prepared pgVer apiReq@ApiRequest{..} sCache serverTimingParams =
  case (iAction, iTarget) of
    (ActionRead headersOnly, TargetIdent identifier) -> do
      wrPlan <- liftEither $ Plan.wrappedReadPlan identifier conf sCache apiReq
      resultSet <- runQuery roleIsoLvl (Plan.wrTxMode wrPlan) $ Query.readQuery wrPlan conf apiReq
      pgrst <- liftEither $ Response.readResponse wrPlan headersOnly identifier apiReq resultSet serverTimingParams
      return $ pgrstResponse pgrst

    (ActionMutate MutationCreate, TargetIdent identifier) -> do
      mrPlan <- liftEither $ Plan.mutateReadPlan MutationCreate apiReq identifier conf sCache
      resultSet <- runQuery roleIsoLvl (Plan.mrTxMode mrPlan) $ Query.createQuery mrPlan apiReq conf
      pgrst <- liftEither $ Response.createResponse identifier mrPlan apiReq resultSet serverTimingParams
      return $ pgrstResponse pgrst

    (ActionMutate MutationUpdate, TargetIdent identifier) -> do
      mrPlan <- liftEither $ Plan.mutateReadPlan MutationUpdate apiReq identifier conf sCache
      resultSet <- runQuery roleIsoLvl (Plan.mrTxMode mrPlan) $ Query.updateQuery mrPlan apiReq conf
      pgrst <- liftEither $ Response.updateResponse mrPlan apiReq resultSet serverTimingParams
      return $ pgrstResponse pgrst

    (ActionMutate MutationSingleUpsert, TargetIdent identifier) -> do
      mrPlan <- liftEither $ Plan.mutateReadPlan MutationSingleUpsert apiReq identifier conf sCache
      resultSet <- runQuery roleIsoLvl (Plan.mrTxMode mrPlan) $ Query.singleUpsertQuery mrPlan apiReq conf
      pgrst <- liftEither $ Response.singleUpsertResponse mrPlan apiReq resultSet serverTimingParams
      return $ pgrstResponse pgrst

    (ActionMutate MutationDelete, TargetIdent identifier) -> do
      mrPlan <- liftEither $ Plan.mutateReadPlan MutationDelete apiReq identifier conf sCache
      resultSet <- runQuery roleIsoLvl (Plan.mrTxMode mrPlan) $ Query.deleteQuery mrPlan apiReq conf
      pgrst <- liftEither $ Response.deleteResponse mrPlan apiReq resultSet serverTimingParams
      return $ pgrstResponse pgrst

    (ActionInvoke invMethod, TargetProc identifier _) -> do
      cPlan <- liftEither $ Plan.callReadPlan identifier conf sCache apiReq invMethod
      resultSet <- runQuery (fromMaybe roleIsoLvl $ pdIsoLvl (Plan.crProc cPlan))(Plan.crTxMode cPlan) $ Query.invokeQuery (Plan.crProc cPlan) cPlan apiReq conf pgVer
      pgrst <- liftEither $ Response.invokeResponse cPlan invMethod (Plan.crProc cPlan) apiReq resultSet serverTimingParams
      return $ pgrstResponse pgrst

    (ActionInspect headersOnly, TargetDefaultSpec tSchema) -> do
      iPlan <- liftEither $ Plan.inspectPlan conf apiReq
      oaiResult <- runQuery roleIsoLvl (Plan.ipTxmode iPlan) $ Query.openApiQuery sCache pgVer conf tSchema
      pgrst <- liftEither $ Response.openApiResponse (T.decodeUtf8 prettyVersion, docsVersion) headersOnly oaiResult conf sCache iSchema iNegotiatedByProfile
      return $ pgrstResponse pgrst

    (ActionInfo, TargetIdent identifier) -> do
      pgrst <- liftEither $ Response.infoIdentResponse identifier sCache
      return $ pgrstResponse pgrst

    (ActionInfo, TargetProc identifier _) -> do
      cPlan <- liftEither $ Plan.callReadPlan identifier conf sCache apiReq ApiRequest.InvHead
      pgrst <- liftEither $ Response.infoProcResponse (Plan.crProc cPlan)
      return $ pgrstResponse pgrst

    (ActionInfo, TargetDefaultSpec _) -> do
      pgrst <- liftEither Response.infoRootResponse
      return $ pgrstResponse pgrst

    _ ->
      -- This is unreachable as the ApiRequest.hs rejects it before
      -- TODO Refactor the Action/Target types to remove this line
      throwError $ Error.ApiRequestError ApiRequestTypes.NotFound
  where
    roleSettings = fromMaybe mempty (HM.lookup authRole $ configRoleSettings conf)
    roleIsoLvl = HM.findWithDefault SQL.ReadCommitted authRole $ configRoleIsoLvl conf
    runQuery isoLvl mode query =
      runDbHandler appState isoLvl mode authenticated prepared $ do
        Query.setPgLocals conf authClaims authRole (HM.toList roleSettings) apiReq pgVer
        Query.runPreReq conf
        query

    pgrstResponse :: Response.PgrstResponse -> Wai.Response
    pgrstResponse (Response.PgrstResponse st hdrs bod) = Wai.responseLBS st hdrs bod

traceHeaderMiddleware :: AppConfig -> Wai.Middleware
traceHeaderMiddleware AppConfig{configServerTraceHeader} app req respond =
  case configServerTraceHeader of
    Nothing -> app req respond
    Just hdr ->
      let hdrVal = L.lookup hdr $ Wai.requestHeaders req in
      app req (respond . Wai.mapResponseHeaders ([(hdr, fromMaybe mempty hdrVal)] ++))

addRetryHint :: Int -> Wai.Response -> Wai.Response
addRetryHint delay response = do
  let h = ("Retry-After", BS.pack $ show delay)
  Wai.mapResponseHeaders (\hs -> if isServiceUnavailable response then h:hs else hs) response

isServiceUnavailable :: Wai.Response -> Bool
isServiceUnavailable response = Wai.responseStatus response == HTTP.status503
