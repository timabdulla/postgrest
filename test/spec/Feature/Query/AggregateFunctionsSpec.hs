module Feature.Query.AggregateFunctionsSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config.PgVersion (PgVersion, pgVersion112)

import Protolude  hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec actualPgVersion =
  describe "aggregate functions" $ do
    context "performing a count without specifying a field" $ do
      it "returns the count of all rows when no other fields are selected" $
        get "/entities?select=count()" `shouldRespondWith`
          [json|[{ "count": 4 }]|] { matchHeaders = [matchContentTypeJson] }
      it "allows you to specify an alias for the count" $
        get "/entities?select=cnt:count()" `shouldRespondWith`
          [json|[{ "cnt": 4 }]|] { matchHeaders = [matchContentTypeJson] }
      it "allows you to cast the result of the count" $
        get "/entities?select=count()::text" `shouldRespondWith`
          [json|[{ "cnt": "4" }]|] { matchHeaders = [matchContentTypeJson] }
      it "returns the count grouped by all provided fields when other fields are selected" $
        get "/projects?select=c:count(),client_id" `shouldRespondWith`
          [json|[{ "c": 1, "client_id": null }, { "c": 2, "client_id": 2 }, { "c": 2, "client_id": 1}|] { matchHeaders = [matchContentTypeJson] }
    context "performing an aggregation on a field" $ do
      context "when no other fields are selected"
      it "when using sum, it returns the sum of all values in the field when no other fields are selected" $
        get "/car_model_sales?select=quantity.sum()" `shouldRespondWith`
          [json|[{ "sum": 20 }]|] { matchHeaders = [matchContentTypeJson] }
      it "when using avg, it returns the avg of all values in the field when no other fields are selected" $
        get "/car_model_sales?select=quantity.avg()" `shouldRespondWith`
          [json|[{ "sum": 5.0000000000000000 }]|] { matchHeaders = [matchContentTypeJson] }
      it "when using min, it returns the min of all values in the field when no other fields are selected" $
        get "/car_model_sales?select=quantity.avg()" `shouldRespondWith`
          [json|[{ "sum": 5.0000000000000000 }]|] { matchHeaders = [matchContentTypeJson] }
          -- Error conditions?
