module Feature.Query.AggregateFunctionsSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config.PgVersion (PgVersion)

import Protolude  hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec _ =
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
          [json|[{ "count": "4" }]|] { matchHeaders = [matchContentTypeJson] }
      it "returns the count grouped by all provided fields when other fields are selected" $
        get "/projects?select=c:count(),client_id" `shouldRespondWith`
          [json|[{ "c": 1, "client_id": null }, { "c": 2, "client_id": 2 }, { "c": 2, "client_id": 1}]|] { matchHeaders = [matchContentTypeJson] }
    context "performing an aggregation on one or more fields" $ do
      it "supports sum()" $
        get "/car_model_sales?select=quantity.sum()" `shouldRespondWith`
          [json|[{ "sum": 20 }]|] { matchHeaders = [matchContentTypeJson] }
      it "supports avg()" $
        get "/car_model_sales?select=quantity.avg()" `shouldRespondWith`
          [json|[{ "avg": 5.0000000000000000 }]|] { matchHeaders = [matchContentTypeJson] }
      it "supports min()" $
        get "/car_model_sales?select=quantity.min()" `shouldRespondWith`
          [json|[{ "min": 1 }]|] { matchHeaders = [matchContentTypeJson] }
      it "supports max()" $
        get "/car_model_sales?select=quantity.max()" `shouldRespondWith`
          [json|[{ "max": 9 }]|] { matchHeaders = [matchContentTypeJson] }
      it "supports count()" $
        get "/car_model_sales?select=car_model_name.count()" `shouldRespondWith`
          [json|[{ "count": 4 }]|] { matchHeaders = [matchContentTypeJson] }
      it "groups by any fields selected that do not have an aggregate applied" $
        get "/car_model_sales?select=quantity.sum(),quantity.max(),date.min(),car_model_name" `shouldRespondWith`
          [json|[{ "sum": 4, "max": 3, "min": "2021-02-11", "car_model_name": "Murcielago"}, { "sum": 16, "max": 9, "min": "2021-01-14", "car_model_name": "DeLorean"}]|] { matchHeaders = [matchContentTypeJson] }
      it "supports the use of aliases on fields that will be used in the group by" $
        get "/car_model_sales?select=quantity.sum(),quantity.max(),date.min(),cm:car_model_name" `shouldRespondWith`
          [json|[{ "sum": 4, "max": 3, "min": "2021-02-11", "cm": "Murcielago"}, { "sum": 16, "max": 9, "min": "2021-01-14", "cm": "DeLorean"}]|] { matchHeaders = [matchContentTypeJson] }
      it "allows you to specify an alias for the aggregate" $
        get "/car_model_sales?select=total_sold:quantity.sum(),car_model_name" `shouldRespondWith`
          [json|[{ "total_sold": 4, "car_model_name": "Murcielago"}, { "total_sold": 16, "car_model_name": "DeLorean" }]|] { matchHeaders = [matchContentTypeJson] }
      it "allows you to cast the result of the aggregate" $
        get "/car_model_sales?select=total_sold:quantity.sum()::text,car_model_name" `shouldRespondWith`
          [json|[{ "total_sold": "4", "car_model_name": "Murcielago"}, { "total_sold": "16", "car_model_name": "DeLorean" }]|] { matchHeaders = [matchContentTypeJson] }
      it "allows you to cast the input argument of the aggregate" $
        get "/trash_details?select=jsonb_col->>key::integer.sum()" `shouldRespondWith`
          [json|[{"sum": 24}]|] { matchHeaders = [matchContentTypeJson] }
      it "allows the combination of an alias, a before cast, and an after cast" $
        get "/trash_details?select=s:jsonb_col->>key::integer.sum()::text" `shouldRespondWith`
          [json|[{"s": "24"}]|] { matchHeaders = [matchContentTypeJson] }
      it "supports use of aggregates on RPC functions that return table values" $
        get "/rpc/getallprojects?select=id.max()" `shouldRespondWith`
          [json|[{"max": 5}]|] { matchHeaders = [matchContentTypeJson] }
    context "performing aggregations on spreaded fields from an embedded resource" $ do
      it "supports the use of aggregates on spreaded fields" $
        get "/car_models?select=car_brand_name,...car_model_sales(quantity.sum())" `shouldRespondWith`
          [json|[{"car_brand_name": "dog"}]|] { matchHeaders = [matchContentTypeJson] }



            -- Error conditionselec
            -- Embedded
            -- Resource representation
