-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Morpheus.Client hiding (fetch)
import Data.Morpheus.Client.CodeGen.Internal (OperationType (..))
import Data.String.Interpolate
import Effectful.Request
import GHC.Generics
import NSO.Metadata.Types
import NSO.Prelude
import Network.HTTP.Req


newtype AllDatasets = AllDatasets {datasetInventories :: [DatasetInventory]}
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON)


instance RequestType AllDatasets where
  type RequestArgs AllDatasets = ()
  __name _ = "AllDatasets"
  __query _ =
    let fields = genQueryFields @DatasetInventory Proxy
     in [i| query AllDatasets { datasetInventories { #{fields} } } |]
  __type _ = OPERATION_QUERY


newtype AllExperiments = AllExperiments {experimentDescriptions :: [ExperimentDescription]}
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON)


instance RequestType AllExperiments where
  type RequestArgs AllExperiments = ()
  __name _ = "AllExperiments"
  __query _ =
    let fields = genQueryFields @ExperimentDescription Proxy
     in [i| query AllExperiments { experimentDescriptions { #{fields} } } |]
  __type _ = OPERATION_QUERY


mockRequest :: Text -> ByteString -> IO ByteString
mockRequest "http://internal-api-gateway.service.prod.consul/graphql" r = do
  rq <- parseRequest r
  putStrLn $ "MOCK Graphql: " <> cs rq.operationName
  case rq.operationName of
    "AllDatasets" -> L.readFile "deps/datasets4.json"
    "AllExperiments" -> L.readFile "deps/experiments.json"
    op -> fail $ "GraphQL Request not mocked: " <> cs op
mockRequest url _ = do
  error $ "URL Not Mocked: " <> show url


parseRequest :: ByteString -> IO GraphQLRequest
parseRequest r = do
  either fail pure $ eitherDecode r


data GraphQLRequest = GraphQLRequest
  {operationName :: Text}
  deriving (Generic, FromJSON)


metadata :: Service
metadata = Service $ http "internal-api-gateway.service.prod.consul" /: "graphql"
