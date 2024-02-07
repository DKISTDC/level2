-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.Aeson (FromJSON (..), Options (..), defaultOptions, eitherDecode, genericParseJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Morpheus.Client hiding (fetch)
import Data.Morpheus.Client.CodeGen.Internal (OperationType (..))
import Data.String.Interpolate
import GHC.Generics
import NSO.Metadata.Types
import NSO.Prelude


-- import Effectful.Request
-- import Network.HTTP.Req

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
mockRequest _ r = do
  rq <- parseRequest r
  putStrLn $ "MOCK Graphql: " <> cs rq.operationName
  case rq.operationName of
    "AllDatasets" -> L.readFile "deps/datasets.json"
    "AllExperiments" -> L.readFile "deps/experiments.json"
    op -> fail $ "GraphQL Request not mocked: " <> cs op


parseRequest :: ByteString -> IO GraphQLRequest
parseRequest r = do
  either fail pure $ eitherDecode r


data GraphQLRequest = GraphQLRequest
  {operationName :: Text}
  deriving (Generic, FromJSON)


data DataResponse a = DataResponse
  { _data :: a
  }
  deriving (Generic)


instance (FromJSON a) => FromJSON (DataResponse a) where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}

-- mockDatasetInventories :: IO [DatasetInventory]
-- mockDatasetInventories = do
--   bs <- L.readFile "deps/datasets.json"
--   case (eitherDecode bs :: Either String (DataResponse AllDatasets)) of
--     Left e -> fail e
--     Right (DataResponse d) -> pure d.datasetInventories
--
--
-- mockExperiments :: IO [ExperimentDescription]
-- mockExperiments = do
--   bs <- L.readFile "deps/experiments.json"
--   case (eitherDecode bs :: Either String (DataResponse AllExperiments)) of
--     Left e -> fail e
--     Right (DataResponse d) -> pure d.experimentDescriptions
