{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NSO.Metadata where

import Control.Monad
import Data.Morpheus.Client

-- import Data.Morpheus.Types
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601
import GHC.Generics

newtype DateTime = DateTime UTCTime
  deriving (Show, Eq, Generic)
  deriving newtype (ISO8601)

newtype JSONString = JSONString Text
  deriving (Show, Eq, Generic)
  deriving newtype (EncodeScalar, DecodeScalar)

instance EncodeScalar DateTime where
  encodeScalar (DateTime x) = String $ Text.pack $ iso8601Show x

instance DecodeScalar DateTime where
  decodeScalar (String s) = iso8601ParseM $ Text.unpack s
  decodeScalar _ = Left $ "Cannot decode DateTime"

-- instance EncodeScalar DateTime where
--   encodeScalar (DateTime x) = String $ pack $ iso8601Show x
--
-- instance DecodeScalar DateTime where

-- this only defines a few types
declareGlobalTypes "deps/metadata.graphql"

-- data AllDatasets' = AllDatasets'
--   -- I suspect this field must be named correctly
--   { datasetInventories :: [Dataset]
--   }
--   deriving (Show, Eq)
--
-- -- instance RequestType AllDatasets' where
-- --   type RequestArgs AllDatasets' = ()
-- --   __name ::
--
-- data Dataset = Dataset
--   { datasetId :: Text
--   , isEmbargoed :: Bool
--   }
--   deriving (Show, Eq)

-- instance RequestType AllDatasets' where
--   type RequestArgs AllDatasets' = ()
--   __name _ = "AllDatasets'"
--   __query _ = " query AllDatasets { datasetInventories { datasetId isEmbargoed } }  "
--   __type _ = Str

-- instance RequestType AllDatasets' where
--   type RequestArgs AllDatasets' = ()

--   asdf = _

-- type RequestArgs AllDatasets' = ()

-- data Query m = Query
--   { datasets :: m [Dataset m]
--   }
--   deriving (Generic, GQLType)
--
-- data Dataset m = Dataset
--   { datasetId :: m Text
--   , isEmbargoed :: m Bool
--   }
--   deriving (Generic, GQLType)

-- TODO: either accept the maybes and have a second parsing step
-- or, write your own schema file... not the end of the world, but we
-- could easily mistype something.
-- However, this could be handled in a test with real server data (with difficulty)
-- Yeah I could just test parsing with real data
--
-- TODO: accept maybes in parsing step. Write a second function that parses all
-- the maybe nonsense into MY types. Then I could use names, etc

declareLocalTypesInline
  "deps/metadata.graphql"
  [raw|
    query AllDatasets {
      datasetInventories {
        datasetId
        isEmbargoed
      }
    }
  |]

--
-- -- https://morpheusgraphql.com/client
client :: GQLClient
client = "http://internal-api-gateway.service.prod.consul/graphql"

fetchAll :: IO (ResponseStream AllDatasets)
fetchAll = request client ()

data Dataset = Dataset
  { datasetId :: Text
  , isEmbargoed :: Bool
  }
  deriving (Show, Eq)

-- NOTE: these aren't SO bad. They are a little annoying
-- could be generically derived? ... Maybe
-- They wouldn't take that long to write. Maintenance is typed well
parseAllDatasets :: AllDatasets -> Either (ParseError AllDatasets) [Dataset]
parseAllDatasets res = first (ParseError res) $ do
  -- TODO: we need to keep the context around...
  -- like we are pasing a particular thing
  divs <- parse ".datasetInventories" res.datasetInventories
  forM divs $ \mads -> do
    ads <- parse "DatasetInventory Object" mads
    i <- parse ".datasetId" ads.datasetId
    e <- parse ".isEmbargoed" ads.isEmbargoed
    pure $ Dataset i e

-- this is the `either` function, right?
parse :: String -> Maybe a -> Either String a
parse e Nothing = Left $ "Missing: " <> e
parse _ (Just v) = Right v

data ParseError a = ParseError a String
  deriving (Show)

test :: IO ()
test = do
  putStrLn "HELLO"
  let woot = AllDatasets $ Just [Just $ AllDatasetsDatasetInventories (Just "T") Nothing]
  print $ parseAllDatasets woot
  pure ()

-- res <- fetchAll
-- -- forEach print res
-- ads <- single res
--
-- case ads of
--   Left e -> print e
--   Right (v :: AllDatasets) -> do
--     let (rest :: Either ParseError [Dataset]) = take 2 <$> parseAllDatasets v
--     print rest

-- {datasetInventories {
--   datasetId,
--   createDate,
--   wavelengthMin
-- }}
--
--
-- {
-- "data": {
--   "datasetInventories": [
--     {
--       "datasetId": "BLKGA",
--       "createDate": "2022-12-08T19:07:55.038280",
--       "wavelengthMin": 486
--     },
--

-- {datasetInventories(datasetIds: "BONJA") {
--   datasetId,
--   datasetSize,
--   datasetInventoryId,
--   asdfObjectKey,
--   createDate,
--   updateDate,
--   startTime,
--   stokesParameters,
--   targetTypes,
--   wavelengthMin,
--   wavelengthMax,
--   isActive,
--   isEmbargoed,
--   embargoEndDate,
--   bucket,
--   endTime,
--   frameCount,
--   instrumentName,
--   originalFrameCount,
--   primaryProposalId,
--   recipeId,
--   recipeRunId,
--   recipeInstanceId,
--   hasSpectralAxis,
--   hasTemporalAxis,
--   qualityReportObjectKey,
--   infoUrl,
-- }}
