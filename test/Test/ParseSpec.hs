module Test.ParseSpec where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import NSO.Metadata
import NSO.Prelude
import Test.Syd


spec :: Spec
spec = do
  describe "Metadata Parser" $ do
    it "should parse local mock file" $ do
      inp <- BL.readFile "deps/datasets.json"
      case eitherDecode @(GraphQLData AllDatasets) inp of
        Left e -> fail e
        Right _ -> pure ()


data GraphQLData a = GraphQLData {_data :: a}
  deriving (Generic)


instance (FromJSON a) => FromJSON (GraphQLData a) where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
