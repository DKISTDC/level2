module Test.ParseSpec where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Effectful.GraphQL
import NSO.Metadata
import NSO.Prelude
import Skeletest


spec :: Spec
spec = do
  describe "Metadata Parser" $ do
    it "should parse local mock file" $ do
      inp <- BL.readFile "deps/datasets.json"
      let q = DatasetInventories Nothing
      case parseQueryResponse q =<< eitherDecode inp of
        Left e -> fail e
        Right _ -> pure ()
