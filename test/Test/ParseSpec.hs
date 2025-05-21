module Test.ParseSpec where

import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.Error.Static
import Effectful.GraphQL
import NSO.Metadata
import NSO.Prelude
import Skeletest


spec :: Spec
spec = do
  describe "Metadata Parser" $ do
    it "should parse local mock file" $ do
      inp <- BL.readFile "deps/datasets.json"
      let q = DatasetInventories mempty mempty
      case runPureEff $ runErrorNoCallStack @GraphQLError $ parseResponse q inp of
        Left e -> fail $ show e
        Right _ -> pure ()
