module Test.ParseSpec where

import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.Error.Static
import Effectful.GraphQL
import NSO.Metadata
import NSO.Prelude
import NSO.Types.Wavelength
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

  describe "SpectralLine parser" $ do
    it "parses basic lines" $ do
      parseSpectralLine "Fe I (630.15 nm)" `shouldBe` SpectralLine (Just FeI) Nothing 630.15
      parseSpectralLine "Ca II (854.21 nm)" `shouldBe` SpectralLine (Just CaII) Nothing 854.21
      parseSpectralLine "Mg I b1 (517.27 nm)" `shouldBe` SpectralLine Nothing (Just "B1") 854.21
      parseSpectralLine "Na I D2 (589.0 nm)" `shouldBe` SpectralLine Nothing (Just "D2") 854.21

      
