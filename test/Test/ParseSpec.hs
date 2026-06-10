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

  describe "SpectralLine parser" $ withMarkers ["focus"] $ do
    it "parses basic lines" $ do
      parseSpectralLine "Fe I (630.15 nm)" `shouldBe` Right (SpectralLine FeI Nothing 630.15)
      parseSpectralLine "Ca II (854.21 nm)" `shouldBe` Right (SpectralLine CaII Nothing 854.21)
      parseSpectralLine "Sr I (460.73 nm)" `shouldBe` Right (SpectralLine SrI Nothing 460.73)

    it "parses designations" $ do
      parseSpectralLine "Na I D2 (589.0 nm)" `shouldBe` Right (SpectralLine NaI (Just D2) 589.0)
      parseSpectralLine "Mg I b1 (517.27 nm)" `shouldBe` Right (SpectralLine (Ion "MgI") (Just B1) 517.27)
      parseSpectralLine "Fe XIII dx (1074.7 nm)" `shouldBe` Right (SpectralLine (Ion "FeXIII") (Just (Designation "DX")) 1074.7)

    it "parses short ions + designations" $ do
      parseSpectralLine "H alpha (656.28 nm)" `shouldBe` Right (SpectralLine H (Just Alpha) 656.28)
      parseSpectralLine "H beta (486.13 nm)" `shouldBe` Right (SpectralLine H (Just Beta) 486.13)

    it "renders simple spectral lines" $ do
      spectralLineName (SpectralLine FeI Nothing 630.15) `shouldBe` "Fe I (630.15 nm)"
      spectralLineName (SpectralLine SrI Nothing 460.73) `shouldBe` "Sr I (460.73 nm)"

    it "renders custom ion spectral lines" $ do
      spectralLineName (SpectralLine (Ion "FeXIII") (Just (Designation "DX")) 1074.7) `shouldBe` "Fe XIII DX (1074.70 nm)"

    it "renders spectral lines with designations" $ do
      spectralLineName (SpectralLine NaI (Just D2) 589.0) `shouldBe` "Na I D2 (589.00 nm)"
      spectralLineName (SpectralLine H (Just Alpha) 656.28) `shouldBe` "H alpha (656.28 nm)"
      spectralLineName (SpectralLine H (Just Beta) 486.13) `shouldBe` "H beta (486.13 nm)"
