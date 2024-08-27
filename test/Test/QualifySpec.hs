module Test.QualifySpec where

import NSO.Data.Qualify
import NSO.Data.Spectra (lineForWaves)
import NSO.Prelude
import NSO.Types.Dataset
import NSO.Types.Wavelength
import Skeletest


spec :: Spec
spec = do
  describe "Solar Diameter" $ do
    -- Apparent diameter from Earth
    -- At 1 A.U.(seconds of arc)  1919.
    -- Maximum (seconds of arc)   1952.
    -- Minimum (seconds of arc)   1887.
    it "should be at a maximum on Perihelion" $ do
      round (solarRadiusDay dayOfPerihelion) `shouldBe` (976 :: Int)

    it "should have a minimum on Aphelion" $ do
      round (solarRadiusDay dayOfAphelion) `shouldBe` (944 :: Int)

  describe "Spectra - identify lines" $ do
    it "should identify normal case 630.2" $ do
      -- Dataset: ADGGO
      lineForWaves 629.4 631.0 `shouldBe` Just FeI

    it "should catch slightly above 630.2" $ do
      -- Dataset: BVJVO
      lineForWaves 630.239 634.38 `shouldBe` Just FeI

    it "should catch slightly above 854.2 nm" $ do
      -- Dataset: BWKXP
      lineForWaves 854.201 856.8 `shouldBe` Just (CaII CaII_854)

    it "should not identify 589.1" $ do
      lineForWaves 589.1 590.1 `shouldBe` Nothing

  describe "Qualify" $ do
    describe "bounding box near edge" $ do
      let bb = BoundingBox (-57.56, -901.62) (-61.91, -963.33)
      it "should be on disk on Perihelion" $ do
        isOnDisk dayOfPerihelion bb `shouldBe` True

      it "should be off disk on Alphelion" $ do
        isOnDisk dayOfAphelion bb `shouldBe` False

    it "should catch off disk" $ do
      let bb = BoundingBox (59.93, -872.63) (-56.05, -991.35)
      isOnDisk dayOfPerihelion bb `shouldBe` False
