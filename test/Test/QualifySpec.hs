module Test.QualifySpec where

import NSO.Data.Qualify
import NSO.Prelude
import NSO.Types.Dataset
import Test.Syd


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

  describe "Qualify" $ do
    it "should be on disk" $ do
      let bb = BoundingBox (-817.11, 255.01) (-870.28, 179.45)
      isOnDisk dayOfPerihelion bb `shouldBe` True

    it "should catch off disk" $ do
      let bb = BoundingBox (59.93, -872.63) (-56.05, -991.35)
      isOnDisk dayOfPerihelion bb `shouldBe` False
