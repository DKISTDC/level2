module Test.FitsGenSpec where

import Data.Massiv.Array as M (Comp (..), Ix1, P, delay, fromLists')
import NSO.Fits.Generate.DimArray
import NSO.Fits.Generate.Profile
import NSO.Prelude
import NSO.Types.Wavelength
import Test.Syd


spec :: Spec
spec = do
  describe "Frames" $ do
    describe "differences" $ do
      describe "wavProfile" $ do
        describe "delta" $ do
          it "avgDelta should calc simple" $ do
            avgDelta simpleNums `shouldBe` 0.1

          it "should calc simple delta" $ do
            (wavProfile simple).delta `shouldBe` 0.1

          it "should calc regular delta" $ do
            (wavProfile wav630).delta `shouldBe` (0.00128 * 10)

        describe "pixel" $ do
          it "should be exactly center in simple" $ do
            pixel0 0.1 simpleNums `shouldBe` 3.5

          it "< positive index in wav630" $ do
            let px = (wavProfile wav630).pixel
            px `shouldSatisfy` (< 4)

          it "> last negative index in wav630" $ do
            let px = (wavProfile wav630).pixel
            px `shouldSatisfy` (> 3)


simple :: DimArray '[Wavelength 630]
simple = DimArray $ M.delay @Ix1 @P $ M.fromLists' Seq simpleNums


simpleNums :: [Float]
simpleNums = [-0.25, -0.15, -0.05, 0.05, 0.15, 0.25, 0.35, 0.45]


-- Actual data. Every 10th element (so the spacing is larger than normal)
wav630 :: DimArray '[Wavelength 630]
wav630 =
  DimArray $
    M.delay @Ix1 @P $
      M.fromLists'
        Seq
        [-0.02888, -0.01608, -0.00328, 0.00952, 0.02232, 0.03512, 0.04792, 0.06072, 0.07352, 0.08632, 0.09912, 0.11191999, 0.12471999, 0.13752, 0.15031999]
