module Test.FrameSpec where

import Data.Massiv.Array as M
import NSO.Image.DataCube
import NSO.Image.Headers.Types
import NSO.Image.Quantity (splitFrames)
import NSO.Prelude
import Skeletest


data Q


spec :: Spec
spec = do
  describe "Frames" $ do
    describe "splitFrames" $ do
      it "should split frames on 3rd dimension" $ do
        let frames = splitFrames sample4D
        length frames `shouldBe` 3
        [f0, _, _] <- pure frames

        M.toLists (computeAs P f0.array) `shouldBe` ([[[0, 1, 2, 3], [1, 2, 3, 4]]] :: [[[Float]]])


sample4D :: DataCube [Q, Depth, FrameY, SlitX]
sample4D = DataCube $ M.makeArray @D @Ix4 Seq (Sz (1 :> 2 :> 3 :. 4)) sumIndex
 where
  sumIndex (a :> b :> fy :. d) = fromIntegral a + fromIntegral b + fromIntegral fy + fromIntegral d
