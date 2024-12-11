module Test.QuantitySpec where

import Data.Massiv.Array as M
import NSO.Image.Quantity (forcePositive360)
import NSO.Prelude
import Skeletest


spec :: Spec
spec =
  describe "Quantities" $ do
    describe "convertData" $ do
      it "should move negative angles to positive" $ do
        let arr = M.fromLists' @P @Ix1 @Float Seq [-100, -200, 0, 30, 300, 400]
        let arr2 = M.map forcePositive360 arr
        M.toLists (computeAs P arr2) `shouldBe` [260, 160, 0, 30, 300, 40]
