module Test.QualifySpec where

import NSO.Prelude
import Test.Syd


spec :: Spec
spec = do
  describe "basic test sss suite" $ do
    it "should fail" $ do
      2 + 2 `shouldBe` (4 :: Int)
