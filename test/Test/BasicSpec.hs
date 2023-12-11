module Test.BasicSpec where

import Test.Syd
import Prelude


spec :: Spec
spec = do
  describe "basic test suite" $ do
    it "should run a test" $ do
      2 + 2 `shouldBe` (4 :: Int)
