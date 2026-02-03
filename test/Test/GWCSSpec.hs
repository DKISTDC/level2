module Test.GWCSSpec where

import Data.ByteString qualified as BS
import Effectful
import NSO.Image.GWCS.L1GWCS
import NSO.Prelude
import Skeletest
import Telescope.Asdf as Asdf
import Telescope.Asdf.Core (Quantity (..))
import Telescope.Asdf.GWCS as Asdf


spec :: Spec
spec =
  describe "Asdf GWCS" $ do
    it "parses L1 gwcs transform" $ do
      L1AsdfFixture asdf <- getFixture
      let L1Asdf (L1AsdfDataset (L1GWCS trans _)) = asdf

      trans.sharedInputs `shouldBe` 1
      trans.spatial.node.schema `shouldBe` "!transform/compose-1.2.0"
      trans.time.node.schema `shouldBe` "!transform/tabular-1.2.0"
      trans.stokes.node.schema `shouldBe` "!transform/tabular-1.2.0"

    it "parses L1 gwcs helioprojective frame" $ do
      L1AsdfFixture asdf <- getFixture
      let L1Asdf (L1AsdfDataset (L1GWCS _ helio)) = asdf
      helio.frame.observation.rsun.value `shouldBe` Number 695700.0


newtype L1AsdfFixture = L1AsdfFixture {l1Asdf :: L1Asdf}
instance Fixture L1AsdfFixture where
  fixtureAction = do
    inp <- BS.readFile "deps/VISP_L1_20231016T204852_BEEMM_metadata.asdf"
    asdf <- runEff $ Asdf.decodeM inp
    pure $ noCleanup $ L1AsdfFixture asdf
