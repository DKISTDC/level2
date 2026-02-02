module Test.GWCSSpec where

import Data.ByteString qualified as BS
import Effectful
import NSO.Image.GWCS.L1GWCS (L1Asdf (..), L1AsdfDataset (..), L1GWCS (..), L1WCSTransform (..))
import NSO.Prelude
import Skeletest
import Telescope.Asdf as Asdf


spec :: Spec
spec =
  describe "Asdf GWCS" $ do
    it "parses the sample L1 gwcs transform" $ do
      L1AsdfFixture asdf <- getFixture
      let L1Asdf (L1AsdfDataset (L1GWCS (L1WCSTransform node) _)) = asdf
      show node.schema `shouldBe` "asdf://dkist.nso.edu/tags/varying_celestial_transform-1.1.0"


newtype L1AsdfFixture = L1AsdfFixture {l1Asdf :: L1Asdf}
instance Fixture L1AsdfFixture where
  fixtureAction = do
    inp <- BS.readFile "deps/VISP_L1_20231016T204852_BEEMM_metadata.asdf"
    asdf <- runEff $ Asdf.decodeM inp
    pure $ noCleanup $ L1AsdfFixture asdf
