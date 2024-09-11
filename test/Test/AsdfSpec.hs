module Test.AsdfSpec where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty qualified as NE
import GHC.Int (Int32)
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Headers.Keywords
import NSO.Prelude
import Skeletest
import Telescope.Asdf
import Telescope.Asdf.NDArray (DataType (..), getUcs4, putUcs4)
import Telescope.Data.Axes
import Telescope.Fits qualified as Fits
import Telescope.Fits.Types (KeywordRecord (..))


-- import NSO.Image.Asdf

spec :: Spec
spec =
  describe "asdf" $ do
    specHeaderTable


specHeaderTable :: Spec
specHeaderTable = describe "Header Table" $ do
  describe "UCS4 Encoding" $ do
    it "should encode 4 bytes per character" $ do
      let out = runPut $ putUcs4 4 "asdf"
      BL.length out `shouldBe` (4 * 4)

    it "should justify shorter" $ do
      let out = runPut $ putUcs4 5 "asdf"
      BL.length out `shouldBe` (5 * 4)

    it "should roundtrip" $ do
      let out = runPut $ putUcs4 6 "asdf"
      let test = runGet (getUcs4 6) out :: Text
      test `shouldBe` ("asdf" :: Text)

  describe "KeywordColumns" $ do
    it "should create one column" $ do
      let key1 v = [KeywordRecord "key1" (Fits.String v) Nothing]
      let headers = NE.fromList [key1 "one", key1 "two", key1 "three"]
      let cols = keywordColumns headers
      length cols `shouldBe` 1
      [c1] <- pure cols
      c1.values `shouldBe` fmap Fits.String (NE.fromList ["one", "two", "three"])

    -- is it an error if they don't match sizes? I think it is!
    it "should create columns with out of order keys" $ do
      let key1 v = KeywordRecord "key1" (Fits.String v) Nothing
      let key2 v = KeywordRecord "key2" (Fits.String v) Nothing
      let headers = NE.fromList [[key1 "one1", key2 "one2"], [key2 "two2", key1 "two1"], [key1 "three1", key2 "three2"]]
      let cols = keywordColumns headers
      length cols `shouldBe` 2
      [c1, c2] <- pure cols
      c1.values `shouldBe` fmap Fits.String (NE.fromList ["one1", "two1", "three1"])
      c2.values `shouldBe` fmap Fits.String (NE.fromList ["one2", "two2", "three2"])

    it "should encode an integer column" $ do
      let kc = KeywordColumn "ints" $ NE.fromList [Fits.Integer 11, Fits.Integer 22]
      -- let kc = KeywordColumn "ints" $ NE.fromList [Fits.String "asdf", Fits.String "wahoo!"]
      let nda = toNDArray kc
      nda.shape `shouldBe` Axes [2]
      nda.datatype `shouldBe` Int32
      runParser (fromNDArray nda) `shouldBe` Right ([11, 22] :: [Int32])

    it "should encode an text column" $ do
      let kc = KeywordColumn "strings" $ NE.fromList [Fits.String "asdf", Fits.String "wahoo!"]
      let nda = toNDArray kc
      nda.shape `shouldBe` Axes [2]
      nda.datatype `shouldBe` Ucs4 6
      runParser (fromNDArray nda) `shouldBe` Right (["asdf", "wahoo!"] :: [Text])

  describe "table node" $ do
    it "should encode to Asdf" $ do
      let table = HeaderTable $ NE.fromList [Sample 1, Sample 2, Sample 3]
      case toValue table of
        Object vals -> do
          lookup "colnames" vals `shouldBe` Just (Node mempty $ Array [fromValue $ String "name", fromValue $ String "test"])

          Just (Node _ (Array cols)) <- pure $ lookup "columns" vals
          length cols `shouldBe` 2
          [Node _ (NDArray n1), Node _ (NDArray n2)] <- pure cols
          n1.shape `shouldBe` Axes [3]
          n1.datatype `shouldBe` Ucs4 3

          n2.shape `shouldBe` Axes [3]
        _ -> fail "Expected Object"


data Sample = Sample
  { test :: Int
  }


instance HeaderKeywords Sample where
  headerKeywords s =
    [ KeywordRecord "name" (Fits.String "bob") Nothing
    , KeywordRecord "test" (Fits.Integer s.test) Nothing
    ]
