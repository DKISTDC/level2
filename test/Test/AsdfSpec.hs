module Test.AsdfSpec where

import Control.Monad.Catch (throwM)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Error.Static
import GHC.Int (Int32)
import NSO.Image.Asdf.HeaderTable
import NSO.Image.Types.Frame
import NSO.Prelude
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Asdf
import Telescope.Asdf.NDArray (DataType (..), getUcs4, putUcs4)
import Telescope.Data.Axes
import Telescope.Data.Binary (ByteOrder (..))
import Telescope.Data.Parser (ParseError, expected, runParser, runParserPure)
import Telescope.Fits (KeywordRecord (..), ToHeader (..))
import Telescope.Fits qualified as Fits
import Telescope.Fits.Header (Header (..), HeaderRecord (..))


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
      let test = runGet (getUcs4 BigEndian 6) out :: Text
      test `shouldBe` ("asdf" :: Text)

  describe "KeywordColumns" $ do
    it "should create one column" $ do
      let key1 v = [Keyword $ KeywordRecord "key1" (Fits.String v) Nothing]
      let headers = NE.fromList $ fmap Header [key1 "one", key1 "two", key1 "three"]
      let cols = keywordColumns headers
      length cols `shouldBe` 1
      [c1] <- pure cols
      c1.values `shouldBe` fmap Fits.String (NE.fromList ["one", "two", "three"])

    -- is it an error if they don't match sizes? I think it is!
    it "should create columns with out of order keys" $ do
      let key1 v = Keyword $ KeywordRecord "key1" (Fits.String v) Nothing
      let key2 v = Keyword $ KeywordRecord "key2" (Fits.String v) Nothing
      let headers = NE.fromList [Header [key1 "one1", key2 "one2"], Header [key2 "two2", key1 "two1"], Header [key1 "three1", key2 "three2"]]
      let cols = keywordColumns headers
      length cols `shouldBe` 2
      [c1, c2] <- pure cols
      c1.values `shouldBe` fmap Fits.String (NE.fromList ["one1", "two1", "three1"])
      c2.values `shouldBe` fmap Fits.String (NE.fromList ["one2", "two2", "three2"])

    it "should encode an integer column" $ do
      let kc = KeywordColumn "ints" $ NE.fromList [Fits.Integer 11, Fits.Integer 22]
      let nda = toNDArray kc
      nda.shape `shouldBe` Axes [2]
      nda.datatype `shouldBe` Int32
      runParserPure (fromNDArray nda) `shouldBe` Right ([11, 22] :: [Int32])

    it "should encode an text column" $ do
      let kc = KeywordColumn "strings" $ NE.fromList [Fits.String "asdf", Fits.String "wahoo!"]
      let nda = toNDArray kc
      nda.shape `shouldBe` Axes [2]
      nda.datatype `shouldBe` Ucs4 6
      runParserPure (fromNDArray nda) `shouldBe` Right (["asdf", "wahoo!"] :: [Text])

  describe "table node" $ do
    it "should encode to Asdf" $ do
      let table = HeaderTable $ Frames $ NE.fromList [Sample 1, Sample 2, Sample 3]
      case toValue table of
        Object vals -> do
          lookup "colnames" vals `shouldSatisfy` P.just (P.con Node{value = P.eq (Array [fromValue $ String "user", fromValue $ String "value"])})

          Just (Node _ _ (Array cols)) <- pure $ lookup "columns" vals
          length cols `shouldBe` 2
          [c1, c2] <- pure cols

          n1 <- parseIO $ columnData c1
          n1.shape `shouldBe` Axes [3]
          n1.datatype `shouldBe` Ucs4 3

          n2 <- parseIO $ columnData c2
          n2.shape `shouldBe` Axes [3]
        _ -> fail "Expected Object"
 where
  columnData = \case
    Node _ _ (Object o) -> do
      case lookup "data" o of
        Just (Node _ _ (NDArray dat)) -> pure dat
        other -> expected "NDArray" other
    node -> expected "Column" node


parseIO :: Eff '[Parser, Error ParseError, IOE] a -> IO a
parseIO p = runEff $ runErrorNoCallStackWith @ParseError throwM $ runParser p


data Sample = Sample
  { value :: Int
  }


instance ToHeader Sample where
  toHeader s =
    Header
      [ Keyword $ KeywordRecord "user" (Fits.String "bob") Nothing
      , Keyword $ KeywordRecord "value" (Fits.Integer s.value) Nothing
      ]
