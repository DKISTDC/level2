module NSO.Image.Asdf.HeaderTable where

import Data.Binary.Put (putByteString, putDoublebe, putInt32be, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import NSO.Image.Types.Frame (Frames (..))
import NSO.Prelude
import Telescope.Asdf
import Telescope.Asdf.NDArray (ByteOrder (..), DataType (..), putUcs4)
import Telescope.Data.Axes (axesRowMajor)
import Telescope.Fits as Fits (Header, KeywordRecord (..), LogicalConstant (..), ToHeader (..), keywords)
import Telescope.Fits qualified as Fits


-- Data ---------------------------------------

newtype HeaderTable a = HeaderTable (Frames a)


instance (ToHeader a) => ToAsdf (HeaderTable a) where
  schema _ = "tag:astropy.org:astropy/table/table-1.0.0"
  toValue (HeaderTable as) =
    Object
      [ ("colnames", colnames as.frames)
      , ("columns", toNode $ Array $ fmap toNode $ keywordColumns $ fmap toHeader as.frames)
      , ("qtable", toNode False)
      ]
   where
    colname (KeywordRecord k _ _) = k

    colnames vals =
      let krs = toHeader (head vals)
       in fromValue $ Array $ fmap (toNode . String) $ L.sort $ fmap colname $ Fits.keywords krs


data KeywordColumn = KeywordColumn
  { keyword :: Key
  , values :: NonEmpty Fits.Value
  }
  deriving (Show)


instance ToAsdf KeywordColumn where
  schema _ = "!core/column-1.0.0"
  toValue col =
    Object
      [ ("name", toNode $ String col.keyword)
      , ("data", toNode $ NDArray $ toNDArray col)
      ]


instance ToNDArray KeywordColumn where
  toNDArray col =
    NDArrayData
      { byteorder = BigEndian
      , datatype = datatype (head col.values)
      , shape = axesRowMajor [length col.values]
      , bytes = BL.toStrict $ runPut $ mapM_ putValue col.values
      }
   where
    datatype = \case
      Fits.String _ -> Ucs4 maxStringLength
      Fits.Integer _ -> Int32
      Fits.Float _ -> Float32
      Fits.Logic _ -> Bool8

    maxStringLength = maximum $ fmap stringLength col.values

    stringLength = \case
      Fits.String t -> T.length t
      _ -> 0

    putValue = \case
      Fits.String t -> putUcs4 maxStringLength t
      Fits.Integer n -> putInt32be $ fromIntegral n
      Fits.Float d -> putDoublebe d
      Fits.Logic l ->
        putByteString $
          BS.singleton $
            case l of
              T -> 0x1
              F -> 0x0


keywordColumns :: NonEmpty Header -> [KeywordColumn]
keywordColumns =
  mapMaybe keyColumn . keyGroups
 where
  keyGroups :: NonEmpty Header -> [[KeywordRecord]]
  keyGroups = groupBy isSameKey . sortOn (.keyword) . mconcat . NE.toList . fmap Fits.keywords
  isSameKey kr1 kr2 = kr1.keyword == kr2.keyword
  keyColumn [] = Nothing
  keyColumn (k : ks) = do
    pure $ KeywordColumn k.keyword $ fmap (.value) (k :| ks)
