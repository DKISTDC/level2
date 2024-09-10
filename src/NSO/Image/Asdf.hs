module NSO.Image.Asdf where

import Data.Binary.Put (putByteString, putFloatbe, putInt32be, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import NSO.Image.Frame (L2Frame)
import NSO.Image.Headers.Keywords (HeaderKeywords (..))
import NSO.Image.Primary
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset)
import NSO.Types.Inversion (Inversion)
import Telescope.Asdf
import Telescope.Asdf.NDArray (ByteOrder (..), DataType (..), putUcs4)
import Telescope.Data.Axes (axesRowMajor)
import Telescope.Fits qualified as Fits
import Telescope.Fits.Types as Fits (KeywordRecord (..), LogicalConstant (..))


data InversionTree = InversionTree
  { created :: UTCTime
  , fileuris :: Fileuris
  , meta :: InversionTreeMeta
  }
  deriving (Generic, ToAsdf)


data InversionTreeMeta = InversionTreeMeta
  { headers :: HeaderTable PrimaryHeader
  , inventory :: InversionInventory
  }
  deriving (Generic, ToAsdf)


data InversionInventory = InversionInventory
  { inversionId :: Id Inversion
  , datasetIds :: [Id Dataset]
  , frameCount :: Int
  }
  deriving (Generic, ToAsdf)


newtype HeaderTable a = HeaderTable (NonEmpty a)


instance (HeaderKeywords a) => ToAsdf (HeaderTable a) where
  toValue (HeaderTable as) =
    Object
      [ ("colnames", colnames as)
      , ("columns", toNode $ Array $ fmap toNode $ keywordColumns $ fmap headerKeywords as)
      , ("qtable", toNode False)
      ]
   where
    -- keywordValue (KeywordRecord k v _) = _

    colname (KeywordRecord k _ _) = fromValue $ String k

    colnames vals =
      let krs = headerKeywords (head vals)
       in fromValue $ Array $ fmap colname krs


data KeywordColumn = KeywordColumn
  { keyword :: Key
  , values :: NonEmpty Fits.Value
  }
  deriving (Show)


instance ToAsdf KeywordColumn where
  schema = "core/column-1.0.0"
  toValue col = NDArray $ toNDArray col


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
      Fits.Float f -> putFloatbe f
      Fits.Logic l ->
        putByteString $
          BS.singleton $
            case l of
              T -> 0x1
              F -> 0x0


keywordColumns :: NonEmpty [KeywordRecord] -> [KeywordColumn]
keywordColumns =
  mapMaybe keyColumn . keyGroups
 where
  keyGroups :: NonEmpty [KeywordRecord] -> [[KeywordRecord]]
  keyGroups = groupBy isSameKey . sortOn (._keyword) . mconcat . NE.toList
  isSameKey kr1 kr2 = kr1._keyword == kr2._keyword
  keyColumn [] = Nothing
  keyColumn (k : ks) = do
    pure $ KeywordColumn k._keyword $ fmap (._value) (k :| ks)


newtype Fileuris = Fileuris [Path' Filename L2Frame]


instance ToAsdf Fileuris where
  toValue (Fileuris ps) =
    Array $ fmap pathNode ps
   where
    pathNode (Path fp) = fromValue $ String $ cs fp
