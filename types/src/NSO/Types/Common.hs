{-# LANGUAGE DerivingVia #-}

module NSO.Types.Common where

import Control.Monad (replicateM)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List qualified as L
import Data.Text qualified as T
import Effectful.GenRandom (GenRandom, randomFromList)
import GHC.Real (Real)
import NSO.Prelude
import Rel8 (DBEq, DBType, ReadShow (..), TypeInformation, parseTypeInformation, typeInformation)
import System.FilePath qualified as FP
import Telescope.Asdf
import Telescope.Asdf.GWCS (ToAxes)
import Telescope.Fits as Telescope
import Text.Read (readMaybe)
import Web.HttpApiData (FromHttpApiData)
import Web.Hyperbole (FromParam (..), Route, ToParam (..))


newtype Id a = Id {fromId :: Text}
  deriving newtype (Show, Read, Eq, DBType, FromJSON, Route, DBEq, ToAsdf, ToKeyword, FromKeyword, ToParam, FromParam, ToJSON)
  deriving (Generic)


instance Ord (Id a) where
  (Id a) <= (Id b)
    | "pid" `T.isPrefixOf` a = proposalLT
    | otherwise = a <= b
   where
    proposalLT =
      proposalParts a <= proposalParts b

    proposalParts :: Text -> (Int, Int)
    proposalParts pid =
      let (cyclet, propt) = T.breakOn "_" $ T.drop 4 pid
       in (readN (cs cyclet), readN (cs $ T.drop 1 propt))

    readN = fromMaybe 0 . readMaybe


randomId :: (GenRandom :> es) => Text -> Eff es (Id a)
randomId pre = do
  let chars = ['A' .. 'Z'] ++ ['0' .. '9']
  sid <- replicateM 6 (randomFromList chars)
  pure $ Id (pre <> "." <> cs sid)


type Coordinate a = (a, a)


newtype Kilometers = Kilometers {value :: Double}
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)


newtype Arcseconds = Arcseconds Double
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)


newtype Radians = Radians Double
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)


radiansToArcseconds :: Radians -> Arcseconds
radiansToArcseconds (Radians r) = Arcseconds $ r * 206265


jsonTypeInfo :: (FromJSON a, ToJSON a) => TypeInformation a
jsonTypeInfo = parseTypeInformation (parseEither parseJSON) toJSON typeInformation


data Stokes = I | Q | U | V
  deriving (Show, Read, Eq, Ord, Generic, ToAxes)
  deriving (DBType) via ReadShow Stokes


newtype StokesParameters = StokesParameters [Stokes]
  deriving newtype (DBType, Eq, Ord, Monoid)


instance Show StokesParameters where
  show (StokesParameters ss) = mconcat $ fmap show ss


instance FromJSON StokesParameters where
  parseJSON = withText "Stokes Params" $ \t -> do
    sps <- mapM parseChar $ cs t
    pure $ StokesParameters sps
   where
    parseChar 'I' = pure I
    parseChar 'Q' = pure Q
    parseChar 'U' = pure U
    parseChar 'V' = pure V
    parseChar c = fail $ "Expected Stokes param (IQUV) but got: " <> [c]


instance Semigroup StokesParameters where
  (StokesParameters a) <> (StokesParameters b) = StokesParameters . L.nub $ a <> b


data Instrument
  = VBI
  | VISP
  | CRYO_NIRSP
  deriving (Show, Read, Ord, Eq, Generic, ToParam, FromParam)
  deriving (DBType) via ReadShow Instrument


instrumentFromName :: Text -> Maybe Instrument
instrumentFromName "CRYO-NIRSP" = pure CRYO_NIRSP
instrumentFromName t = readMaybe . cs $ t


newtype Path system pathType a = Path {filePath :: FilePath}
  deriving newtype (Show, Read, Eq, Ord, IsString, DBType, FromHttpApiData, ToJSON, FromJSON)
  deriving (Generic)


-- type Path = Path' File

-- we don't want to use the default "String = [Char]" instance
instance FromParam (Path s x a) where
  parseParam t = do
    Path . cs <$> parseParam @Text t


-- combine a directory with... something
(</>) :: Path s Dir a -> Path s x b -> Path s y c
Path dir </> Path x = Path $ FP.normalise $ (FP.</>) dir x
infixr 5 </>


isPathAbsolute :: Path s x a -> Bool
isPathAbsolute (Path fp) = take 1 fp == ['/']


filePath :: Path s Dir a -> Path s Filename b -> Path s File c
filePath (Path a) (Path b) = Path $ (FP.</>) a b


data PathType
  = Dir -- directory
  | File -- complete path
  | Filename -- only the filename


{- | UTCTime that can handle missing Zs at the end
   REMOVED: This is the same as a LocalTime with an implicit Data.Time.utc timezone
newtype DateTime = DateTime {utc :: UTCTime}
  deriving (Show, Eq, Generic)
  deriving newtype (ISO8601, FormatTime, ToKeyword)


instance FromJSON DateTime where
  parseJSON = withText "UTC" $ \s -> do
    iso8601ParseM $ cs s <> "Z"


instance FromKeyword DateTime where
  parseKeywordValue = \case
    Telescope.String t ->
      case iso8601ParseM $ cs $ addUTCZ t of
        Nothing -> expected "DateTime" t
        Just utc -> pure $ DateTime utc
    v -> expected "UTCTime" v


addUTCZ :: Text -> Text
addUTCZ t
  | T.isSuffixOf "Z" t = t
  | otherwise = t <> "Z"
-}
