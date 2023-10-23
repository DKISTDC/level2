{-# LANGUAGE DerivingVia #-}

module NSO.Data.Types where

import Data.Aeson (FromJSON (..), withText)
import Data.List qualified as L
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Real (Real)
import NSO.Prelude
import Rel8
import Text.Read (readEither)
import Web.Hyperbole (Param (..), Route)

newtype Id a = Id {fromId :: Text}
  deriving newtype (Show, Read, Eq, Ord, DBType, FromJSON, Route, DBEq)
  deriving (Generic)

instance Param (Id a) where
  toParam (Id t) = t
  parseParam t = pure $ Id t

data Stokes = I | Q | U | V
  deriving (Show, Read, Eq)
  deriving (DBType) via ReadShow Stokes

data ObserveFrames

newtype StokesParameters = StokesParameters [Stokes]
  deriving newtype (DBType, Eq, Monoid)

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

showDate :: UTCTime -> Text
showDate = cs . formatTime defaultTimeLocale "%F"

showTimestamp :: UTCTime -> Text
showTimestamp = cs . formatTime defaultTimeLocale "%F %T"

newtype Wavelength a = Wavelength Double
  deriving newtype (Num, Eq, Ord, Show, DBType, Floating, Fractional, RealFloat, RealFrac, Real)

data Nm

newtype Arcseconds = Arcseconds Float
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)

type Coordinate a = (a, a)

data BoundingBox = BoundingBox
  { upperRight :: Coordinate Arcseconds
  , lowerLeft :: Coordinate Arcseconds
  }
  deriving (Eq)

instance Show BoundingBox where
  show (BoundingBox ur ll) = show (ur, ll)

-- | Serialize as a tuple, just like from the API
instance DBType BoundingBox where
  typeInformation :: TypeInformation BoundingBox
  typeInformation =
    parseTypeInformation
      parseBoundingBox
      serialize
      typeInformation
   where
    serialize :: BoundingBox -> Text
    serialize bb = cs $ show (bb.upperRight, bb.lowerLeft)

instance FromJSON BoundingBox where
  parseJSON = withText "BoundingBox (Upper Left, Lower Right)" $ \t -> do
    -- the Metadata format is ALMOST a tuple ((x1,y1),(x2,y2)
    -- "boundingBox": "(-351.85,-375.79),(-468.82,-495.48)",
    -- so wrap it in parens
    either (const $ fail $ "no parse: " <> cs t) pure $ parseBoundingBox $ "(" <> t <> ")"

parseBoundingBox :: Text -> Either String BoundingBox
parseBoundingBox t = do
  (ur, ll) <- readEither (cs t) :: Either String (Coordinate Arcseconds, Coordinate Arcseconds)
  pure $ BoundingBox{upperRight = ur, lowerLeft = ll}

isCoordNaN :: Coordinate Arcseconds -> Bool
isCoordNaN (a, b) = isNaN a || isNaN b
