{-# LANGUAGE DerivingVia #-}

module NSO.Types.Dataset where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List qualified as L
import GHC.Real (Real)
import NSO.Prelude
import Rel8
import Text.Read (readEither)
import Web.Hyperbole (Param)

data ObserveFrames

data Stokes = I | Q | U | V
  deriving (Show, Read, Eq, Ord)
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

data ObservingProgram
data Proposal

data Instrument
  = VBI
  | VISP
  deriving (Show, Ord, Eq, Read, Param)
  deriving (DBType) via ReadShow Instrument

type Coordinate a = (a, a)

newtype Arcseconds = Arcseconds Float
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)

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

data Health = Health
  { good :: Maybe Int
  , bad :: Maybe Int
  , ill :: Maybe Int
  , unknown :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance DBType Health where
  typeInformation = parseTypeInformation (parseEither parseJSON) toJSON typeInformation

data GOSStatus = GOSStatus
  { open :: Maybe Int
  , opening :: Maybe Int
  , closed :: Maybe Int
  , closing :: Maybe Int
  , undefined :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance DBType GOSStatus where
  typeInformation = parseTypeInformation (parseEither parseJSON) toJSON typeInformation

data Distribution = Distribution
  { min :: Float
  , p25 :: Float
  , med :: Float
  , p75 :: Float
  , max :: Float
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance DBType Distribution where
  typeInformation = parseTypeInformation (parseEither parseJSON) toJSON typeInformation
