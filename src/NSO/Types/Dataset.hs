{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module NSO.Types.Dataset where

import Data.Aeson
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength
import Rel8
import Text.Read (readEither)


data Dataset = Dataset
  { datasetId :: Id Dataset
  , scanDate :: UTCTime
  , latest :: Bool
  , observingProgramId :: Id ObservingProgram
  , instrument :: Instrument
  , instrumentProgramId :: Id InstrumentProgram
  , stokesParameters :: StokesParameters
  , createDate :: UTCTime
  , updateDate :: UTCTime
  , wavelengthMin :: Wavelength Nm
  , wavelengthMax :: Wavelength Nm
  , startTime :: UTCTime
  , endTime :: UTCTime
  , frameCount :: Int
  , primaryExperimentId :: Id Experiment
  , primaryProposalId :: Id Proposal
  , experimentDescription :: Text
  , exposureTime :: Float
  , boundingBox :: Maybe BoundingBox
  , health :: Health
  , gosStatus :: GOSStatus
  , aoLocked :: Int
  , lightLevel :: Distribution
  , polarimetricAccuracy :: Distribution
  , friedParameter :: Distribution
  , embargo :: Maybe UTCTime
  }
  deriving (Show, Eq)


data ObserveFrames


data ObservingProgram
data Proposal


data BoundingBox = BoundingBox
  { upperRight :: Coordinate Arcseconds
  , lowerLeft :: Coordinate Arcseconds
  }
  deriving (Eq)


-- | Gives all 4 points of the box
boundingPoints :: BoundingBox -> [Coordinate Arcseconds]
boundingPoints bb =
  let (xu, yu) = bb.upperRight
      (xl, yl) = bb.lowerLeft
      upperLeft = (xl, yu)
      lowerRight = (xu, yl)
   in [bb.upperRight, upperLeft, bb.lowerLeft, lowerRight]


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
  parseJSON = withText "BoundingBox (Upper Right, Lower Left)" $ \t -> do
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
  typeInformation = jsonTypeInfo


data GOSStatus = GOSStatus
  { open :: Maybe Int
  , opening :: Maybe Int
  , closed :: Maybe Int
  , closing :: Maybe Int
  , undefined :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType GOSStatus where
  typeInformation = jsonTypeInfo


data Distribution = Distribution
  { min :: Float
  , p25 :: Float
  , med :: Float
  , p75 :: Float
  , max :: Float
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Distribution where
  typeInformation = jsonTypeInfo
