{-# LANGUAGE StrictData #-}

module NSO.Types.Inversion where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import Rel8 (Column, DBType, Rel8able, Result)
import Web.Hyperbole (FromParam, ToParam)


data InvQuantities
data InvProfileFit
data InvProfileOrig


data Inversion = Inversion
  { inversionId :: Id Inversion
  , programId :: Id InstrumentProgram
  , proposalId :: Id Proposal
  , created :: UTCTime
  , updated :: UTCTime
  , datasets :: [Id Dataset]
  , invert :: Invert
  , generate :: Generate
  , published :: Maybe UTCTime
  , invError :: Maybe Text
  , notes :: Text
  }
  deriving (Show, Eq)


-- The database definition is flattened. Needs validation on return from DB!
data InversionRow f = InversionRow
  { inversionId :: Column f (Id Inversion)
  , programId :: Column f (Id InstrumentProgram)
  , proposalId :: Column f (Id Proposal)
  , created :: Column f UTCTime
  , updated :: Column f UTCTime
  , datasets :: Column f [Id Dataset] -- datasets used for the inversion
  , uploadedProfileFit :: Column f (Maybe UTCTime)
  , uploadedProfileOrig :: Column f (Maybe UTCTime)
  , uploadedQuantities :: Column f (Maybe UTCTime)
  , invSoftware :: Column f (Maybe GitCommit)
  , generateFits :: Column f (Maybe UTCTime)
  , generateAsdf :: Column f (Maybe UTCTime)
  , generateTransfer :: Column f (Maybe UTCTime)
  , published :: Column f (Maybe UTCTime)
  , invError :: Column f (Maybe Text)
  , notes :: Column f Text
  }
  deriving (Generic, Rel8able)


deriving stock instance (f ~ Result) => Show (InversionRow f)
deriving stock instance (f ~ Result) => Eq (InversionRow f)


newtype GitCommit = GitCommit Text
  deriving newtype (Show, Read, Eq, ToJSON, FromJSON, DBType, Ord, FromParam, ToParam)
  deriving (Generic)


data Invert = Invert
  { datasets :: [Id Dataset]
  , commit :: Maybe GitCommit
  , profileFit :: Maybe UTCTime
  , profileOrig :: Maybe UTCTime
  , quantities :: Maybe UTCTime
  }
  deriving (Show, Eq)


data Generate = Generate
  { fits :: Maybe UTCTime
  , asdf :: Maybe UTCTime
  , transfer :: Maybe UTCTime
  }
  deriving (Show, Eq)
