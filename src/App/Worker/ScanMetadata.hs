module App.Worker.ScanMetadata where

import Data.Aeson (Value)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import NSO.Data.Datasets as Datasets
import NSO.Data.Scan qualified as Scan
import NSO.Prelude
import NSO.Types.InstrumentProgram


-- it's a map?
data ScanState = ScanState
  { progress :: Map DatasetMetadata ScanResult
  , errors :: [Scan.ScanError]
  }


data DatasetMetadata = DatasetMetadata
  { datasetId :: Id Dataset
  , programId :: Id InstrumentProgram
  , proposalId :: Id Proposal
  }
  deriving (Show, Eq, Ord)


-- what if it is currently processing?
-- we do one at a time?
-- that'll be slow, but easy to understand...
--
-- maybe we update proposals one at a time?
--
-- hmmmm
--
-- The easiest to digest is:
--
-- 1. Get all the proposals
-- 2. Run a task per-proposal?
--
--
-- In a given proposal,
--
--
-- only scan for ones that are greater than last scan?
--
-- it's simpler if we scan EVERYTHING...
-- we can compare the createDate, updateDate
--
-- do a big scan, and compare only those
--
--
-- I'm sure I can trust the update date
--
--

data ScanResult
  = ScanUpdated Dataset -- the old dataset for comparison?
  | ScanNew
  | ScanUnchanged
