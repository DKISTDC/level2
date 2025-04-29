{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module NSO.Data.Scan where

import Data.Aeson as Aeson (Result (..), Value)
import Data.Bifunctor (bimap)
import Data.Either (lefts, rights)
import Data.Grouped
import Data.List qualified as L
import Data.Map qualified as M
import Data.String.Interpolate (i)
import Effectful.Dispatch.Dynamic
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Metadata
import NSO.Prelude
import NSO.Types.InstrumentProgram


-- Check Available -------------------------------------------------

runScanAvailable :: (Metadata :> es, Datasets :> es) => Eff es [Grouped (Id Proposal) DatasetAvailable]
runScanAvailable = do
  -- assume there aren't parse errors in these fields
  scanAvailable <$> send AvailableDatasets
 where
  scanAvailable :: [DatasetAvailable] -> [Grouped (Id Proposal) DatasetAvailable]
  scanAvailable = grouped (\d -> Id d.primaryProposalId)


-- Scan Proposal --------------------------------------------------------

data ScanProposal = ScanProposal
  { proposalId :: Id Proposal
  , errors :: [ScanError]
  , datasets :: [SyncDataset]
  }


runScanProposals :: (Metadata :> es, Datasets :> es, Time :> es) => [Grouped (Id Proposal) DatasetAvailable] -> Eff es [ScanProposal]
runScanProposals gds = do
  exs <- experimentDescriptions <$> send AllExperiments
  forM gds $ \gd -> do
    let propId = Id (sample gd).primaryProposalId
    runScanProposal' exs propId


runScanProposal :: (Metadata :> es, Datasets :> es, Time :> es) => Id Proposal -> Eff es ScanProposal
runScanProposal propId = do
  exs <- send AllExperiments
  runScanProposal' (experimentDescriptions exs) propId


runScanProposal' :: (Metadata :> es, Datasets :> es, Time :> es) => Map (Id Experiment) Text -> Id Proposal -> Eff es ScanProposal
runScanProposal' exs propId = do
  now <- currentTime
  pds <- send $ DatasetsByProposal propId
  ds <- indexed <$> Datasets.find (ByProposal propId)
  pure $ scanProposal $ scanResult now exs ds pds
 where
  scanProposal ScanResult{..} = ScanProposal{proposalId = propId, datasets, errors}


-- Scan Results / Calculate Sync ------------------------------------------------

data ScanResult = ScanResult
  { errors :: [ScanError]
  , datasets :: [SyncDataset]
  }


scanResult :: UTCTime -> Map (Id Experiment) Text -> Map (Id Dataset) Dataset -> [ParsedResult DatasetInventory] -> ScanResult
scanResult now exs m res =
  let ds = fmap (toDataset now exs) res
   in ScanResult
        { errors = lefts ds
        , datasets = fmap (syncDataset m) $ rights ds
        }


data SyncDataset = SyncDataset
  { dataset :: Dataset
  , sync :: Sync
  }


syncDataset :: Map (Id Dataset) Dataset -> Dataset -> SyncDataset
syncDataset m dataset =
  SyncDataset
    { dataset
    , sync = (sync m dataset)
    }


sync :: Map (Id Dataset) Dataset -> Dataset -> Sync
sync old d = fromMaybe New $ do
  dold <- M.lookup d.datasetId old
  if d == dold{scanDate = d.scanDate}
    then pure Skip
    else pure Update


-- Perform Sync -------------------------------------------------------------------------------------------------------------------

execSync :: [SyncDataset] -> Eff es ()
execSync = _


-- Sync Dataset --------------------------------------------------------------

data Sync
  = New
  | Skip
  | Update
  deriving (Eq, Show)


data ScanError = ScanError String Value
  deriving (Show, Eq)


toDataset :: UTCTime -> Map (Id Experiment) Text -> ParsedResult DatasetInventory -> Either ScanError Dataset
toDataset _ _ (ParsedResult val (Error err)) = Left $ ScanError err val
toDataset scanDate exs (ParsedResult val (Success d)) = do
  bimap (\err -> ScanError err val) id parseDataset
 where
  parseDataset = do
    ins <- parseInstrument d.instrumentName
    exd <- parseExperiment $ Id d.primaryExperimentId
    emb <- parseEmbargo
    pure $
      Dataset'
        { datasetId = Id d.datasetId
        , scanDate = scanDate
        , bucket = d.bucket
        , observingProgramId = Id d.observingProgramExecutionId
        , instrumentProgramId = Id d.instrumentProgramExecutionId
        , boundingBox = boundingBoxNaN d.boundingBox
        , instrument = ins
        , stokesParameters = d.stokesParameters
        , createDate = d.createDate.utc
        , updateDate = d.updateDate.utc
        , wavelengthMin = Wavelength d.wavelengthMin
        , wavelengthMax = Wavelength d.wavelengthMax
        , startTime = d.startTime.utc
        , endTime = d.endTime.utc
        , frameCount = fromIntegral d.frameCount
        , primaryExperimentId = Id d.primaryExperimentId
        , primaryProposalId = Id d.primaryProposalId
        , experimentDescription = exd
        , exposureTime = realToFrac d.exposureTime
        , health = d.health
        , gosStatus = d.gosStatus
        , aoLocked = fromIntegral d.aoLocked
        , friedParameter = d.friedParameter
        , polarimetricAccuracy = Distribution 0 0 0 0 0 -- d.polarimetricAccuracy
        , lightLevel = d.lightLevel
        , embargo = emb
        }

  parseExperiment :: Id Experiment -> Either String Text
  parseExperiment eid =
    case M.lookup eid exs of
      Nothing -> Left "Experiment Description"
      (Just e) -> pure e

  parseInstrument :: Text -> Either String Instrument
  parseInstrument input =
    maybe (Left [i|Invalid Instrument: #{input}|]) Right $ instrumentFromName $ cs input

  parseEmbargo :: Either String (Maybe UTCTime)
  parseEmbargo =
    if d.isEmbargoed
      then do
        dt <- required "Embargo End Date" d.embargoEndDate
        pure (Just dt.utc)
      else pure Nothing

  required :: String -> Maybe a -> Either String a
  required n Nothing = Left ("Missing Required: " <> n)
  required _ (Just v) = pure v

  -- parseRead :: (Read a) => Text -> Text -> Either String a
  -- parseRead expect input =
  --   maybe (Left [i|Invalid #{expect}: #{input}|]) Right $ instrumentFromName $ cs input

  boundingBoxNaN bb =
    if isCoordNaN bb.lowerLeft || isCoordNaN bb.upperRight
      then Nothing
      else Just bb


experimentDescriptions :: [ExperimentDescription] -> Map (Id Experiment) Text
experimentDescriptions eds = M.fromList $ fmap (\ed -> (Id ed.experimentId, ed.experimentDescription)) eds


indexed :: [Dataset] -> Map (Id Dataset) Dataset
indexed = M.fromList . map (\d -> (d.datasetId, d))

-- data ScanDataset = ScanDataset
--   { datasetId :: Id Dataset
--   , primaryProposalId :: Id Proposal
--   , updateDate :: UTCTime
--   , sync :: Sync
--   }
--
-- scanDataset :: Map (Id Dataset) Dataset -> DatasetAvailable -> ScanDataset
-- scanDataset m dsa =
--   case M.lookup (Id dsa.datasetId) m of
--     Nothing -> scanSync New
--     Just d ->
--       if dsa.updateDate.utc > d.updateDate
--         then scanSync Update
--         else scanSync Skip
--  where
--   scanSync s =
--     ScanDataset
--       { datasetId = Id dsa.datasetId
--       , primaryProposalId = Id dsa.primaryProposalId
--       , updateDate = dsa.updateDate.utc
--       , sync = s
--       }
--       p
--
-- syncDatasets :: (Datasets :> es, Metadata :> es, Time :> es) => Eff es SyncResults
-- syncDatasets = do
--   scan <- scanDatasetInventory
--   old <- indexed <$> Datasets.find All
--
--   let sync = syncResults old scan
--
--   -- Insert any new datasets
--   send $ Create sync.new
--
--   -- Update any old datasets
--   mapM_ (send . Save) sync.updated
--
--   -- Ignore any unchanged
--   pure sync
