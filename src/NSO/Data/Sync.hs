{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module NSO.Data.Sync where

import Data.Aeson as Aeson (Result (..), Value)
import Data.Bifunctor (first)
import Data.Either (lefts, rights)
import Data.Grouped
import Data.List qualified as L
import Data.Map qualified as M
import Data.String.Interpolate (i)
import Data.Time (localTimeToUTC, utc)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Time
import NSO.Data.Datasets (Datasets)
import NSO.Data.Datasets qualified as Datasets
import NSO.Metadata
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength


type SyncId = UTCTime


data SyncState = SyncState
  { started :: UTCTime
  , -- , error :: Maybe String
    proposals :: Maybe [Id Proposal]
  , scans :: Map (Id Proposal) ScanProposal
  }


data MetadataSync :: Effect where
  Create :: MetadataSync m SyncId
  History :: MetadataSync m [SyncId]
  Get :: SyncId -> MetadataSync m SyncState
  SetProposals :: SyncId -> [Id Proposal] -> MetadataSync m ()
  SetScan :: SyncId -> Id Proposal -> ScanProposal -> MetadataSync m ()


type instance DispatchOf MetadataSync = Dynamic


type History = TVar [SyncState]


runMetadataSync :: (Concurrent :> es, IOE :> es, Time :> es) => History -> Eff (MetadataSync : es) a -> Eff es a
runMetadataSync var = interpret $ \_ -> \case
  Create -> do
    now <- currentTime
    atomically $ do
      modifyTVar var (empty now :)
    pure now
  History -> do
    sts <- readTVarIO var
    pure $ fmap (.started) sts
  Get t -> do
    sts <- readTVarIO var
    pure $ fromMaybe (empty t) $ L.find (\st -> st.started == t) sts
  SetProposals s propIds -> do
    modifySync s $ \st -> st{proposals = Just propIds}
  SetScan s propId scan ->
    modifySync s $ \st -> st{scans = M.insert propId scan st.scans}
 where
  -- SetError s e ->
  -- modifySync s $ \st -> st{error = Just e}

  empty t = SyncState t Nothing M.empty

  modifySync :: (Concurrent :> es) => SyncId -> (SyncState -> SyncState) -> Eff es ()
  modifySync t f = do
    atomically $ do
      modifyTVar var $ fmap (modifyId t f)

  modifyId :: SyncId -> (SyncState -> SyncState) -> SyncState -> SyncState
  modifyId time f st =
    if st.started == time
      then f st
      else st


lastSync :: (MetadataSync :> es) => Eff es (Maybe UTCTime)
lastSync = do
  listToMaybe <$> send History


initMetadataSync :: (Concurrent :> es) => Eff es (TVar [SyncState])
initMetadataSync = do
  newTVarIO []


-- Check Available -------------------------------------------------

runScanAvailable :: (Metadata es) => Eff es [Group (Id Proposal) DatasetAvailable]
runScanAvailable = do
  -- assume there aren't parse errors in these fields
  scanAvailable <$> send AvailableDatasets
 where
  scanAvailable :: [DatasetAvailable] -> [Group (Id Proposal) DatasetAvailable]
  scanAvailable = grouped (\d -> Id d.primaryProposalId)


-- Scan Proposal --------------------------------------------------------

data ScanProposal = ScanProposal
  { proposalId :: Id Proposal
  , errors :: [ScanError]
  , datasets :: [SyncDataset]
  }
  deriving (Show)


runScanProposals :: (Metadata es, Datasets :> es, Time :> es, Log :> es) => [Group (Id Proposal) DatasetAvailable] -> Eff es [ScanProposal]
runScanProposals gds = do
  exs <- experimentDescriptions <$> send AllExperiments
  forM gds $ \gd -> do
    let propId = Id (sample gd).primaryProposalId
    runScanProposal' exs propId


runScanProposal :: (Metadata es, Datasets :> es, Time :> es, Log :> es) => Id Proposal -> Eff es ScanProposal
runScanProposal propId = do
  exs <- send AllExperiments
  runScanProposal' (experimentDescriptions exs) propId


runScanProposal' :: (Metadata es, Datasets :> es, Time :> es, Log :> es) => Map (Id Experiment) Text -> Id Proposal -> Eff es ScanProposal
runScanProposal' exs propId = do
  now <- currentTime
  pds <- send $ DatasetsByProposal propId
  ds <- indexed <$> Datasets.find (Datasets.ByProposal propId)
  log Debug $ dump " Datasets:" $ length ds
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
  deriving (Show)


syncDataset :: Map (Id Dataset) Dataset -> Dataset -> SyncDataset
syncDataset m dataset =
  SyncDataset
    { dataset
    , sync = sync m dataset
    }


sync :: Map (Id Dataset) Dataset -> Dataset -> Sync
sync old d = fromMaybe New $ do
  dold <- M.lookup d.datasetId old
  if d == dold{scanDate = d.scanDate}
    then pure Skip
    else pure Update


-- Perform Sync -------------------------------------------------------------------------------------------------------------------

execSync :: (Log :> es, Datasets :> es) => [SyncDataset] -> Eff es ()
execSync sds = do
  -- replace all the datasets!
  let new = fmap (.dataset) $ filter (\s -> s.sync == New) sds
  log Debug $ " new " <> show (length new)
  send $ Datasets.Create new

  let ups = fmap (.dataset) $ filter (\s -> s.sync == Update) sds
  log Debug $ " update " <> show (length ups)
  mapM_ (send . Datasets.Save) ups


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
  first (`ScanError` val) parseDataset
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
        , createDate = localTimeToUTC utc d.createDate
        , updateDate = localTimeToUTC utc d.updateDate
        , wavelengthMin = Wavelength d.wavelengthMin
        , wavelengthMax = Wavelength d.wavelengthMax
        , startTime = localTimeToUTC utc d.startTime
        , endTime = localTimeToUTC utc d.endTime
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
        , embargo = localTimeToUTC utc <$> emb
        , spectralLines = fromMaybe [] d.spectralLines
        }

  parseExperiment :: Id Experiment -> Either String Text
  parseExperiment eid =
    case M.lookup eid exs of
      Nothing -> Left "Experiment Description"
      (Just e) -> pure e

  parseInstrument :: Text -> Either String Instrument
  parseInstrument input =
    maybe (Left [i|Invalid Instrument: #{input}|]) Right $ instrumentFromName $ cs input

  parseEmbargo :: Either String (Maybe LocalTime)
  parseEmbargo =
    if d.isEmbargoed
      then do
        dt <- required "Embargo End Date" d.embargoEndDate
        pure (Just dt)
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
