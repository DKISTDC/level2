module NSO.Data.Scan where

import Data.Aeson as Aeson (Result (..), Value)
import Data.Bifunctor (bimap)
import Data.List qualified as L
import Data.Map qualified as M
import Data.String.Interpolate (i)
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Error
import NSO.Metadata
import NSO.Prelude


-- scanDatasets :: (Time :> es, GraphQL :> es, Rel8 :> es, Error RequestError :> es) => Eff es [Dataset]
-- scanDatasets = do
--   now <- currentTime
--   ds <- fetchDatasets now
--   _ <- query () $ insertAll ds
--   pure ds

data SyncResult
  = New
  | Unchanged
  | Updated
  deriving (Eq)


data SyncResults = SyncResults
  { new :: [Dataset]
  , updated :: [Dataset]
  , unchanged :: [Dataset]
  , errors :: [ScanError]
  }


data ScanResult = ScanResult
  { datasets :: [Dataset]
  , errors :: [ScanError]
  }


data ScanError = ScanError String Value


scanDatasetInventory :: (Metadata :> es, Time :> es, Error DataError :> es) => Eff es ScanResult
scanDatasetInventory = do
  now <- currentTime
  ads <- send AllDatasets
  exs <- send AllExperiments
  let parsed = fmap (toDataset now exs) ads
  let datasets = mapMaybe success parsed
  let errors = mapMaybe scanError parsed
  pure $ ScanResult{datasets, errors}
 where
  success (Left _) = Nothing
  success (Right d) = Just d
  scanError (Left err) = Just err
  scanError (Right _) = Nothing


syncDatasets :: (Datasets :> es, Metadata :> es, Time :> es, Error DataError :> es) => Eff es SyncResults
syncDatasets = do
  scan <- scanDatasetInventory
  old <- indexed <$> Datasets.find All

  let sync = syncResults old scan

  -- Insert any new datasets
  send $ Create sync.new

  -- Update any old datasets
  mapM_ (send . Save) sync.updated

  -- Ignore any unchanged
  pure sync


syncResults :: Map (Id Dataset) Dataset -> ScanResult -> SyncResults
syncResults old scan =
  let srs = map (syncResult old) scan.datasets
      res = zip srs scan.datasets
      new = results New res
      updated = results Updated res
      unchanged = results Unchanged res
   in SyncResults{new, updated, unchanged, errors = scan.errors}
 where
  results r = map snd . filter ((== r) . fst)


syncResult :: Map (Id Dataset) Dataset -> Dataset -> SyncResult
syncResult old d = fromMaybe New $ do
  dold <- M.lookup d.datasetId old
  if d == dold{scanDate = d.scanDate}
    then pure Unchanged
    else pure Updated


toDataset :: UTCTime -> [ExperimentDescription] -> ParsedDataset -> Either ScanError Dataset
toDataset _ _ (ParsedDataset val (Error err)) = Left $ ScanError err val
toDataset scanDate exs (ParsedDataset val (Success d)) = do
  bimap (\err -> ScanError err val) id parseDataset
 where
  parseDataset = do
    ins <- parseInstrument d.instrumentName
    exd <- parseExperiment d.primaryExperimentId
    emb <- parseEmbargo
    pure $
      Dataset'
        { datasetId = Id d.datasetId
        , scanDate = scanDate
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

  parseExperiment :: Text -> Either String Text
  parseExperiment eid =
    case L.find (\e -> e.experimentId == eid) exs of
      Nothing -> Left "Experiment Description"
      (Just e) -> pure e.experimentDescription

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


indexed :: [Dataset] -> Map (Id Dataset) Dataset
indexed = M.fromList . map (\d -> (d.datasetId, d))
