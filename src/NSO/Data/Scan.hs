module NSO.Data.Scan where

import Data.List qualified as L
import Data.Map qualified as M
import Data.String.Interpolate (i)
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Request
import Effectful.Time
import NSO.Data.Datasets
import NSO.DataStore.Datasets
import NSO.Metadata
import NSO.Metadata.Types
import NSO.Prelude
import Text.Read (readMaybe)


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
  }
  deriving (Eq)


scanDatasetInventory :: (GraphQL :> es, Error RequestError :> es, Time :> es) => Service -> Eff es [Dataset]
scanDatasetInventory metadata = do
  now <- currentTime
  ads <- fetch @AllDatasets metadata ()
  exs <- fetch @AllExperiments metadata ()
  let res = mapM (toDataset now exs) ads.datasetInventories
  either (throwError . ParseError) pure res


syncDatasets :: (Datasets :> es, GraphQL :> es, Error RequestError :> es, Time :> es) => Service -> Eff es SyncResults
syncDatasets metadata = do
  scan <- scanDatasetInventory metadata
  old <- indexed <$> send (Query Latest)

  let res = syncResults old scan

  -- Insert any new datasets
  send $ Create res.new

  -- Update any old datasets
  send $ Modify SetOld $ map (.datasetId) res.updated
  send $ Create res.updated

  -- Ignore any unchanged
  pure res


syncResults :: Map (Id Dataset) Dataset -> [Dataset] -> SyncResults
syncResults old scan =
  let srs = map (syncResult old) scan
      res = zip srs scan
      new = results New res
      updated = results Updated res
      unchanged = results Unchanged res
   in SyncResults{new, updated, unchanged}
 where
  results r = map snd . filter ((== r) . fst)


syncResult :: Map (Id Dataset) Dataset -> Dataset -> SyncResult
syncResult old d = fromMaybe New $ do
  dold <- M.lookup d.datasetId old
  if d == (dold :: Dataset){scanDate = d.scanDate}
    then pure Unchanged
    else pure Updated


toDataset :: UTCTime -> AllExperiments -> DatasetInventory -> Either String Dataset
toDataset scanDate (AllExperiments exs) d = do
  ins <- parseRead "Instrument" d.instrumentName
  exd <- parseExperiment d.primaryExperimentId
  emb <- parseEmbargo
  pure $
    Dataset
      { datasetId = Id d.datasetId
      , scanDate = scanDate
      , latest = True
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
 where
  parseExperiment :: Text -> Either String Text
  parseExperiment eid =
    case L.find (\e -> e.experimentId == eid) exs of
      Nothing -> fail "Experiment Description"
      (Just e) -> pure e.experimentDescription

  parseEmbargo :: Either String (Maybe UTCTime)
  parseEmbargo =
    if d.isEmbargoed
      then do
        utc <- required "Embargo End Date" $ (.utc) <$> d.embargoEndDate
        pure (Just utc)
      else pure Nothing

  required :: String -> Maybe a -> Either String a
  required n Nothing = fail ("Missing Required: " <> n)
  required _ (Just v) = pure v

  parseRead :: (Read a) => Text -> Text -> Either String a
  parseRead expect input =
    maybe (Left [i|Invalid #{expect}: #{input}|]) Right $ readMaybe $ cs input

  boundingBoxNaN bb =
    if isCoordNaN bb.lowerLeft || isCoordNaN bb.upperRight
      then Nothing
      else Just bb


indexed :: [Dataset] -> Map (Id Dataset) Dataset
indexed = M.fromList . map (\d -> (d.datasetId, d))
