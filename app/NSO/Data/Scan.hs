module NSO.Data.Scan where

import Effectful
import Effectful.Error.Static
import Effectful.Request
import Effectful.Time
import NSO.Data.Dataset
import NSO.Data.Types
import NSO.Metadata
import NSO.Metadata.Types
import NSO.Prelude

-- scanDatasets :: (Time :> es, GraphQL :> es, Rel8 :> es, Error RequestError :> es) => Eff es [Dataset]
-- scanDatasets = do
--   now <- currentTime
--   ds <- fetchDatasets now
--   _ <- query () $ insertAll ds
--   pure ds

scanDatasetInventory :: (GraphQL :> es, Error RequestError :> es, Time :> es) => Eff es [Dataset]
scanDatasetInventory = do
  now <- currentTime
  ads <- fetch @AllDatasets metadata ()
  pure $ fmap (toDataset now) ads.datasetInventories

toDataset :: UTCTime -> DatasetInventory -> Dataset
toDataset scanDate d =
  Dataset
    { datasetId = Id d.datasetId
    , scanDate = scanDate
    , observingProgramExecutionId = Id d.observingProgramExecutionId
    , instrumentProgramExecutionId = Id d.instrumentProgramExecutionId
    , instrumentName = d.instrumentName
    , stokesParameters = d.stokesParameters
    , createDate = d.createDate.utc
    , wavelengthMin = d.wavelengthMin
    , wavelengthMax = d.wavelengthMax
    , startTime = d.startTime.utc
    , endTime = d.endTime.utc
    , frameCount = fromIntegral d.frameCount
    , primaryExperimentId = Id d.primaryExperimentId
    , primaryProposalId = Id d.primaryProposalId
    , experimentDescription = d.experimentDescription
    , exposureTime = realToFrac d.exposureTime
    , inputDatasetObserveFramesPartId = Id . cs $ show d.inputDatasetObserveFramesPartId
    }
