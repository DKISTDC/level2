module NSO.Data.Scan where

import Data.String.Interpolate (i)
import Effectful
import Effectful.Error.Static
import Effectful.Rel8
import Effectful.Request
import Effectful.Time
import NSO.Data.Dataset
import NSO.Data.Types
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

scanDatasetInventory :: (GraphQL :> es, Error RequestError :> es, Time :> es) => Eff es [Dataset]
scanDatasetInventory = do
  now <- currentTime
  ads <- fetch @AllDatasets metadata ()
  let res = mapM (toDataset now) ads.datasetInventories
  either (throwError . ParseError) pure res

syncDatasets :: (GraphQL :> es, Error RequestError :> es, Rel8 :> es, Time :> es) => Eff es [Dataset]
syncDatasets = do
  ds <- scanDatasetInventory
  _ <- query () $ insertAll ds
  pure ds

toDataset :: UTCTime -> DatasetInventory -> Either String Dataset
toDataset scanDate d = do
  ins <- parseRead "Instrument" d.instrumentName
  pure
    $ Dataset
      { datasetId = Id d.datasetId
      , scanDate = scanDate
      , observingProgramExecutionId = Id d.observingProgramExecutionId
      , instrumentProgramExecutionId = Id d.instrumentProgramExecutionId
      , boundingBox = boundingBoxNaN d.boundingBox
      , instrument = ins
      , stokesParameters = d.stokesParameters
      , createDate = d.createDate.utc
      , wavelengthMin = Wavelength d.wavelengthMin
      , wavelengthMax = Wavelength d.wavelengthMax
      , startTime = d.startTime.utc
      , endTime = d.endTime.utc
      , frameCount = fromIntegral d.frameCount
      , primaryExperimentId = Id d.primaryExperimentId
      , primaryProposalId = Id d.primaryProposalId
      , experimentDescription = d.experimentDescription
      , exposureTime = realToFrac d.exposureTime
      , inputDatasetObserveFramesPartId = Id . cs $ show d.inputDatasetObserveFramesPartId
      }
 where
  parseRead :: (Read a) => Text -> Text -> Either String a
  parseRead expect input =
    maybe (Left [i|Invalid #{expect}: #{input}|]) Right $ readMaybe $ cs input

  boundingBoxNaN bb =
    if (isCoordNaN bb.lowerLeft || isCoordNaN bb.upperRight)
      then Nothing
      else Just bb
