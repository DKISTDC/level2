module NSO.Data.Scan where

import Effectful
import Effectful.Error.Static
import Effectful.Request
import NSO.Metadata
import NSO.Metadata.Types
import NSO.Prelude

-- import Effectful.Rel8
-- import Effectful.Time
-- import NSO.Data.Dataset
-- import Rel8

-- scanDatasets :: (Time :> es, GraphQL :> es, Rel8 :> es, Error RequestError :> es) => Eff es [Dataset]
-- scanDatasets = do
--   now <- currentTime
--   ds <- fetchDatasets now
--   _ <- query () $ insertAll ds
--   pure ds

fetchDatasets :: (GraphQL :> es, Error RequestError :> es) => Eff es [DatasetInventory]
fetchDatasets = do
  ads <- fetch @AllDatasets metadata ()
  pure ads.datasetInventories

-- either (throwError . ParseError) pure $ parseAllDatasets scanDate ads

-- just sav eall of them!
-- importDataset :: ( Rel8 :> es ) => Dataset Result -> Eff es ()
-- importDataset ds =
--   insert $ Insert
--     { into =
--
