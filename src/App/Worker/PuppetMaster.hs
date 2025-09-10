module App.Worker.PuppetMaster where

import App.Worker.Generate
import App.Worker.SyncMetadata
import Data.Maybe (isNothing)
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Sync as Sync
import NSO.Prelude


-- The Puppeteer checks the status of systems and starts jobs as necessary
manageMinions
  :: ( Concurrent :> es
     , Inversions :> es
     , Time :> es
     , Datasets :> es
     , MetadataSync :> es
     , Log :> es
     , Tasks GenTask :> es
     , Tasks SyncMetadataTask :> es
     )
  => Eff es ()
manageMinions = do
  -- 5 second delay
  threadDelay (5 * 1000 * 1000)

  AllInversions ivs <- send Inversions.All
  let gfs = generateFits ivs
  send $ TasksAdd gfs

  checkMetadataSync


checkMetadataSync :: (Log :> es, MetadataSync :> es, Time :> es, Tasks SyncMetadataTask :> es) => Eff es ()
checkMetadataSync = do
  b <- needsMetadataSync
  when b $ do
    log Debug "Metadata Sync"
    s <- send Sync.Create
    send $ TaskAdd $ SyncMetadataTask s


generateFits :: [Inversion] -> [GenTask]
generateFits ivs = do
  map genTask $ filter isNeedsGen ivs
 where
  genTask inv = GenTask inv.proposalId inv.inversionId

  isNeedsGen inv =
    isInverted inv
      && not (isError inv)
      && not inv.deleted
      && (isNothing inv.generate.fits || isNothing inv.generate.asdf)
