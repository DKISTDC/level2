module App.Worker.PuppetMaster where

import App.Worker.GenWorker
import App.Worker.SyncMetadata
import Data.Maybe (isJust, isNothing)
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
     , Tasks GenFits :> es
     , Tasks GenAsdf :> es
     , Tasks SyncMetadataTask :> es
     )
  => Eff es ()
manageMinions = do
  -- log Debug "GO FORTH MY MINIONS"

  -- 5 second delay
  threadDelay (5 * 1000 * 1000)

  AllInversions ivs <- send Inversions.All
  let gfs = generateFits ivs
  send $ TasksAdd gfs

  let gas = generateAsdf ivs
  send $ TasksAdd gas

  checkMetadataSync


checkMetadataSync :: (Log :> es, MetadataSync :> es, Time :> es, Tasks SyncMetadataTask :> es) => Eff es ()
checkMetadataSync = do
  b <- needsMetadataSync
  when b $ do
    log Debug "QUEUE - Metadata Sync"
    s <- send Sync.Create
    send $ TaskAdd $ SyncMetadataTask s


generateFits :: [Inversion] -> [GenFits]
generateFits ivs = do
  map genTask $ filter isFitsGen ivs
 where
  genTask inv = GenFits inv.proposalId inv.inversionId

  isFitsGen inv =
    isInverted inv
      && not (isError inv)
      && not inv.deleted
      && isNothing inv.generate.fits


generateAsdf :: [Inversion] -> [GenAsdf]
generateAsdf ivs = do
  map genTask $ filter isAsdfGen ivs
 where
  genTask inv = GenAsdf inv.proposalId inv.inversionId

  isAsdfGen inv =
    isInverted inv
      && not (isError inv)
      && not inv.deleted
      && isJust inv.generate.fits
      && isNothing inv.generate.asdf
