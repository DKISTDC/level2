{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.Sync where

import App.Colors
import App.Effect.Auth
import App.Route as Route
import App.Style qualified as Style
import App.View.Common
import App.View.Layout
import Data.List qualified as L
import Data.Map.Strict qualified as M
import NSO.Data.Datasets as Datasets
import NSO.Data.Sync as Sync
import NSO.Prelude
import NSO.Types.Common
import Web.Atomic.CSS
import Web.Hyperbole


page :: (Hyperbole :> es, Datasets :> es, MetadataSync :> es, Auth :> es) => SyncId -> Eff es (Page '[SyncDetails])
page syncId = do
  sc <- send $ Sync.Get syncId
  appLayout (Route.Datasets Route.DatasetRoot) $ do
    col ~ Style.page $ do
      col ~ Style.card . pad 15 $ do
        hyper (SyncDetails syncId) $ viewSyncDetails sc


-- Scan --------------------------------------------------

data SyncDetails = SyncDetails SyncId
  deriving (Generic, ViewId)


instance (MetadataSync :> es) => HyperView SyncDetails es where
  data Action SyncDetails
    = Refresh
    deriving (Generic, ViewAction)


  update Refresh = do
    SyncDetails syncId <- viewId
    sc <- send $ Sync.Get syncId
    pure $ viewSyncDetails sc


viewSyncDetails :: SyncState -> View SyncDetails ()
viewSyncDetails s = do
  col @ onLoad Refresh 1000 ~ gap 10 $ do
    el ~ bold . fontSize 24 $ text $ "Metadata Sync: " <> cs (showDate s.started)
    -- el (color Danger) $ text $ cs $ show s.error
    -- code id $ cs $ show s.proposals
    case s.scans of
      [] -> el ~ italic $ "Empty"
      ss ->
        forM_ (L.reverse $ M.elems ss) $ \sc -> do
          viewSyncProposal sc


viewSyncProposal :: ScanProposal -> View SyncDetails ()
viewSyncProposal scan = do
  let skips = filter (\d -> d.sync == Skip) scan.datasets
  let news = filter (\d -> d.sync == New) scan.datasets
  let ups = filter (\d -> d.sync == Update) scan.datasets

  col ~ gap 5 $ do
    row ~ gap 10 $ do
      appRoute (Route.Proposal scan.proposalId Route.PropRoot) ~ Style.link . bold $ text $ cs scan.proposalId.fromId
      el ~ italic $ text $ "new: " <> cs (show $ length news) <> ","
      el ~ italic $ text $ "updated: " <> cs (show $ length ups) <> ","
      el ~ italic $ text $ "skipped: " <> cs (show $ length skips)

    forM_ scan.errors $ \e -> do
      el ~ color Danger . pad (XY 5 0) $ text $ cs $ show e

    mapM_ viewSyncDataset (news <> ups)


viewSyncDataset :: SyncDataset -> View SyncDetails ()
viewSyncDataset s = do
  row ~ gap 10 . pad (XY 5 0) $ do
    appRoute (Route.Datasets $ Route.Dataset s.dataset.datasetId) ~ Style.link $ text s.dataset.datasetId.fromId
    case s.sync of
      New -> el "New"
      Update -> el "Update"
      Skip -> el "Skip"
