{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.Sync where

import App.Colors
import App.Effect.Auth
import App.Route qualified as Route
import App.Style qualified as Style
import App.View.Common
import App.View.Layout
import Data.List qualified as L
import Data.Map.Strict qualified as M
import NSO.Data.Datasets as Datasets
import NSO.Data.Sync as Sync
import NSO.Prelude
import Web.Hyperbole


page :: (Hyperbole :> es, Datasets :> es, MetadataSync :> es, Auth :> es) => SyncId -> Eff es (Page '[SyncDetails])
page syncId = do
  sc <- send $ Sync.Get syncId
  appLayout (Route.Datasets Route.DatasetRoot) $ do
    col (Style.page) $ do
      col (Style.card . pad 15) $ do
        hyper (SyncDetails syncId) $ viewSyncDetails sc


-- Scan --------------------------------------------------

data SyncDetails = SyncDetails SyncId
  deriving (Show, Read, ViewId)


instance (MetadataSync :> es) => HyperView SyncDetails es where
  data Action SyncDetails
    = Refresh
    deriving (Show, Read, ViewAction)


  update Refresh = do
    SyncDetails syncId <- viewId
    sc <- send $ Sync.Get syncId
    pure $ viewSyncDetails sc


viewSyncDetails :: SyncState -> View SyncDetails ()
viewSyncDetails s = do
  col (gap 10 . onLoad Refresh 1000) $ do
    el (bold . fontSize 24) $ text $ "Metadata Sync: " <> cs (showDate s.started)
    -- el (color Danger) $ text $ cs $ show s.error
    -- code id $ cs $ show s.proposals
    case s.scans of
      [] -> el italic "Empty"
      ss ->
        forM_ (L.reverse $ M.elems ss) $ \sc -> do
          viewSyncProposal sc


viewSyncProposal :: ScanProposal -> View SyncDetails ()
viewSyncProposal scan = do
  let skips = filter (\d -> d.sync == Skip) scan.datasets
  let other = filter (\d -> d.sync /= Skip) scan.datasets

  col (gap 5) $ do
    row (gap 10) $ do
      route (Route.Proposal scan.proposalId Route.PropRoot) (Style.link . bold) $ text $ cs scan.proposalId.fromId
      el italic $ text $ (cs $ show $ length skips) <> " skipped"

    forM_ scan.errors $ \e -> do
      el (color Danger . pad (XY 5 0)) (text $ cs $ show e)

    mapM_ viewSyncDataset other


viewSyncDataset :: SyncDataset -> View SyncDetails ()
viewSyncDataset s = do
  row (gap 10 . pad (XY 5 0)) $ do
    route (Route.Datasets $ Route.Dataset s.dataset.datasetId) Style.link $ text $ s.dataset.datasetId.fromId
    case s.sync of
      New -> el_ "New"
      Update -> el_ "Update"
      Skip -> el_ "Skip"
