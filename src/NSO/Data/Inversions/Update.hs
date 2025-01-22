{-# LANGUAGE RecordWildCards #-}

module NSO.Data.Inversions.Update where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Rel8 hiding (Update)
import Effectful.Time
import NSO.Data.Inversions.Effect
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.Inversion
import Network.Globus.Transfer (Task)


setDownloading :: (Inversions :> es) => Id Inversion -> Id Task -> Eff es ()
setDownloading iid tid = do
  send $ Update iid $ \r -> r{downloadTaskId = lit (Just tid)}


setDownloaded :: (Inversions :> es, Time :> es) => Id Inversion -> [Id Dataset] -> Eff es ()
setDownloaded iid ds = do
  now <- currentTime
  send $ Update iid $ \r -> r{downloaded = lit (Just now), downloadDatasets = lit ds}


setUploading :: (Inversions :> es) => Id Inversion -> Id Task -> Eff es ()
setUploading iid tid = do
  send $ Update iid $ \r -> r{uploadTaskId = lit (Just tid)}


setUploaded :: (Inversions :> es, Time :> es) => Id Inversion -> Eff es ()
setUploaded iid = do
  now <- currentTime
  send $ Update iid $ \r -> r{uploaded = lit (Just now)}


setInverted :: (Inversions :> es, Time :> es) => Id Inversion -> GitCommit -> Eff es ()
setInverted iid soft = do
  now <- currentTime
  send $ Update iid $ \r -> r{inverted = lit (Just now), inversionSoftware = lit (Just soft)}


setGeneratedFits :: (Inversions :> es, Time :> es) => Id Inversion -> Eff es ()
setGeneratedFits iid = do
  now <- currentTime
  send $ Update iid $ \InversionRow{..} -> InversionRow{generatedFits = lit (Just now), ..}


setGeneratedAsdf :: (Inversions :> es, Time :> es) => Id Inversion -> Eff es ()
setGeneratedAsdf iid = do
  now <- currentTime
  send $ Update iid $ \InversionRow{..} -> InversionRow{generatedAsdf = lit (Just now), ..}


setGenTransferring :: (Log :> es, Inversions :> es) => Id Inversion -> Id Task -> Eff es ()
setGenTransferring iid tid = do
  send $ Update iid $ \r -> r{generateTaskId = lit (Just tid)}


setGenTransferred :: (Inversions :> es, Time :> es) => Id Inversion -> Eff es ()
setGenTransferred iid = do
  now <- currentTime
  send $ Update iid $ \r -> r{generateTaskCompleted = lit (Just now)}


setPublishing :: (Inversions :> es, Time :> es) => Id Inversion -> Id Task -> Eff es ()
setPublishing iid task = do
  send $ Update iid $ \r -> r{publishTaskId = lit (Just task)}


setPublished :: (Inversions :> es, Time :> es) => Id Inversion -> Eff es ()
setPublished iid = do
  now <- currentTime
  send $ Update iid $ \r -> r{published = lit (Just now)}


setError :: (Inversions :> es) => Id Inversion -> Text -> Eff es ()
setError iid err = do
  send $ Update iid $ \InversionRow{..} -> InversionRow{invError = lit (Just err), ..}


clearError :: (Inversions :> es) => Id Inversion -> Eff es ()
clearError iid = do
  send $ Update iid $ \InversionRow{..} -> InversionRow{invError = lit Nothing, ..}


resetGenerating :: (Inversions :> es) => Id Inversion -> Eff es ()
resetGenerating iid = do
  -- updateInversion iid $ \r -> r{generateTaskId = lit Nothing, generateL1FrameDir = lit Nothing, generateTaskCompleted = lit Nothing, invError = lit Nothing}
  send $ Update iid $ \InversionRow{..} -> InversionRow{invError = lit Nothing, generatedFits = lit Nothing, generatedAsdf = lit Nothing, published = lit Nothing, ..}


resetGeneratingAsdf :: (Inversions :> es) => Id Inversion -> Eff es ()
resetGeneratingAsdf iid = do
  send $ Update iid $ \InversionRow{..} ->
    InversionRow{invError = lit Nothing, generatedAsdf = lit Nothing, published = lit Nothing, ..}
