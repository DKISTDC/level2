{-# LANGUAGE RecordWildCards #-}

module NSO.Data.Inversions.Update where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Rel8 hiding (Update)
import Effectful.Time
import NSO.Data.Inversions.Effect
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset)
import NSO.Types.Inversion


setSoftwareCommit :: (Inversions :> es) => Id Inversion -> GitCommit -> Eff es ()
setSoftwareCommit iid commit = do
  send $ Update iid $ \InversionRow{..} -> InversionRow{invSoftware = lit (Just commit), ..}


setGeneratedFits :: (Inversions :> es, Time :> es) => Id Inversion -> Eff es ()
setGeneratedFits iid = do
  now <- currentTime
  send $ Update iid $ \InversionRow{..} -> InversionRow{generateFits = lit (Just now), ..}


setGeneratedAsdf :: (Inversions :> es, Time :> es) => Id Inversion -> Eff es ()
setGeneratedAsdf iid = do
  now <- currentTime
  send $ Update iid $ \InversionRow{..} -> InversionRow{generateAsdf = lit (Just now), ..}


setGenTransferred :: (Inversions :> es, Time :> es) => Id Inversion -> Eff es ()
setGenTransferred iid = do
  now <- currentTime
  send $ Update iid $ \r -> r{generateTransfer = lit (Just now)}


setPublished :: (Inversions :> es, Time :> es) => Id Inversion -> Eff es ()
setPublished iid = do
  now <- currentTime
  send $ Update iid $ \InversionRow{..} ->
    InversionRow{published = lit (Just now), ..}


setError :: (Inversions :> es) => Id Inversion -> Text -> Eff es ()
setError iid err = do
  send $ Update iid $ \InversionRow{..} -> InversionRow{invError = lit (Just err), ..}


clearError :: (Inversions :> es) => Id Inversion -> Eff es ()
clearError iid = do
  send $ Update iid $ \InversionRow{..} -> InversionRow{invError = lit Nothing, ..}


resetGenerating :: (Inversions :> es) => Id Inversion -> Eff es ()
resetGenerating iid = do
  -- updateInversion iid $ \r -> r{generateTaskId = lit Nothing, generateL1FrameDir = lit Nothing, generateTaskCompleted = lit Nothing, invError = lit Nothing}
  send $ Update iid $ \InversionRow{..} -> InversionRow{invError = lit Nothing, generateFits = lit Nothing, generateAsdf = lit Nothing, published = lit Nothing, ..}


resetGeneratingAsdf :: (Inversions :> es) => Id Inversion -> Eff es ()
resetGeneratingAsdf iid = do
  send $ Update iid $ \InversionRow{..} ->
    InversionRow{invError = lit Nothing, generateAsdf = lit Nothing, published = lit Nothing, ..}


setDatasetUsed :: (Inversions :> es) => Inversion -> Id Dataset -> Bool -> Eff es ()
setDatasetUsed inv dsetId sel = do
  send $ Update inv.inversionId $ \InversionRow{..} ->
    InversionRow{datasets = lit $ setDataset sel dsetId inv.datasets, ..}
 where
  setDataset True = addDataset
  setDataset False = remDataset
  remDataset d =
    filter (/= d)
  addDataset d ds =
    if d `elem` ds
      then ds
      else d : ds


setNotes :: (Inversions :> es) => Id Inversion -> Text -> Eff es ()
setNotes invId txt = do
  send $ Update invId $ \InversionRow{..} -> InversionRow{notes = lit txt, ..}
