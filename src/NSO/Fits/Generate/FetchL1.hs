module NSO.Fits.Generate.FetchL1 where

import App.Globus as Globus
import Data.List qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets
import NSO.Data.Spectra (identifyLine)
import NSO.Prelude
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength


-- DONE: identify which dataset to use
-- TODO: trigger globus transfer to local storage

findCanonicalDataset :: (Datasets :> es) => Id InstrumentProgram -> Eff es (Maybe Dataset)
findCanonicalDataset ip = do
  ds <- send $ Query (ByProgram ip)
  pure $ L.find isCanonicalDataset ds


isCanonicalDataset :: Dataset -> Bool
isCanonicalDataset d =
  identifyLine d == Just FeI

-- TODO: maybe we want to return the folder as well? Some easy way to look it up, abstractly
-- also we need to know when it finishes...
-- transferCanonicalDataset :: (Globus :> es) => Dataset -> Eff es (Id Task, FilePath)
-- transferCanonicalDataset d = do
--   (t, f) <- Globus.initTransferDataset _ d
--   pure (t, f)
