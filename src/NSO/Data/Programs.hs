module NSO.Data.Programs
  ( InstrumentProgram (..)
  , WithDatasets (..)
  , programStatus
  , programInversions
  , instrumentProgram
  , toExperiment
  , toExperiments
  , fromDatasets
  , loadAllExperiments
  , loadAll
  ) where

import App.Error
import Data.Either (lefts, rights)
import Data.Grouped as G
import Data.List.NonEmpty qualified as NE
import Effectful.Error.Static
import Effectful.Rel8
import NSO.Data.Datasets
import NSO.Data.Inversions
import NSO.Data.Qualify
import NSO.Data.Spectra qualified as Spectra
import NSO.Prelude
import NSO.Types.InstrumentProgram
import NSO.Types.Status
import NSO.Types.Wavelength


-- import NSO.Data.Provenance hiding (Inverted, Queued)
-- import NSO.Data.Qualify

programStatus :: Grouped InstrumentProgram Dataset -> [Inversion] -> ProgramStatus
programStatus gd [] =
  if isQualified gd
    then StatusQualified
    else StatusInvalid
programStatus _ (i : _) = do
  -- TODO: find latest inversion
  StatusInversion i.step


loadAll :: (Rel8 :> es, Error AppError :> es) => Eff es [InstrumentProgram]
loadAll = do
  ds <- queryLatest
  ai <- queryAll
  pure $ fmap (.program) $ fromDatasets ai ds


loadAllExperiments :: (Rel8 :> es, Error AppError :> es) => Eff es [Experiment]
loadAllExperiments = toExperiments <$> loadAll


fromDatasets :: AllInversions -> [Dataset] -> [WithDatasets]
fromDatasets ai ds =
  let gds = grouped (.instrumentProgramId) ds :: [Grouped InstrumentProgram Dataset]
      pvs = fmap (programInversions ai) gds :: [[Inversion]]
      ips = zipWith instrumentProgram gds pvs
   in zipWith WithDatasets ips gds


-- | All inversions for the given program
programInversions :: AllInversions -> Grouped InstrumentProgram Dataset -> [Inversion]
programInversions (AllInversions ivs) gd =
  let d = sample gd
   in filter (\i -> i.programId == d.instrumentProgramId) ivs


toExperiments :: [InstrumentProgram] -> [Experiment]
toExperiments ips =
  map toExperiment $ grouped (.experimentId) ips


toExperiment :: Grouped Experiment InstrumentProgram -> Experiment
toExperiment g =
  let ip = sample g
   in Experiment
        { experimentId = ip.experimentId
        , description = ip.experimentDescription
        , startTime = ip.startTime
        , programs = Grouped g.items
        }


-- No, it *might* have inversions, it might not
instrumentProgram :: Grouped InstrumentProgram Dataset -> [Inversion] -> InstrumentProgram
instrumentProgram gd ivs =
  let d = sample gd
      ls = NE.toList $ fmap identifyLine gd.items
   in InstrumentProgram
        { programId = d.instrumentProgramId
        , experimentId = d.primaryExperimentId
        , experimentDescription = d.experimentDescription
        , createDate = d.createDate
        , stokesParameters = d.stokesParameters
        , startTime = d.startTime
        , instrument = d.instrument
        , onDisk = qualifyOnDisk gd
        , spectralLines = rights ls
        , otherWavelengths = lefts ls
        , status = programStatus gd ivs
        , embargo = d.embargo
        }
 where
  midWave :: Dataset -> Wavelength Nm
  midWave d = (d.wavelengthMin + d.wavelengthMax) / 2

  identifyLine :: Dataset -> Either (Wavelength Nm) SpectralLine
  identifyLine d = maybe (Left $ midWave d) Right $ Spectra.identifyLine d.wavelengthMin d.wavelengthMax


data WithDatasets = WithDatasets
  { program :: InstrumentProgram
  , datasets :: Grouped InstrumentProgram Dataset
  }
