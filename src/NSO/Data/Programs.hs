module NSO.Data.Programs
  ( InstrumentProgram (..)
  , WithDatasets (..)
  , programStatus
  , programProvenance
  , instrumentProgram
  , toExperiment
  , toExperiments
  , fromDatasets
  , loadAllExperiments
  , loadAll
  ) where

import Data.Either (lefts, rights)
import Data.Grouped as G
import Data.List.NonEmpty qualified as NE
import Effectful.Rel8
import NSO.Data.Datasets
import NSO.Data.Provenance as Provenance
import NSO.Data.Qualify
import NSO.Data.Spectra qualified as Spectra
import NSO.Prelude
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength


-- import NSO.Data.Provenance hiding (Inverted, Queued)
-- import NSO.Data.Qualify

programStatus :: Grouped InstrumentProgram Dataset -> [ProvenanceEntry] -> ProgramStatus
programStatus gd ps = either id id $ do
  when (any wasInverted ps) $ Left Inverted
  when (any wasQueued ps) $ Left Queued
  when (isQualified gd) $ Left Qualified
  pure Invalid
 where
  wasQueued (WasQueued _) = True
  wasQueued _ = False

  wasInverted (WasInverted _) = True
  wasInverted _ = False


loadAll :: (Rel8 :> es) => Eff es [InstrumentProgram]
loadAll = do
  ds <- queryLatest
  pv <- loadAllProvenance
  pure $ fmap (.program) $ fromDatasets pv ds


loadAllExperiments :: (Rel8 :> es) => Eff es [Experiment]
loadAllExperiments = toExperiments <$> loadAll


fromDatasets :: AllProvenance -> [Dataset] -> [WithDatasets]
fromDatasets pv ds =
  let gds = grouped (.instrumentProgramId) ds :: [Grouped InstrumentProgram Dataset]
      pvs = fmap (programProvenance pv) gds :: [[ProvenanceEntry]]
      ips = zipWith instrumentProgram gds pvs
   in zipWith WithDatasets ips gds


programProvenance :: AllProvenance -> Grouped InstrumentProgram Dataset -> [ProvenanceEntry]
programProvenance (AllProvenance pv) gd =
  let d = sample gd
   in filter (Provenance.isProgram d.instrumentProgramId) pv


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


instrumentProgram :: Grouped InstrumentProgram Dataset -> [ProvenanceEntry] -> InstrumentProgram
instrumentProgram gd pv =
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
        , status = programStatus gd pv
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
