{-# LANGUAGE StrictData #-}

module NSO.Data.Programs
  ( InstrumentProgram (..)
  , WithDatasets (..)
  , programStatus
  , programInversions
  , instrumentProgram
  , toProposal
  , toProposals
  , fromDatasets
  , loadAllProposals
  , loadAll
  ) where

import Data.Either (lefts, rights)
import Data.Grouped as G
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Qualify
import NSO.Data.Spectra qualified as Spectra
import NSO.Prelude
import NSO.Types.InstrumentProgram


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


loadAll :: (Datasets :> es, Inversions :> es) => Eff es [InstrumentProgram]
loadAll = do
  ds <- send $ Datasets.Query Latest
  ai <- send Inversions.All
  pure $ fmap (.program) $ fromDatasets ai ds


loadAllProposals :: (Datasets :> es, Inversions :> es) => Eff es [Proposal]
loadAllProposals = toProposals <$> loadAll


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


toProposals :: [InstrumentProgram] -> [Proposal]
toProposals ips =
  map toProposal $ grouped (.proposalId) ips


toProposal :: Grouped Proposal InstrumentProgram -> Proposal
toProposal g =
  let ip = sample g
   in Proposal
        { proposalId = ip.proposalId
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
        , proposalId = d.primaryProposalId
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
