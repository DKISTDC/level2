{-# LANGUAGE StrictData #-}

module NSO.Data.Programs
  ( InstrumentProgram (..)
  , ProgramFamily (..)
  , ProgramStatus (..)
  , InstrumentProgramStatus (..)
  , programStatus
  , programInversions
  , instrumentProgram
  , instrumentProgramStatus
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
import NSO.Types.Status


loadAll :: (Datasets :> es, Inversions :> es) => Eff es [InstrumentProgramStatus]
loadAll = do
  ds <- send $ Datasets.Query Latest
  ai <- send Inversions.All
  pure $ fmap (.program) $ fromDatasets ai ds


loadAllProposals :: (Datasets :> es, Inversions :> es) => Eff es [ProposalPrograms]
loadAllProposals = toProposals <$> loadAll


fromDatasets :: AllInversions -> [Dataset] -> [ProgramFamily]
fromDatasets ai ds =
  let gds = grouped (.instrumentProgramId) ds :: [Grouped InstrumentProgram Dataset]
      pvs = fmap (programInversions ai) gds :: [[Inversion]]
      ips = zipWith instrumentProgramStatus gds pvs
   in zipWith3 ProgramFamily ips gds pvs


-- | All inversions for the given program
programInversions :: AllInversions -> Grouped InstrumentProgram Dataset -> [Inversion]
programInversions (AllInversions ivs) gd =
  let d = sample gd
   in filter (\i -> i.programId == d.instrumentProgramId) ivs


toProposals :: [InstrumentProgramStatus] -> [ProposalPrograms]
toProposals ips =
  map toProposal $ grouped (\ip -> ip.program.proposalId) ips


toProposal :: Grouped Proposal InstrumentProgramStatus -> ProposalPrograms
toProposal g =
  let ip = sample g
      prop =
        Proposal
          { proposalId = ip.program.proposalId
          , description = ip.program.experimentDescription
          , startTime = ip.program.startTime
          }
   in ProposalPrograms{proposal = prop, programs = Grouped g.items}


instrumentProgramStatus :: Grouped InstrumentProgram Dataset -> [Inversion] -> InstrumentProgramStatus
instrumentProgramStatus gd invs =
  InstrumentProgramStatus
    { program = instrumentProgram gd
    , status = programStatus gd invs
    }


programStatus :: Grouped InstrumentProgram Dataset -> [Inversion] -> ProgramStatus
programStatus gd [] =
  if isQualified gd
    then StatusQualified
    else StatusInvalid
programStatus _ (i : is) = do
  inversionStatus $ NE.sortWith (.updated) $ i :| is
 where
  inversionStatus is' =
    let i' = head is'
     in case i'.invError of
          Just e -> StatusError e
          Nothing -> StatusInversion (inversionStep i')


-- status gd ivs =

instrumentProgram :: Grouped InstrumentProgram Dataset -> InstrumentProgram
instrumentProgram gd =
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
        , embargo = d.embargo
        , qualified = isQualified gd
        }
 where
  midWave :: Dataset -> Wavelength Nm
  midWave d = (d.wavelengthMin + d.wavelengthMax) / 2

  identifyLine :: Dataset -> Either (Wavelength Nm) SpectralLine
  identifyLine d = maybe (Left $ midWave d) Right $ Spectra.identifyLine d


data ProgramFamily = ProgramFamily
  { program :: InstrumentProgramStatus
  , datasets :: Grouped InstrumentProgram Dataset
  , inversions :: [Inversion]
  }
