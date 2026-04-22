{-# LANGUAGE StrictData #-}

module NSO.Data.Programs
  ( InstrumentProgram (..)
  , ProgramStatus (..)
  , groupByProgram
  , instrumentProgram
  , loadAllPrograms
  , loadAllProposals
  , loadProgram
  , loadProposalPrograms
  , programFamilies
  , programInversions
  , programStatus
  , proposalFromDataset
  , toProposal
  , toProposals
  ) where

import Data.Grouped as G
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Rel8
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Qualify
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Status


-- loadProgram :: (Datasets :> es, Inversions :> es) => Id InstrumentProgram -> Eff es [ProgramFamily]
-- loadProgram progId = do
--   ds <- send $ Datasets.Find $ Datasets.ByProgram progId
--   invs <- Inversions.findByProgram progId
--   pure $ programFamilies invs ds
--
--
-- loadAllPrograms :: (Datasets :> es, Inversions :> es) => Eff es [ProgramFamily]
-- loadAllPrograms = do
--   ds <- Datasets.find Datasets.All
--   AllInversions ai <- send Inversions.All
--   pure $ programFamilies ai ds
--
--
-- loadProposalPrograms :: (Datasets :> es, Inversions :> es) => Eff es [ProposalPrograms]
-- loadProposalPrograms = do
--   progs <- loadAllPrograms
--   pure $ toProposals progs

-- loadProposalProgram :: (Datasets :> es, Inversions :> es) => Id Proposal -> Eff es [ProposalPrograms]
-- loadProposalProgram propId = do
--   ds <- send $ Datasets.Find $ Datasets.ByProposal propId
--   case ds of
--     [] -> pure []
--     (d : ds) -> do
--       invs <- send $ Inversions.ByProposal propId
--       let proposal = proposalFromDataset d
--       let programs = fmap (\g -> instrumentProgramStatus g invs) (grouped (.instrumentProgramId) (d : ds))
--       let gprogs = grouped (\ip -> ip.program.proposalId) programs
--       pure $ ProposalPrograms{proposal, programs = gprogs}

groupByProgram :: [Dataset] -> [Group (Id InstrumentProgram) Dataset]
groupByProgram = grouped (.instrumentProgramId)


-- programFamilies :: [Inversion] -> [Dataset] -> [ProgramFamily]
-- programFamilies invs ds =
--   sortOn startTime $ fmap programFamily (groupByProgram ds)
--  where
--   startTime pf = pf.program.startTime
--
--   programFamily :: Group (Id InstrumentProgram) Dataset -> ProgramFamily
--   programFamily gd =
--     let invs' = programInversions invs gd
--      in ProgramFamily
--           { program = instrumentProgram gd
--           , status = programStatus gd invs'
--           , datasets = gd
--           , inversions = invs'
--           }

-- | Filter inversions for the given program
programInversions :: [Inversion] -> Id InstrumentProgram -> [Inversion]
programInversions ivs progId =
  let d = sample gd
   in filter (\i -> i.programId == progId) ivs


-- toProposals :: [ProgramFamily] -> [ProposalPrograms]
-- toProposals pfs =
--   map toProposal $ grouped (\ip -> ip.program.proposalId) pfs
--
--
-- toProposal :: Group (Id Proposal) ProgramFamily -> ProposalPrograms
-- toProposal g =
--   let ip = sample g
--       prop =
--         Proposal
--           { proposalId = ip.program.proposalId
--           , description = ip.program.experimentDescription
--           , startTime = ip.program.startTime
--           }
--    in ProposalPrograms{proposal = prop, programs = g}

programStatus :: Group (Id InstrumentProgram) Dataset -> [Inversion] -> ProgramStatus
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
          Nothing -> StatusInversion i'


-- midWave :: Dataset -> Wavelength Nm
-- midWave d =
--   let Wavelength mn = d.wavelengthMin
--       Wavelength mx = d.wavelengthMax
--    in Wavelength $ fromIntegral $ round @Double @Int $ (mn + mx) / 2.0

-- identifyLine :: Dataset -> Either (Wavelength Nm) SpectralLine
-- identifyLine d = maybe (Left $ midWave d) Right $ Spectra.identifyLine d

table :: TableSchema (InstrumentProgram' Name)
table =
  TableSchema
    { name = "programs"
    , columns =
        InstrumentProgram'
          { programId = "program_id"
          , proposalId = "proposal_id"
          , experimentId = "experiment_id"
          , instrument = "instrument"
          , createDate = "create_date"
          , startTime = "start_time"
          , stokesParameters = "stokes"
          , spectralLines = "spectral_lines"
          , embargo = "embargo"
          , status = "status"
          }
    }
