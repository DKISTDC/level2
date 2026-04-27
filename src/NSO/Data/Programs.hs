module NSO.Data.Programs
  ( InstrumentProgram (..)
  , Programs (..)
  , ProgramStatus (..)
  , ProgramStore (..)
  , runDataPrograms
  , programStatus
  , instrumentProgram
  , instrumentProgramFrom
  , loadProgram
  , initStore
  ) where

import Data.Grouped
import Data.Map qualified as M
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets as Datasets hiding (All, ByProgram, ByProposal, Save)
import NSO.Data.Datasets qualified as Datasets
import NSO.Data.Inversions (Inversion (..), Inversions, inversionStatus)
import NSO.Data.Inversions qualified as Inversions
import NSO.Data.Qualify
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Proposal
import NSO.Types.Status


data Programs :: Effect where
  ProposalIds :: Programs m [Id Proposal]
  Lookup :: Id InstrumentProgram -> Programs m (Maybe InstrumentProgram)
  ByProposal :: Id Proposal -> Programs m [InstrumentProgram]
type instance DispatchOf Programs = 'Dynamic


newtype ProgramStore = ProgramStore (TVar (Map (Id InstrumentProgram) InstrumentProgram))


-- TODO: fall back to datasets database
runDataPrograms
  :: (Datasets :> es, Inversions :> es)
  => Eff (Programs : es) a
  -> Eff es a
runDataPrograms = interpret $ \_ -> \case
  ProposalIds -> do
    dsets <- send $ Datasets.Distinct Datasets.DistinctProposals
    pure $ fmap (.primaryProposalId) dsets
  Lookup progId -> do
    dsets <- Datasets.findLatest (Datasets.ByProgram progId)
    case dsets of
      (d : ds) -> fmap Just <$> loadWithDatasets $ Group (d :| ds)
      _ -> pure Nothing
  ByProposal propId -> do
    dsets <- Datasets.findLatest (Datasets.ByProposal propId)
    mapM loadWithDatasets $ grouped (.instrumentProgramId) dsets
 where
  loadWithDatasets :: (Inversions :> es) => Group (Id InstrumentProgram) Dataset -> Eff es InstrumentProgram
  loadWithDatasets dsets = do
    let progId = (sample dsets).instrumentProgramId
    invs <- send $ Inversions.ByProgramLatest progId
    pure $ instrumentProgramFrom dsets.items invs


initStore :: (Concurrent :> es) => Eff es ProgramStore
initStore = ProgramStore <$> newTVarIO M.empty


loadProgram :: (Programs :> es) => Id InstrumentProgram -> Eff es (Maybe InstrumentProgram)
loadProgram progId = send $ Lookup progId


programStatus :: NonEmpty Dataset -> Maybe Inversion -> ProgramStatus
programStatus ds Nothing =
  if isQualified ds
    then StatusQualified
    else StatusInvalid
programStatus _ (Just inv) = do
  StatusInversion $ inversionStatus inv


instrumentProgramFrom :: NonEmpty Dataset -> Maybe Inversion -> InstrumentProgram
instrumentProgramFrom ds minv =
  let d = head ds
      ps = programStatus ds minv
   in instrumentProgram d ps


instrumentProgram :: Dataset -> ProgramStatus -> InstrumentProgram
instrumentProgram d ps =
  InstrumentProgram
    { programId = d.instrumentProgramId
    , proposalId = d.primaryProposalId
    , experimentId = d.primaryExperimentId
    , instrument = d.instrument
    , createDate = d.createDate
    , startTime = d.startTime
    , stokesParameters = d.stokesParameters
    , spectralLines = d.spectralLines
    , embargo = d.embargo
    , status = ps
    }

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

-- midWave :: Dataset -> Wavelength Nm
-- midWave d =
--   let Wavelength mn = d.wavelengthMin
--       Wavelength mx = d.wavelengthMax
--    in Wavelength $ fromIntegral $ round @Double @Int $ (mn + mx) / 2.0

-- identifyLine :: Dataset -> Either (Wavelength Nm) SpectralLine
-- identifyLine d = maybe (Left $ midWave d) Right $ Spectra.identifyLine d
