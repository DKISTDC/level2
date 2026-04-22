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

import Data.Grouped as G
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets (Dataset, Dataset' (..))
import NSO.Data.Datasets qualified as Datasets
import NSO.Data.Inversions (Inversion (..), inversionStatus)
import NSO.Data.Inversions qualified as Inversions
import NSO.Data.Qualify
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Proposal
import NSO.Types.Status


data Programs :: Effect where
  Save :: InstrumentProgram -> Programs m ()
  Lookup :: Id InstrumentProgram -> Programs m (Maybe InstrumentProgram)
  All :: Programs m [InstrumentProgram]
  ProposalIds :: Programs m [Id Proposal]
  ByProposal :: Id Proposal -> Programs m [InstrumentProgram]
type instance DispatchOf Programs = 'Dynamic


newtype ProgramStore = ProgramStore (TVar (Map (Id InstrumentProgram) InstrumentProgram))


runDataPrograms
  :: (Concurrent :> es)
  => ProgramStore
  -> Eff (Programs : es) a
  -> Eff es a
runDataPrograms (ProgramStore var) = interpret $ \_ -> \case
  Save prog -> do
    atomically $ modifyTVar var $ M.insert prog.programId prog
  Lookup progId -> do
    M.lookup progId <$> readTVarIO var
  All -> do
    M.elems <$> readTVarIO var
  ByProposal propId -> do
    filter (\prog -> prog.proposalId == propId) . M.elems <$> readTVarIO var
  ProposalIds -> do
    L.nub . fmap (.proposalId) . M.elems <$> readTVarIO var


initStore :: (Concurrent :> es) => Eff es ProgramStore
initStore = ProgramStore <$> newTVarIO M.empty


loadProgram :: (Programs :> es) => Id InstrumentProgram -> Eff es (Maybe InstrumentProgram)
loadProgram progId = send $ Lookup progId


programStatus :: NonEmpty Dataset -> [Inversion] -> ProgramStatus
programStatus ds [] =
  if isQualified ds
    then StatusQualified
    else StatusInvalid
programStatus _ invs = do
  StatusInversion $ inversionStatus $ L.maximumBy (comparing (.updated)) invs


instrumentProgramFrom :: NonEmpty Dataset -> [Inversion] -> InstrumentProgram
instrumentProgramFrom ds invs =
  let d = head ds
      ps = programStatus ds invs
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
