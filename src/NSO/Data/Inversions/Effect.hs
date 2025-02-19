{-# LANGUAGE RecordWildCards #-}

module NSO.Data.Inversions.Effect where

import Data.Maybe (isJust)
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.GenRandom
import Effectful.Rel8 hiding (Update)
import Effectful.Rel8 qualified as Rel8
import Effectful.Time
import NSO.Data.Inversions.Commit
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset)
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion


data Inversions :: Effect where
  All :: Inversions m AllInversions
  ById :: Id Inversion -> Inversions m [Inversion]
  ByProgram :: Id InstrumentProgram -> Inversions m [Inversion]
  ByProposal :: Id Proposal -> Inversions m [Inversion]
  Create :: Id Proposal -> Id InstrumentProgram -> Id Inversion -> Maybe GitCommit -> [Id Dataset] -> Inversions m Inversion
  NewId :: Inversions m (Id Inversion)
  Deleted :: Id Inversion -> Bool -> Inversions m ()
  Update :: Id Inversion -> (InversionRow Expr -> InversionRow Expr) -> Inversions m ()
  -- maybe doesn't belong on Inversions?
  ValidateGitCommit :: GitRepo -> GitCommit -> Inversions m Bool
type instance DispatchOf Inversions = 'Dynamic


-- | Provenance of EVERY Instrument Program
newtype AllInversions = AllInversions {inversions :: [Inversion]}


runDataInversions
  :: (Concurrent :> es, IOE :> es, Rel8 :> es, Time :> es, GenRandom :> es)
  => Eff (Inversions : es) a
  -> Eff es a
runDataInversions = interpret $ \_ -> \case
  All -> queryAll
  ByProgram pid -> queryInstrumentProgram pid
  ByProposal pid -> do
    irs <- run $ select $ do
      row <- each inversions
      where_ (row.proposalId ==. lit pid)
      return row
    pure $ map fromRow irs
  ById iid -> queryById iid
  Create propId progId invId commit dsets -> do
    row <- emptyRow propId progId invId
    let row' = row{invSoftware = commit, datasets = dsets}
    run_ $
      insert $
        Insert
          { into = inversions
          , rows = values [lit row']
          , onConflict = DoNothing
          , returning = NoReturning
          }
    pure $ fromRow row
  Deleted iid b -> do
    now <- currentTime
    let val = if b then (Just now) else Nothing
    updateInversion iid $ \InversionRow{..} -> InversionRow{deleted = lit val, ..}
  Update iid f -> updateInversion iid f
  ValidateGitCommit repo gc -> validateGitCommit repo gc
  NewId -> newInversionId
 where
  -- TODO: only return the "latest" inversion for each instrument program
  queryAll :: (Rel8 :> es) => Eff es AllInversions
  queryAll = do
    irs <- run $ select $ each inversions
    pure $ AllInversions $ map fromRow irs

  queryById :: (Rel8 :> es) => Id Inversion -> Eff es [Inversion]
  queryById iid = do
    irs <- run $ select $ do
      row <- each inversions
      where_ (row.inversionId ==. lit iid)
      pure row
    pure $ map fromRow irs

  queryInstrumentProgram :: (Rel8 :> es) => Id InstrumentProgram -> Eff es [Inversion]
  queryInstrumentProgram ip = do
    irs <- run $ select $ do
      row <- each inversions
      where_ (row.programId ==. lit ip)
      return row
    pure $ map fromRow irs

  updateInversion :: (Rel8 :> es, Time :> es) => Id Inversion -> (InversionRow Expr -> InversionRow Expr) -> Eff es ()
  updateInversion iid f = do
    now <- currentTime
    run_ $
      update $
        Rel8.Update
          { target = inversions
          , from = each inversions
          , updateWhere = \_ r -> r.inversionId ==. lit iid
          , set = \_ r -> f . setUpdated now $ r
          , returning = NoReturning
          }

  setUpdated :: UTCTime -> InversionRow Expr -> InversionRow Expr
  setUpdated now InversionRow{..} = InversionRow{updated = lit now, ..}


inversions :: TableSchema (InversionRow Name)
inversions =
  TableSchema
    { name = "inversions"
    , columns =
        InversionRow
          { inversionId = "inversion_id"
          , programId = "program_id"
          , proposalId = "proposal_id"
          , created = "created"
          , updated = "updated"
          , deleted = "deleted"
          , invError = "error"
          , datasets = "download_datasets"
          , invSoftware = "inversion_software"
          , generateFits = "generate_fits"
          , generateAsdf = "generate_asdf"
          , generateTransfer = "generate_task_completed"
          , published = "publish"
          , notes = "notes"
          }
    }


newInversionId :: (GenRandom :> es) => Eff es (Id Inversion)
newInversionId = randomId "inv"


emptyRow :: (Time :> es, GenRandom :> es) => Id Proposal -> Id InstrumentProgram -> Id Inversion -> Eff es (InversionRow Identity)
emptyRow propId progId invId = do
  now <- currentTime
  pure $
    InversionRow
      { inversionId = invId
      , programId = progId
      , proposalId = propId
      , created = now
      , updated = now
      , deleted = Nothing
      , invError = Nothing
      , datasets = []
      , invSoftware = Nothing
      , generateFits = Nothing
      , generateAsdf = Nothing
      , generateTransfer = Nothing
      , published = Nothing
      , notes = ""
      }


fromRow :: InversionRow Identity -> Inversion
fromRow row =
  Inversion
    { inversionId = row.inversionId
    , programId = row.programId
    , proposalId = row.proposalId
    , datasets = row.datasets
    , created = row.created
    , deleted = isJust row.deleted
    , updated = row.updated
    , invError = row.invError
    , invert = invert row
    , generate = generate row
    , published = row.published
    , notes = row.notes
    }


invert :: InversionRow Identity -> Invert
invert row =
  Invert
    { datasets = row.datasets
    , commit = row.invSoftware
    }


generate :: InversionRow Identity -> Generate
generate row =
  Generate
    { fits = row.generateFits
    , asdf = row.generateAsdf
    , transfer = row.generateTransfer
    }
