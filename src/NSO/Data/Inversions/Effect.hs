{-# LANGUAGE RecordWildCards #-}

module NSO.Data.Inversions.Effect where

import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.GenRandom
import Effectful.Rel8 hiding (Update)
import Effectful.Rel8 qualified as Rel8
import Effectful.Time
import NSO.Data.Inversions.Commit
import NSO.Data.Inversions.Step
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion


data Inversions :: Effect where
  All :: Inversions m AllInversions
  ById :: Id Inversion -> Inversions m [Inversion]
  ByProgram :: Id InstrumentProgram -> Inversions m [Inversion]
  ByProposal :: Id Proposal -> Inversions m [Inversion]
  Create :: Id Proposal -> Id InstrumentProgram -> Inversions m Inversion
  Remove :: Id Inversion -> Inversions m ()
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
  Create ip iip -> create ip iip
  Remove iid -> remove iid
  Update iid f -> updateInversion iid f
  ValidateGitCommit repo gc -> validateGitCommit repo gc
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

  remove :: (Rel8 :> es) => Id Inversion -> Eff es ()
  remove iid = do
    run_ $
      delete $
        Delete
          { from = inversions
          , using = each inversions
          , deleteWhere = \_ r -> r.inversionId ==. lit iid
          , returning = Returning (.inversionId)
          }

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

  create :: (Rel8 :> es, Time :> es, GenRandom :> es) => Id Proposal -> Id InstrumentProgram -> Eff es Inversion
  create ip iip = do
    inv <- empty ip iip
    run_ $
      insert $
        Insert
          { into = inversions
          , rows = values [lit (emptyRow inv)]
          , onConflict = DoNothing
          , returning = NoReturning
          }
    pure inv
   where
    emptyRow :: Inversion -> InversionRow Identity
    emptyRow inv =
      InversionRow
        { inversionId = inv.inversionId
        , programId = inv.programId
        , proposalId = inv.proposalId
        , created = inv.created
        , updated = inv.created
        , invError = Nothing
        , downloaded = Nothing
        , downloadTaskId = Nothing
        , downloadDatasets = []
        , uploaded = Nothing
        , uploadTaskId = Nothing
        , inverted = Nothing
        , inversionSoftware = Nothing
        , generatedFits = Nothing
        , generatedAsdf = Nothing
        , generateTaskId = Nothing
        , generateTaskCompleted = Nothing
        , published = Nothing
        , publishTaskId = Nothing
        }


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
          , invError = "error"
          , downloaded = "download"
          , downloadTaskId = "download_task_id"
          , downloadDatasets = "download_datasets"
          , uploaded = "upload"
          , uploadTaskId = "upload_task_id"
          , inverted = "inversion"
          , inversionSoftware = "inversion_software"
          , generatedFits = "generate_fits"
          , generatedAsdf = "generate_asdf"
          , generateTaskId = "generate_task_id"
          , generateTaskCompleted = "generate_task_completed"
          , published = "publish"
          , publishTaskId = "publish_task_id"
          }
    }


empty :: (Time :> es, GenRandom :> es) => Id Proposal -> Id InstrumentProgram -> Eff es Inversion
empty ip iip = do
  now <- currentTime
  i <- randomId "inv"
  pure $
    Inversion
      { inversionId = i
      , programId = iip
      , proposalId = ip
      , created = now
      , updated = now
      , invError = Nothing
      , download = StepDownloadNone
      , invert = StepInvertNone
      , generate = StepGenerateNone
      , publish = StepPublishNone
      }


fromRow :: InversionRow Identity -> Inversion
fromRow row =
  Inversion
    { inversionId = row.inversionId
    , programId = row.programId
    , proposalId = row.proposalId
    , created = row.created
    , updated = row.updated
    , invError = row.invError
    , download = stepDownload row
    , invert = stepInvert row
    , generate = stepGenerate row
    , publish = stepPublish row
    }
