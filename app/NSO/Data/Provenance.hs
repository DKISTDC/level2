{-# LANGUAGE DerivingVia #-}

module NSO.Data.Provenance where

import Control.Monad (void)
import Effectful.Rel8
import Effectful.Time
import NSO.Data.Dataset
import NSO.Data.Types
import NSO.Prelude
import Rel8

type Queued = Queued' Identity
data Queued' f = Queued
  { instrumentProgramId :: Column f (Id InstrumentProgram)
  , completed :: Column f UTCTime
  }
  deriving (Generic, Rel8able)

deriving stock instance (f ~ Result) => Eq (Queued' f)

queued :: TableSchema (Queued' Name)
queued =
  TableSchema
    { name = "provenance_queued"
    , schema = Nothing
    , columns =
        Queued
          { instrumentProgramId = "instrument_program_id"
          , completed = "completed"
          }
    }

type Inverted = Inverted' Identity
data Inverted' f = Inverted
  { instrumentProgramId :: Column f (Id InstrumentProgram)
  , completed :: Column f UTCTime
  }
  deriving (Generic, Rel8able)

deriving stock instance (f ~ Result) => Eq (Inverted' f)

inverted :: TableSchema (Inverted' Name)
inverted =
  TableSchema
    { name = "provenance_inverted"
    , schema = Nothing
    , columns =
        Inverted
          { instrumentProgramId = "instrument_program_id"
          , completed = "completed"
          }
    }

data ProvenanceEntry
  = WasInverted Inverted
  | WasQueued Queued
  deriving (Eq)

completed :: ProvenanceEntry -> UTCTime
completed (WasInverted p) = p.completed
completed (WasQueued p) = p.completed

loadProvenance :: (Rel8 :> es) => Id InstrumentProgram -> Eff es [ProvenanceEntry]
loadProvenance ip = do
  qus <- queryQueued
  inv <- queryInverted
  pure $ sortOn completed $ map WasInverted inv <> map WasQueued qus
 where
  queryQueued = query () $ select $ do
    row <- each queued
    where_ (row.instrumentProgramId ==. lit ip)
    return row

  queryInverted = query () $ select $ do
    row <- each inverted
    where_ (row.instrumentProgramId ==. lit ip)
    return row

markQueued :: (Rel8 :> es, Time :> es) => Id InstrumentProgram -> Eff es ()
markQueued ip = do
  now <- currentTime
  let qd = Queued ip now
  mark queued [lit qd]

markInverted :: (Rel8 :> es, Time :> es) => Id InstrumentProgram -> Eff es ()
markInverted ip = do
  now <- currentTime
  let iv = Inverted ip now
  mark inverted [lit iv]

mark :: (Rel8 :> es, Rel8able f) => TableSchema (f Name) -> [f Expr] -> Eff es ()
mark table as = do
  void
    $ query ()
    $ Rel8.insert
    $ Insert
      { into = table
      , rows = values as
      , onConflict = DoNothing
      , returning = NumberOfRowsAffected
      }
