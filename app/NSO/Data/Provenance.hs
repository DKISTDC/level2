{-# LANGUAGE DerivingVia #-}

module NSO.Data.Provenance where

import Control.Monad (void)
import Effectful.Rel8
import Effectful.Time
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import Rel8

data QueuedRow f = QueuedRow
  { programId :: Column f (Id InstrumentProgram)
  , completed :: Column f UTCTime
  }
  deriving (Generic, Rel8able)

deriving stock instance (f ~ Result) => Eq (QueuedRow f)

queued :: TableSchema (QueuedRow Name)
queued =
  TableSchema
    { name = "provenance_queued"
    , schema = Nothing
    , columns =
        QueuedRow
          { programId = "program_id"
          , completed = "completed"
          }
    }

data InvertedRow f = InvertedRow
  { programId :: Column f (Id InstrumentProgram)
  , completed :: Column f UTCTime
  }
  deriving (Generic, Rel8able)

deriving stock instance (f ~ Result) => Eq (InvertedRow f)

inverted :: TableSchema (InvertedRow Name)
inverted =
  TableSchema
    { name = "provenance_inverted"
    , schema = Nothing
    , columns =
        InvertedRow
          { programId = "program_id"
          , completed = "completed"
          }
    }

data ProvenanceEntry
  = WasInverted (InvertedRow Identity)
  | WasQueued (QueuedRow Identity)
  deriving (Eq)

completed :: ProvenanceEntry -> UTCTime
completed (WasInverted p) = p.completed
completed (WasQueued p) = p.completed

isProgram :: Id InstrumentProgram -> ProvenanceEntry -> Bool
isProgram ip pe = programId pe == ip
 where
  programId (WasInverted p) = p.programId
  programId (WasQueued p) = p.programId

loadProvenance :: (Rel8 :> es) => Id InstrumentProgram -> Eff es [ProvenanceEntry]
loadProvenance ip = do
  qus <- queryQueued
  inv <- queryInverted
  pure $ sortOn completed $ map WasInverted inv <> map WasQueued qus
 where
  queryQueued = query () $ select $ do
    row <- each queued
    where_ (row.programId ==. lit ip)
    return row

  queryInverted = query () $ select $ do
    row <- each inverted
    where_ (row.programId ==. lit ip)
    return row

-- | Loads the entire database for sorting later. Use `isProgram` above
loadAllProvenance :: (Rel8 :> es) => Eff es AllProvenance
loadAllProvenance = do
  qus <- query () $ select $ each queued
  inv <- query () $ select $ each inverted
  pure $ AllProvenance $ map WasInverted inv <> map WasQueued qus

markQueued :: (Rel8 :> es, Time :> es) => Id InstrumentProgram -> Eff es ()
markQueued ip = do
  now <- currentTime
  let qd = QueuedRow ip now
  mark queued [lit qd]

markInverted :: (Rel8 :> es, Time :> es) => Id InstrumentProgram -> Eff es ()
markInverted ip = do
  now <- currentTime
  let iv = InvertedRow ip now
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

-- | Provenance of EVERY program
newtype AllProvenance = AllProvenance [ProvenanceEntry]
