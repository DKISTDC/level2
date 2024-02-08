module NSO.Data.Inversions
  ( Inversions (..)
  , Inversion (..)
  , inversion
  , runDataInversions
  , AllInversions (..)
  , module NSO.Types.Inversion
  , Id
  ) where

import Control.Monad (void)
import Data.Diverse.Many hiding (select)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Rel8
import Effectful.Time
import NSO.Error
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Rel8


data Inversions :: Effect where
  All :: Inversions m AllInversions
  ByProgram :: Id InstrumentProgram -> Inversions m [Inversion]
  Create :: Id InstrumentProgram -> Inversions m Inversion
  SetDownloaded :: Id Inversion -> Inversions m ()
  Remove :: Id Inversion -> Inversions m ()


type instance DispatchOf Inversions = 'Dynamic


-- | Provenance of EVERY Instrument Program
newtype AllInversions = AllInversions [Inversion]


empty :: (Time :> es, GenRandom :> es) => Id InstrumentProgram -> Eff es Inversion
empty ip = do
  now <- currentTime
  i <- randomId
  let start = Started now :: Started
  pure $
    Inversion
      { inversionId = i
      , programId = ip
      , step = StepStarted (start ./ nil)
      }


inversion :: InversionRow Identity -> Either String Inversion
inversion row = maybe err pure $ do
  -- Parse each step. Try the last one first
  stp <- step
  pure $
    Inversion
      { inversionId = row.inversionId
      , programId = row.programId
      , step = stp
      }
 where
  err = Left $ "Bad Step: " <> cs row.inversionId.fromId

  -- go through each possibility in reverse order. If published exists, load all previous steps
  -- if any are missing skip
  step =
    (StepPublished <$> published)
      <|> (StepProcessed <$> processed)
      <|> (StepInverted <$> inverted)
      <|> (StepCalibrated <$> calibrated)
      <|> (StepDownloaded <$> downloaded)
      <|> (StepStarted <$> started)

  started :: Maybe (Many StepStarted)
  started = do
    pure $ Started row.created ./ nil

  downloaded :: Maybe (Many StepDownloaded)
  downloaded = do
    prev <- started
    down <- row.download
    pure $ Downloaded down ./ prev

  calibrated :: Maybe (Many StepCalibrated)
  calibrated = do
    prev <- downloaded
    cal <- row.calibration
    url <- row.calibrationUrl
    pure $ Calibrated cal url ./ prev

  inverted :: Maybe (Many StepInverted)
  inverted = do
    prev <- calibrated
    inv <- row.inversion
    sft <- row.inversionSoftware
    pure $ Inverted inv sft ./ prev

  processed :: Maybe (Many StepProcessed)
  processed = do
    prev <- inverted
    proc <- row.postProcess
    pure $ Processed proc ./ prev

  published :: Maybe (Many StepPublished)
  published = do
    prev <- processed
    publ <- row.publish
    pure $ Published publ ./ prev


runDataInversions
  :: (IOE :> es, Rel8 :> es, Error DataError :> es, Time :> es, GenRandom :> es)
  => Eff (Inversions : es) a
  -> Eff es a
runDataInversions = interpret $ \_ -> \case
  All -> queryAll
  ByProgram pid -> queryInstrumentProgram pid
  Create pid -> create pid
  Remove iid -> remove iid
  SetDownloaded iid -> setDownloaded iid
 where
  -- TODO: only return the "latest" inversion for each instrument program
  queryAll :: (Rel8 :> es, Error DataError :> es) => Eff es AllInversions
  queryAll = do
    irs <- query () $ select $ do
      each inversions
    AllInversions <$> toInversions irs

  queryInstrumentProgram :: (Rel8 :> es, Error DataError :> es) => Id InstrumentProgram -> Eff es [Inversion]
  queryInstrumentProgram ip = do
    irs <- query () $ select $ do
      row <- each inversions
      where_ (row.programId ==. lit ip)
      return row
    toInversions irs

  remove :: (Rel8 :> es) => Id Inversion -> Eff es ()
  remove iid = do
    void $
      query () $
        Rel8.delete $
          Delete
            { from = inversions
            , using = each inversions
            , deleteWhere = \_ r -> r.inversionId ==. lit iid
            , returning = NumberOfRowsAffected
            }

  -- you can only set this if it is currently StepStarted...
  setDownloaded :: (Rel8 :> es, Time :> es) => Id Inversion -> Eff es ()
  setDownloaded iid = do
    now <- currentTime
    void $
      query () $
        Rel8.update $
          Update
            { target = inversions
            , from = each inversions
            , updateWhere = \_ r -> r.inversionId ==. lit iid
            , set = \_ r -> r{download = lit (Just now)}
            , returning = NumberOfRowsAffected
            }

  create :: (Rel8 :> es, Time :> es, GenRandom :> es) => Id InstrumentProgram -> Eff es Inversion
  create ip = do
    inv <- empty ip
    void $
      query () $
        Rel8.insert $
          Insert
            { into = inversions
            , rows = values [lit (emptyRow inv)]
            , onConflict = DoNothing
            , returning = NumberOfRowsAffected
            }
    pure inv
   where
    emptyRow :: Inversion -> InversionRow Identity
    emptyRow inv =
      let Started time = stepStarted inv.step
       in InversionRow
            { inversionId = inv.inversionId
            , programId = inv.programId
            , created = time
            , download = Nothing
            , calibration = Nothing
            , calibrationUrl = Nothing
            , inversion = Nothing
            , inversionSoftware = Nothing
            , postProcess = Nothing
            , publish = Nothing
            }

    stepStarted :: InversionStep -> Started
    stepStarted (StepStarted m) = grab @Started m
    stepStarted (StepDownloaded m) = grab @Started m
    stepStarted (StepCalibrated m) = grab @Started m
    stepStarted (StepInverted m) = grab @Started m
    stepStarted (StepProcessed m) = grab @Started m
    stepStarted (StepPublished m) = grab @Started m


inversions :: TableSchema (InversionRow Name)
inversions =
  TableSchema
    { name = "inversions"
    , schema = Nothing
    , columns =
        InversionRow
          { inversionId = "inversion_id"
          , programId = "program_id"
          , created = "created"
          , download = "download"
          , calibration = "calibration"
          , calibrationUrl = "calibration_url"
          , inversion = "inversion"
          , inversionSoftware = "inversion_software"
          , postProcess = "post_process"
          , publish = "publish"
          }
    }


toInversions :: (Error DataError :> es) => [InversionRow Identity] -> Eff es [Inversion]
toInversions irs = do
  case traverse inversion irs of
    Left err -> throwError $ ValidationError err
    Right ivs -> pure ivs
