module NSO.Data.Inversions where

import App.Error
import Data.Diverse.Many hiding (select)
import Effectful.Error.Static
import Effectful.Rel8
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Status
import Rel8


-- The database definition is flattened. Needs validation on return from DB!
data InversionRow f = InversionRow
  { inversionId :: Column f (Id Inversion)
  , programId :: Column f (Id InstrumentProgram)
  , created :: Column f UTCTime
  , download :: Column f (Maybe UTCTime)
  , calibration :: Column f (Maybe UTCTime)
  , calibrationUrl :: Column f (Maybe Url)
  , inversion :: Column f (Maybe UTCTime)
  , inversionSoftware :: Column f (Maybe InversionSoftware)
  , postProcess :: Column f (Maybe UTCTime)
  , publish :: Column f (Maybe UTCTime)
  }
  deriving (Generic, Rel8able)


data Inversion = Inversion
  { inversionId :: Id Inversion
  , programId :: Id InstrumentProgram
  , step :: InversionStep
  }


inversion :: InversionRow Identity -> Either String Inversion
inversion row = maybe err pure $ do
  -- Parse each step. Try the last one first
  stp <- step
  pure
    $ Inversion
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


queryInstrumentProgram :: (Rel8 :> es, Error AppError :> es) => Id InstrumentProgram -> Eff es [Inversion]
queryInstrumentProgram ip = do
  irs <- query () $ select $ do
    row <- each inversions
    where_ (row.programId ==. lit ip)
    return row
  toInversions irs


-- TODO: only return the "latest" inversion for each instrument program
queryAll :: (Rel8 :> es, Error AppError :> es) => Eff es AllInversions
queryAll = do
  irs <- query () $ select $ do
    each inversions
  AllInversions <$> toInversions irs


toInversions :: (Error AppError :> es) => [InversionRow Identity] -> Eff es [Inversion]
toInversions irs = do
  case traverse inversion irs of
    Left err -> throwError $ ValidationError err
    Right ivs -> pure ivs


-- | Provenance of EVERY Instrument Program
newtype AllInversions = AllInversions [Inversion]
