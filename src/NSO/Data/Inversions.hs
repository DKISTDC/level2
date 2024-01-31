module NSO.Data.Inversions where

import App.Error
import Data.Aeson
import Data.Diverse.Many hiding (select)
import Data.Tagged
import Effectful.Error.Dynamic
import Effectful.Rel8
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import Rel8


-- https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/ManySpec.hs

-- The database definition is flattened. Needs validation on return from DB!
data InversionRow f = InversionRow
  { inversionId :: Column f (Id Inversion)
  , instrumentProgramId :: Column f (Id InstrumentProgram)
  , started :: Column f Started
  , downloaded :: Column f (Maybe Downloaded)
  , calibrated :: Column f (Maybe Calibrated)
  , inverted :: Column f (Maybe Inverted)
  , processed :: Column f (Maybe Processed)
  , published :: Column f (Maybe Published)
  }
  deriving (Generic, Rel8able)


data Inversion = Inversion
  { inversionId :: Id Inversion
  , instrumentProgramId :: Id InstrumentProgram
  , step :: InversionStep
  }


type Url = String
newtype InversionSoftware = InversionSoftware String
  deriving newtype (Show, Eq, ToJSON, FromJSON)


data Started = Started {timestamp :: UTCTime}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Started where
  typeInformation = jsonTypeInfo


data Downloaded = Downloaded {timestamp :: UTCTime}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Downloaded where
  typeInformation = jsonTypeInfo


data Calibrated = Calibrated {timestamp :: UTCTime, calibrationUrl :: Url}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Calibrated where
  typeInformation = jsonTypeInfo


data Inverted = Inverted {timestamp :: UTCTime, inversionSoftware :: InversionSoftware}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Inverted where
  typeInformation = jsonTypeInfo


data Processed = Processed {timestamp :: UTCTime}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Processed where
  typeInformation = jsonTypeInfo


data Published = Published {timestamp :: UTCTime}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance DBType Published where
  typeInformation = jsonTypeInfo


-- Each step depends on the previous step being completed
-- it'll be useful to pass these around
type StepStarted = '[Started]
type StepDownloaded = Downloaded : StepStarted
type StepCalibrated = Calibrated : StepDownloaded
type StepInverted = Inverted : StepCalibrated
type StepProcessed = Processed : StepInverted
type StepPublished = Published : StepProcessed


data InversionStep
  = StepStarted (Many StepStarted)
  | StepDownloaded (Many StepDownloaded)
  | StepCalibrated (Many StepCalibrated)
  | StepInverted (Many StepInverted)
  | StepProcessed (Many StepProcessed)
  | StepPublished (Many StepPublished)


-- I could use effects instead of this?
inversion :: InversionRow Identity -> Either String Inversion
inversion row = maybe err pure $ do
  stp <-
    Nothing
      <|> (StepPublished <$> published)
      <|> (StepProcessed <$> processed)
      <|> (StepInverted <$> inverted)
      <|> (StepCalibrated <$> calibrated)
      <|> (StepDownloaded <$> downloaded)
      <|> (StepStarted <$> started)
  pure
    $ Inversion
      { inversionId = row.inversionId
      , instrumentProgramId = row.instrumentProgramId
      , step = stp
      }
 where
  err = Left $ "Bad Step: " <> cs row.inversionId.fromId

  step :: Maybe a -> Maybe (Many previous) -> Maybe (Many (a : previous))
  step ma mmp = do
    mp <- mmp
    a <- ma
    pure $ a ./ mp

  -- this isn't right! Where/s my error?
  started :: Maybe (Many StepStarted)
  started = do
    pure $ row.started ./ nil

  downloaded :: Maybe (Many StepDownloaded)
  downloaded = step row.downloaded started

  calibrated :: Maybe (Many StepCalibrated)
  calibrated = step row.calibrated downloaded

  inverted :: Maybe (Many StepInverted)
  inverted = step row.inverted calibrated

  processed :: Maybe (Many StepProcessed)
  processed = step row.processed inverted

  published :: Maybe (Many StepPublished)
  published = step row.published processed


-- data Step
--   = StepStarted (Many '[Started])
--   | StepDownloaded (Many '[Started, Downloaded])
--   | StepCalibrated (Many '[Started, Downloaded, Calibrated])
--   | StepInverted (Many '[Started, Downloaded, Calibrated, Inverted])
--   | StepProcessed (Many '[Started, Downloaded, Calibrated, Inverted, Processed])
--   | StepPublished (Many '[Started, Downloaded, Calibrated, Inverted, Processed, Published])

inversions :: TableSchema (InversionRow Name)
inversions =
  TableSchema
    { name = "inversions"
    , schema = Nothing
    , columns =
        InversionRow
          { inversionId = "inversion_id"
          , instrumentProgramId = "instrument_program_id"
          , started = "started"
          , downloaded = "downloaded"
          , calibrated = "calibrated"
          , inverted = "inverted"
          , processed = "processed"
          , published = "published"
          }
    }


test :: IO ()
test = do
  putStrLn "HELLO"


-- ok, this works well!
woot :: Many '[Int, Tagged "hello" Char]
woot = (5 :: Int) ./ Tagged @"hello" 'A' ./ nil


-- I *could* handle it all in the serialization step, but no, a recursive strucure would be obnoxious to work with

queryInstrumentProgram :: (Rel8 :> es, Error AppError :> es) => Eff es [Inversion]
queryInstrumentProgram = do
  irs <- query () $ select $ do
    row <- each inversions
    where_ (row.latest ==. lit True)
    return row

  case traverse inversion irs of
    -- invalid! throw error!
    Left err -> fail err
    Right ivs -> pure ivs
