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
import Effectful.Debug
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
import Network.Globus.Transfer (Task)
import Network.HTTP.Req
import Rel8


data Inversions :: Effect where
  All :: Inversions m AllInversions
  ById :: Id Inversion -> Inversions m [Inversion]
  ByProgram :: Id InstrumentProgram -> Inversions m [Inversion]
  Create :: Id InstrumentProgram -> Inversions m Inversion
  Remove :: Id Inversion -> Inversions m ()
  SetDownloaded :: Id Inversion -> Id Task -> Inversions m ()
  SetCalibrated :: Id Inversion -> GitCommit -> Inversions m ()
  SetInverted :: Id Inversion -> GitCommit -> Inversions m ()
  SetPostProcessed :: Id Inversion -> Inversions m ()
  SetPublished :: Id Inversion -> Inversions m ()
  ValidateDesireCommit :: GitCommit -> Inversions m Bool
  ValidateCalibrationCommit :: GitCommit -> Inversions m Bool
type instance DispatchOf Inversions = 'Dynamic


-- | Provenance of EVERY Instrument Program
newtype AllInversions = AllInversions [Inversion]


empty :: (Time :> es, GenRandom :> es) => Id InstrumentProgram -> Eff es Inversion
empty ip = do
  now <- currentTime
  i <- randomId "inv"
  let start = Created now :: Created
  pure $
    Inversion
      { inversionId = i
      , programId = ip
      , step = StepCreated (start ./ nil)
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
      <|> (StepCreated <$> started)

  started :: Maybe (Many StepCreated)
  started = do
    pure $ Created row.created ./ nil

  downloaded :: Maybe (Many StepDownloaded)
  downloaded = do
    prev <- started
    down <- row.download
    task <- row.downloadTaskId
    pure $ Downloaded down task ./ prev

  calibrated :: Maybe (Many StepCalibrated)
  calibrated = do
    prev <- downloaded
    cal <- row.calibration
    url <- row.calibrationSoftware
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
  :: (IOE :> es, Rel8 :> es, Error DataError :> es, Time :> es, GenRandom :> es, Debug :> es)
  => Eff (Inversions : es) a
  -> Eff es a
runDataInversions = interpret $ \_ -> \case
  All -> queryAll
  ByProgram pid -> queryInstrumentProgram pid
  ById iid -> queryById iid
  Create pid -> create pid
  Remove iid -> remove iid
  SetDownloaded iid tid -> setDownloaded iid tid
  SetCalibrated iid url -> setCalibrated iid url
  SetInverted iid soft -> setInverted iid soft
  SetPostProcessed iid -> setPostProcessed iid
  SetPublished iid -> setPublished iid
  ValidateDesireCommit gc -> validateGitCommit desireRepo gc
  ValidateCalibrationCommit gc -> validateGitCommit calibrationRepo gc
 where
  -- TODO: Get correct repo
  calibrationRepo = https "github.com" /: "DKISTDC" /: "level2"

  -- TODO: Get correct repo. Need to move into the data center?
  desireRepo = https "github.com" /: "han-uitenbroek" /: "RH"

  -- TODO: only return the "latest" inversion for each instrument program
  queryAll :: (Rel8 :> es, Error DataError :> es) => Eff es AllInversions
  queryAll = do
    irs <- query () $ select $ do
      each inversions
    AllInversions <$> toInversions irs

  queryById :: (Rel8 :> es, Error DataError :> es) => Id Inversion -> Eff es [Inversion]
  queryById iid = do
    irs <- query () $ select $ do
      row <- each inversions
      where_ (row.inversionId ==. lit iid)
      pure row
    toInversions irs

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

  updateInversion :: (Rel8 :> es) => Id Inversion -> (InversionRow Expr -> InversionRow Expr) -> Eff es ()
  updateInversion iid f = do
    void $
      query () $
        Rel8.update $
          Update
            { target = inversions
            , from = each inversions
            , updateWhere = \_ r -> r.inversionId ==. lit iid
            , set = \_ r -> f r
            , returning = NumberOfRowsAffected
            }

  setDownloaded :: (Rel8 :> es, Time :> es) => Id Inversion -> Id Task -> Eff es ()
  setDownloaded iid tid = do
    now <- currentTime
    updateInversion iid $ \r -> r{download = lit (Just now), downloadTaskId = lit (Just tid.fromId)}

  setCalibrated :: (Debug :> es, Rel8 :> es, Time :> es) => Id Inversion -> GitCommit -> Eff es ()
  setCalibrated iid url = do
    now <- currentTime
    updateInversion iid $ \r -> r{calibration = lit (Just now), calibrationSoftware = lit (Just url)}

  setInverted :: (Debug :> es, Rel8 :> es, Time :> es) => Id Inversion -> GitCommit -> Eff es ()
  setInverted iid soft = do
    now <- currentTime
    updateInversion iid $ \r -> r{inversion = lit (Just now), inversionSoftware = lit (Just soft)}

  setPostProcessed :: (Rel8 :> es, Time :> es) => Id Inversion -> Eff es ()
  setPostProcessed iid = do
    now <- currentTime
    updateInversion iid $ \r -> r{postProcess = lit (Just now)}

  setPublished :: (Rel8 :> es, Time :> es) => Id Inversion -> Eff es ()
  setPublished iid = do
    now <- currentTime
    updateInversion iid $ \r -> r{publish = lit (Just now)}

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
      let Created time = stepCreated inv.step
       in InversionRow
            { inversionId = inv.inversionId
            , programId = inv.programId
            , created = time
            , download = Nothing
            , downloadTaskId = Nothing
            , calibration = Nothing
            , calibrationSoftware = Nothing
            , inversion = Nothing
            , inversionSoftware = Nothing
            , postProcess = Nothing
            , publish = Nothing
            }


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
          , downloadTaskId = "download_task_id"
          , calibration = "calibration"
          , calibrationSoftware = "calibration_software"
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


validateGitCommit :: (MonadIO m) => Url Https -> GitCommit -> m Bool
validateGitCommit repo gc = do
  res <- runReq httpConfig $ do
    req GET (commitUrl gc) NoReqBody ignoreResponse mempty
  pure $ responseStatusCode res == 200
 where
  commitUrl (GitCommit hash) =
    repo /: "commit" /: hash
  httpConfig = defaultHttpConfig{httpConfigCheckResponse = \_ _ _ -> Nothing}
