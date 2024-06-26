{-# LANGUAGE RecordWildCards #-}

module NSO.Data.Inversions
  ( Inversions (..)
  , Inversion (..)
  , fromRow
  , runDataInversions
  , AllInversions (..)
  , module NSO.Types.Inversion
  , Id
  , desireRepo
  , preprocessRepo
  , GitRepo
  , isActive
  , GenStatus (..)
  , GenTask (..)
  ) where

import Control.Monad (void)
import Data.Diverse.Many hiding (select)
import Data.Text qualified as Text
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.GenRandom
import Effectful.Rel8
import Effectful.Time
import Effectful.Worker
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
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
  SetDownloaded :: Id Inversion -> [Id Dataset] -> Inversions m ()
  SetDownloading :: Id Inversion -> Id Task -> Inversions m ()
  SetPreprocessed :: Id Inversion -> GitCommit -> Inversions m ()
  SetUploading :: Id Inversion -> Id Task -> Inversions m ()
  SetUploaded :: Id Inversion -> Inversions m ()
  SetInversion :: Id Inversion -> GitCommit -> Inversions m ()
  SetGenerated :: Id Inversion -> Inversions m ()
  ResetGenerating :: Id Inversion -> Inversions m ()
  SetGenerating :: Id Inversion -> Id Task -> FilePath -> Inversions m ()
  SetGenTransferred :: Id Inversion -> Inversions m ()
  SetPublished :: Id Inversion -> Inversions m ()
  SetError :: Id Inversion -> Text -> Inversions m ()
  -- maybe doesn't belong on Inversions?
  ValidateGitCommit :: GitRepo -> GitCommit -> Inversions m Bool
type instance DispatchOf Inversions = 'Dynamic


-- | Provenance of EVERY Instrument Program
newtype AllInversions = AllInversions [Inversion]


newtype GenTask = GenTask {inversionId :: Id Inversion}
  deriving newtype (Ord, Eq, Show)


data GenStatus
  = GenWaiting
  | GenStarted
  | GenTransferring
  | GenCreating Int Int
  deriving (Show, Ord, Eq)


instance WorkerTask GenTask where
  type Status GenTask = GenStatus
  idle = GenWaiting


empty :: (Time :> es, GenRandom :> es) => Id InstrumentProgram -> Eff es Inversion
empty ip = do
  now <- currentTime
  i <- randomId "inv"
  let start = Created now :: Created
  pure $
    Inversion
      { inversionId = i
      , programId = ip
      , created = now
      , updated = now
      , invError = Nothing
      , step = StepCreated (start ./ nil)
      }


fromRow :: InversionRow Identity -> Inversion
fromRow row =
  -- Parse each step. Try the last one first
  let stp = fromMaybe (StepCreated started) step
   in Inversion
        { inversionId = row.inversionId
        , programId = row.programId
        , created = row.created
        , updated = row.updated
        , invError = row.invError
        , step = stp
        }
 where
  -- go through each possibility in reverse order. If published exists, load all previous steps
  -- if any are missing skip
  step =
    (StepPublished <$> published)
      <|> (StepGenerated <$> generated)
      <|> (StepGenerating <$> generating)
      <|> (StepGenTransfer <$> genTransfer)
      <|> (StepInverted <$> inverted)
      <|> (StepInverting <$> inverting)
      <|> (StepPreprocessed <$> preprocessed)
      <|> (StepDownloaded <$> downloaded)
      <|> (StepDownloading <$> downloading)
  -- <|> (StepCreated <$> started)

  started :: Many StepCreated
  started = do
    Created row.created ./ nil

  downloaded :: Maybe (Many StepDownloaded)
  downloaded = do
    let prev = started
    down <- row.download
    task <- row.downloadTaskId
    let dtss = fmap (.fromId) row.downloadDatasets
    pure $ Downloaded down task dtss ./ prev

  downloading :: Maybe (Many StepDownloading)
  downloading = do
    let prev = started
    task <- row.downloadTaskId
    pure $ Transfer task ./ prev

  preprocessed :: Maybe (Many StepPreprocessed)
  preprocessed = do
    prev <- downloaded
    pre <- row.preprocess
    sft <- row.preprocessSoftware
    pure $ Preprocessed pre sft ./ prev

  inverting :: Maybe (Many StepInverting)
  inverting = do
    prev <- preprocessed
    let sft = row.inversionSoftware
    let tsk = row.uploadTaskId
    pure $ Invert sft tsk ./ prev

  inverted :: Maybe (Many StepInverted)
  inverted = do
    prev <- preprocessed
    inv <- row.inversion
    sft <- row.inversionSoftware
    upl <- row.upload
    tsk <- row.uploadTaskId
    pure $ Inverted inv sft upl tsk ./ prev

  generated :: Maybe (Many StepGenerated)
  generated = do
    prev <- inverted
    proc <- row.generate
    pure $ Generated proc ./ prev

  generating :: Maybe (Many StepGenerating)
  generating = do
    prev <- genTransfer
    tc <- row.generateTaskCompleted
    let e = row.invError
    pure $ Generate tc e ./ prev

  genTransfer :: Maybe (Many StepGenTransfer)
  genTransfer = do
    prev <- inverted
    tsk <- row.generateTaskId
    fdir <- cs <$> row.generateL1FrameDir
    pure $ GenTransfer tsk fdir ./ prev

  published :: Maybe (Many StepPublished)
  published = do
    prev <- generated
    publ <- row.publish
    pure $ Published publ ./ prev


runDataInversions
  :: (Concurrent :> es, IOE :> es, Rel8 :> es, Time :> es, GenRandom :> es)
  => Eff (Inversions : es) a
  -> Eff es a
runDataInversions = interpret $ \_ -> \case
  All -> queryAll
  ByProgram pid -> queryInstrumentProgram pid
  ById iid -> queryById iid
  Create pid -> create pid
  Remove iid -> remove iid
  SetDownloaded iid ds -> setDownloaded iid ds
  SetDownloading iid tid -> setDownloading iid tid
  SetPreprocessed iid url -> setPreprocessed iid url
  SetUploaded iid -> setUploaded iid
  SetUploading iid tid -> setUploading iid tid
  SetInversion iid soft -> setInversion iid soft
  SetGenerated iid -> setGenerated iid
  SetGenerating iid tid fdir -> setGenerating iid tid fdir
  SetGenTransferred iid -> setGenTransferred iid
  SetPublished iid -> setPublished iid
  ValidateGitCommit repo gc -> validateGitCommit repo gc
  SetError iid e -> do
    updateInversion iid $ \InversionRow{..} -> InversionRow{invError = lit (Just e), ..}
  ResetGenerating iid -> do
    updateInversion iid $ \r -> r{generateTaskId = lit Nothing, generateL1FrameDir = lit Nothing, generateTaskCompleted = lit Nothing, invError = lit Nothing}
 where
  -- TODO: only return the "latest" inversion for each instrument program
  queryAll :: (Rel8 :> es) => Eff es AllInversions
  queryAll = do
    irs <- runQuery () $ select $ do
      each inversions
    pure $ AllInversions $ map fromRow irs

  queryById :: (Rel8 :> es) => Id Inversion -> Eff es [Inversion]
  queryById iid = do
    irs <- runQuery () $ select $ do
      row <- each inversions
      where_ (row.inversionId ==. lit iid)
      pure row
    pure $ map fromRow irs

  queryInstrumentProgram :: (Rel8 :> es) => Id InstrumentProgram -> Eff es [Inversion]
  queryInstrumentProgram ip = do
    irs <- runQuery () $ select $ do
      row <- each inversions
      where_ (row.programId ==. lit ip)
      return row
    pure $ map fromRow irs

  remove :: (Rel8 :> es) => Id Inversion -> Eff es ()
  remove iid = do
    void $
      runQuery () $
        Rel8.delete $
          Delete
            { from = inversions
            , using = each inversions
            , deleteWhere = \_ r -> r.inversionId ==. lit iid
            , returning = NumberOfRowsAffected
            }

  updateInversion :: (Rel8 :> es, Time :> es) => Id Inversion -> (InversionRow Expr -> InversionRow Expr) -> Eff es ()
  updateInversion iid f = do
    now <- currentTime
    void $
      runQuery () $
        Rel8.update $
          Update
            { target = inversions
            , from = each inversions
            , updateWhere = \_ r -> r.inversionId ==. lit iid
            , set = \_ r -> f . setUpdated now $ r
            , returning = NumberOfRowsAffected
            }

  setUpdated :: UTCTime -> InversionRow Expr -> InversionRow Expr
  setUpdated now InversionRow{..} = InversionRow{updated = lit now, ..}

  setDownloading iid tid = do
    updateInversion iid $ \r -> r{downloadTaskId = lit (Just tid)}

  setDownloaded iid ds = do
    now <- currentTime
    updateInversion iid $ \r -> r{download = lit (Just now), downloadDatasets = lit ds}

  setUploading iid tid = do
    updateInversion iid $ \r -> r{uploadTaskId = lit (Just tid)}

  setUploaded iid = do
    now <- currentTime
    updateInversion iid $ \r -> r{upload = lit (Just now)}

  setPreprocessed iid url = do
    now <- currentTime
    updateInversion iid $ \r -> r{preprocess = lit (Just now), preprocessSoftware = lit (Just url)}

  setInversion iid soft = do
    now <- currentTime
    updateInversion iid $ \r -> r{inversion = lit (Just now), inversionSoftware = lit (Just soft)}

  setGenerated iid = do
    now <- currentTime
    updateInversion iid $ \r -> r{generate = lit (Just now)}

  setGenerating iid tid fdir = do
    updateInversion iid $ \r -> r{generateTaskId = lit (Just tid), generateL1FrameDir = lit (Just (cs fdir))}

  setGenTransferred iid = do
    now <- currentTime
    updateInversion iid $ \r -> r{generateTaskCompleted = lit (Just now)}

  setPublished iid = do
    now <- currentTime
    updateInversion iid $ \r -> r{publish = lit (Just now)}

  create :: (Rel8 :> es, Time :> es, GenRandom :> es) => Id InstrumentProgram -> Eff es Inversion
  create ip = do
    inv <- empty ip
    void $
      runQuery () $
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
      InversionRow
        { inversionId = inv.inversionId
        , programId = inv.programId
        , created = inv.created
        , updated = inv.created
        , invError = Nothing
        , download = Nothing
        , downloadTaskId = Nothing
        , downloadDatasets = []
        , preprocess = Nothing
        , preprocessSoftware = Nothing
        , upload = Nothing
        , uploadTaskId = Nothing
        , inversion = Nothing
        , inversionSoftware = Nothing
        , generate = Nothing
        , generateTaskId = Nothing
        , generateL1FrameDir = Nothing
        , generateTaskCompleted = Nothing
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
          , updated = "updated"
          , invError = "error"
          , download = "download"
          , downloadTaskId = "download_task_id"
          , downloadDatasets = "download_datasets"
          , preprocess = "preprocess"
          , preprocessSoftware = "preprocess_software"
          , upload = "upload"
          , uploadTaskId = "upload_task_id"
          , inversion = "inversion"
          , inversionSoftware = "inversion_software"
          , generate = "generate"
          , generateTaskId = "generate_task_id"
          , generateL1FrameDir = "generate_l1_frame_dir"
          , generateTaskCompleted = "generate_task_completed"
          , publish = "publish"
          }
    }


validateGitCommit :: (MonadIO m) => GitRepo -> GitCommit -> m Bool
validateGitCommit (GitRepo repo) gc
  | validHash gc = checkRemoteRepo
  | otherwise = pure False
 where
  commitUrl (GitCommit hash) =
    repo /: "commit" /: hash

  httpConfig = defaultHttpConfig{httpConfigCheckResponse = \_ _ _ -> Nothing}

  checkRemoteRepo = do
    res <- runReq httpConfig $ do
      req GET (commitUrl gc) NoReqBody ignoreResponse mempty
    pure $ responseStatusCode res == 200

  -- don't allow empty hashes or hashes shorter than 7
  validHash (GitCommit hs) = Text.length hs >= 7


newtype GitRepo = GitRepo (Url Https)


preprocessRepo :: GitRepo
preprocessRepo = GitRepo $ https "github.com" /: "DKISTDC" /: "level2-preprocess"


desireRepo :: GitRepo
desireRepo = GitRepo $ https "github.com" /: "han-uitenbroek" /: "RH"


isActive :: Inversion -> Bool
isActive inv =
  case inv.step of
    StepPublished _ -> False
    _ -> True
