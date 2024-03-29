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
  ) where

import Control.Monad (void)
import Data.Diverse.Many hiding (select)
import Data.Text qualified as Text
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
  SetPublished :: Id Inversion -> Inversions m ()
  -- maybe doesn't belong on Inversions?
  ValidateGitCommit :: GitRepo -> GitCommit -> Inversions m Bool
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
      , created = now
      , updated = now
      , step = StepCreated (start ./ nil)
      }


fromRow :: InversionRow Identity -> Either String Inversion
fromRow row = maybe err pure $ do
  -- Parse each step. Try the last one first
  stp <- step
  pure $
    Inversion
      { inversionId = row.inversionId
      , programId = row.programId
      , created = row.created
      , updated = row.updated
      , step = stp
      }
 where
  err = Left $ "Bad Step: " <> cs row.inversionId.fromId

  -- go through each possibility in reverse order. If published exists, load all previous steps
  -- if any are missing skip
  step =
    (StepPublished <$> published)
      <|> (StepGenerated <$> generated)
      <|> (StepInverted <$> inverted)
      <|> (StepInverting <$> inverting)
      <|> (StepPreprocessed <$> preprocessed)
      <|> (StepDownloaded <$> downloaded)
      <|> (StepDownloading <$> downloading)
      <|> (StepCreated <$> started)

  started :: Maybe (Many StepCreated)
  started = do
    pure $ Created row.created ./ nil

  downloaded :: Maybe (Many StepDownloaded)
  downloaded = do
    prev <- started
    down <- row.download
    task <- row.downloadTaskId
    let dtss = row.downloadDatasets
    pure $ Downloaded down task dtss ./ prev

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

  published :: Maybe (Many StepPublished)
  published = do
    prev <- generated
    publ <- row.publish
    pure $ Published publ ./ prev

  downloading :: Maybe (Many StepDownloading)
  downloading = do
    prev <- started
    task <- row.downloadTaskId
    pure $ Transfer task ./ prev


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
  SetDownloaded iid ds -> setDownloaded iid ds
  SetDownloading iid tid -> setDownloading iid tid
  SetPreprocessed iid url -> setPreprocessed iid url
  SetUploaded iid -> setUploaded iid
  SetUploading iid tid -> setUploading iid tid
  SetInversion iid soft -> setInversion iid soft
  SetGenerated iid -> setGenerated iid
  SetPublished iid -> setPublished iid
  ValidateGitCommit repo gc -> validateGitCommit repo gc
 where
  -- TODO: only return the "latest" inversion for each instrument program
  queryAll :: (Rel8 :> es, Error DataError :> es) => Eff es AllInversions
  queryAll = do
    irs <- runQuery () $ select $ do
      each inversions
    AllInversions <$> toInversions irs

  queryById :: (Rel8 :> es, Error DataError :> es) => Id Inversion -> Eff es [Inversion]
  queryById iid = do
    irs <- runQuery () $ select $ do
      row <- each inversions
      where_ (row.inversionId ==. lit iid)
      pure row
    toInversions irs

  queryInstrumentProgram :: (Rel8 :> es, Error DataError :> es) => Id InstrumentProgram -> Eff es [Inversion]
  queryInstrumentProgram ip = do
    irs <- runQuery () $ select $ do
      row <- each inversions
      where_ (row.programId ==. lit ip)
      return row
    toInversions irs

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

  setDownloading :: (Rel8 :> es, Time :> es) => Id Inversion -> Id Task -> Eff es ()
  setDownloading iid tid = do
    updateInversion iid $ \r -> r{downloadTaskId = lit (Just tid.fromId)}

  setDownloaded :: (Rel8 :> es, Time :> es) => Id Inversion -> [Id Dataset] -> Eff es ()
  setDownloaded iid ds = do
    now <- currentTime
    updateInversion iid $ \r -> r{download = lit (Just now), downloadDatasets = lit $ map (.fromId) ds}

  setUploading :: (Rel8 :> es, Time :> es) => Id Inversion -> Id Task -> Eff es ()
  setUploading iid tid = do
    updateInversion iid $ \r -> r{uploadTaskId = lit (Just tid.fromId)}

  setUploaded :: (Rel8 :> es, Time :> es) => Id Inversion -> Eff es ()
  setUploaded iid = do
    now <- currentTime
    updateInversion iid $ \r -> r{upload = lit (Just now)}

  setPreprocessed :: (Debug :> es, Rel8 :> es, Time :> es) => Id Inversion -> GitCommit -> Eff es ()
  setPreprocessed iid url = do
    now <- currentTime
    updateInversion iid $ \r -> r{preprocess = lit (Just now), preprocessSoftware = lit (Just url)}

  setInversion :: (Debug :> es, Rel8 :> es, Time :> es) => Id Inversion -> GitCommit -> Eff es ()
  setInversion iid soft = do
    now <- currentTime
    updateInversion iid $ \r -> r{inversion = lit (Just now), inversionSoftware = lit (Just soft)}

  setGenerated :: (Rel8 :> es, Time :> es) => Id Inversion -> Eff es ()
  setGenerated iid = do
    now <- currentTime
    updateInversion iid $ \r -> r{generate = lit (Just now)}

  setPublished :: (Rel8 :> es, Time :> es) => Id Inversion -> Eff es ()
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
          , publish = "publish"
          }
    }


toInversions :: (Error DataError :> es) => [InversionRow Identity] -> Eff es [Inversion]
toInversions irs = do
  case traverse fromRow irs of
    Left err -> throwError $ ValidationError err
    Right ivs -> pure ivs


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


-- TODO: Get correct repo
preprocessRepo :: GitRepo
preprocessRepo = GitRepo $ https "github.com" /: "DKISTDC" /: "level2-preprocess"


-- TODO: Get correct repo. Need to move into the data center?
desireRepo :: GitRepo
desireRepo = GitRepo $ https "github.com" /: "han-uitenbroek" /: "RH"
