module Effectful.Rel8
  ( Rel8 (..)
  , runRel8
  , connect
  , run
  , run_

    -- ** Re-exports
  , Rel8Error (..)
  , Connection
  , TableSchema (..)
  , Rel8able
  , delete
  , Delete (..)
  , update
  , Update (..)
  , insert
  , Insert (..)
  , select
  , each
  , where_
  , Name
  , Expr
  , lit
  , (==.)
  , Returning (..)
  , values
  , OnConflict (..)
  , in_
  , distinctOn
  , aggregate
  , Query
  ) where

import Control.Exception (Exception)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Hasql.Connection (Connection, ConnectionError, Settings)
import Hasql.Connection qualified as Connection
import Hasql.Session (SessionError (..))
import Hasql.Session qualified as Session
import Hasql.Statement as Hasql
import NSO.Prelude
import Rel8 hiding (run, run_)
import Rel8 qualified


data Rel8 :: Effect where
  RunQuery :: Hasql.Statement () a -> Rel8 m a


-- RunQueryN :: Statement () -> Rel8 m Int64
-- Delete :: Rel8.Delete () -> Rel8 m ()

type instance DispatchOf Rel8 = 'Dynamic


runRel8
  :: (IOE :> es, Error Rel8Error :> es)
  => Connection
  -> Eff (Rel8 : es) a
  -> Eff es a
runRel8 conn = interpret $ \_ -> \case
  RunQuery stmt -> do
    let session = Session.statement () stmt
    er <- liftIO $ Session.run session conn
    either (throwError . Rel8ErrQuery) pure er


-- Delete del -> do
--   let session = Session.statement () $ Rel8.runN $ Rel8.delete stmt
--   er <- liftIO $ Session.run session conn
--   either (throwError . Rel8ErrQuery) pure er

connect
  :: (IOE :> es, Error Rel8Error :> es)
  => Settings
  -> Eff es Connection
connect settings = do
  er <- liftIO $ Connection.acquire settings
  either (throwError . Rel8ErrConn) pure er


-- make different runs here....

run :: (Rel8 :> es, Serializable exprs a) => Rel8.Statement (Query exprs) -> Eff es [a]
run stmt = send $ RunQuery $ Rel8.run stmt


run_ :: (Rel8 :> es) => Rel8.Statement a -> Eff es ()
run_ stmt = send $ RunQuery $ Rel8.run_ stmt


-- runQueryN :: (Rel8 :> es) => Statement () -> Eff es Int64
-- runQueryN stmt = send $ RunQueryN stmt

data Rel8Error
  = Rel8ErrQuery SessionError
  | Rel8ErrConn ConnectionError
  deriving (Show, Exception)
