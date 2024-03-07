module Effectful.Rel8
  ( Rel8 (..)
  , runRel8
  , connect
  , runQuery
  , Rel8Error (..)
  , Connection
  ) where

import Control.Exception (Exception)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Hasql.Connection (Connection, ConnectionError, Settings)
import Hasql.Connection qualified as Connection
import Hasql.Session (QueryError)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import NSO.Prelude


data Rel8 :: Effect where
  RunQuery :: params -> Statement params result -> Rel8 m result


type instance DispatchOf Rel8 = 'Dynamic


runRel8
  :: (IOE :> es, Error Rel8Error :> es)
  => Connection
  -> Eff (Rel8 : es) a
  -> Eff es a
runRel8 conn = interpret $ \_ -> \case
  RunQuery par stmt -> do
    let session = Session.statement par stmt
    er <- liftIO $ Session.run session conn
    either (throwError . Rel8ErrQuery) pure er


connect
  :: (IOE :> es, Error Rel8Error :> es)
  => Settings
  -> Eff es Connection
connect settings = do
  er <- liftIO $ Connection.acquire settings
  either (throwError . Rel8ErrConn) pure er


runQuery :: (Rel8 :> es) => params -> Statement params result -> Eff es result
runQuery par stmt = send $ RunQuery par stmt


data Rel8Error
  = Rel8ErrQuery QueryError
  | Rel8ErrConn ConnectionError
  deriving (Show, Exception)
