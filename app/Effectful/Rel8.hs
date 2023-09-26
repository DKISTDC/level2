module Effectful.Rel8 where

import Control.Exception (Exception)
import NSO.Prelude
import Effectful
import Effectful.Error.Static
import Effectful.Dispatch.Dynamic
import Hasql.Connection (Settings, Connection, ConnectionError)
import Hasql.Connection qualified as Connection
import Hasql.Statement (Statement)
import Hasql.Session (QueryError)
import Hasql.Session qualified as Session

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
    either (throwError . Query) pure er

connect
  :: (IOE :> es, Error Rel8Error :> es)
  => Settings
  -> Eff es Connection
connect settings = do
  er <- liftIO $ Connection.acquire settings
  either (throwError . Conn) pure er
  
query :: Rel8 :> es => params -> Statement params result -> Eff es result
query par stmt = send $ RunQuery par stmt

data Rel8Error
  = Query QueryError
  | Conn ConnectionError
  deriving (Show, Exception)
