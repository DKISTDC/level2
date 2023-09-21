module App.Test where

import Data.Int (Int64)
import NSO.Prelude
import Rel8
import Hasql.Connection (acquire)
import Hasql.Session (run, statement)

data Message f = Message
  { messageId :: Column f Int64
  , message   :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
deriving stock instance f ~ Result => Show (Message f)

messages :: TableSchema (Message Name)
messages = TableSchema
  { name = "test"
  , schema = Nothing
  , columns = Message
      { messageId = "message_id"
      , message = "message"
      }
  }

test :: IO ()
test = do
  let url = "postgres://guest:guest@127.0.0.1:5432/level2"
  Right conn <- acquire url
  let session = statement () $ select (each messages)
  res <- run session conn
  print res
  -- mapM_ print res
  -- putStrLn "EHLLO"
