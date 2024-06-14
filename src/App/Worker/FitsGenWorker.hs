module App.Worker.FitsGenWorker where

import Data.Text (unpack)
import Effectful
import Effectful.Concurrent
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram


data Task = Task
  { instrumentProgramId :: Id InstrumentProgram
  }


instance Show Task where
  show (Task i) = "FITS | " <> unpack i.fromId


workTask :: (IOE :> es, Concurrent :> es) => Task -> Eff es ()
workTask t = do
  print t
  threadDelay 1000000
  putStrLn $ show t <> " - done"
