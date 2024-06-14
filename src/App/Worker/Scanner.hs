module App.Worker.Scanner where

import App.Worker.FitsGenWorker qualified as FitsGenWorker
import Data.Text (pack)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Chan
import NSO.Prelude
import NSO.Types.Common


scan :: (Concurrent :> es, IOE :> es) => Chan FitsGenWorker.Task -> Eff es ()
scan fits = do
  putStrLn "SCAN"

  -- forM_ [0 .. 10 :: Int] $ \n -> do
  --   writeChan fits $ FitsGenWorker.Task (Id (pack $ show n))

  threadDelay 2000000
