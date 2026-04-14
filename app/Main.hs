module Main where

import App qualified
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Log as Log
import Effectful.Reader.Dynamic
import NSO.Prelude
import Publisher qualified
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)


-- Run: ./level2 app publisher
-- Run: ./level2 app
-- Run: ./level2 publisher
-- Run: cabal run level2 -- publisher
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  putStrLn $ "NSO Level2: " <> show args

  runEff . runConcurrent $ do
    logs <- Log.init

    runReader logs $ do
      concurrently_ Log.startUpdater $ do
        forConcurrently_ args $ \arg -> do
          case arg of
            "app" -> App.start
            "publisher" -> Publisher.start
            p -> error $ "Unrecognized process: " <> p
