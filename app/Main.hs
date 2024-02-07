module Main where

import App qualified
import App.Mock (mockApp)
import NSO.Prelude
import System.IO


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  App.main
