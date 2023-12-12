module Main where

import Data.Text (unpack)
import Prelude


main :: IO ()
main = putStrLn $ unpack "HELLO WORLD"
