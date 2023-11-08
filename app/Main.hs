module Main where

import NSO.Prelude

-- import TUI
import qualified App

-- TODO: Listen for dataset activator message
--   DONE: AMQP Library
--   TODO: worker to listen
--   TODO: worker launches scan

-- DONE: Get file metadata via object store (GraphQL API)
--  DONE: Connect to Metadata API
--  DONE: Fetch all datasets

-- DOING: Scan
--   DOING: Scan Datasets
--   DOING: Group into Observing Programs
--   TODO: Identify qualified / invertible ones

-- TODO: Integrate local database
--   TODO: Migrations
--   TODO: Access module
--   TODO: Save scan results to db
--
-- DOING: Admin Interface
--   DONE: Brick?
--   DOING: Show scanned programs
--   DOING: Show program details
--   TODO: Show program status

-- DONE: Preprocessing (Fake)

-- TODO: Place files in location when ready
--   DONE: Figure out where to put files for easy access with Han
--   TODO: Identify GLOBUS location of programs
--   TODO: Notify Han of ready programs

main :: IO ()
main = App.main

migrate :: IO ()
migrate = putStrLn "MIGRATE"
