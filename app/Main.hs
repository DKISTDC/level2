module Main where

import TUI

-- TODO: Listen for dataset activator message
--   DONE: AMQP Library
--   TODO: worker to listen
--   TODO: worker launches main process

-- DONE: Get file metadata via object store (GraphQL API)
--  DONE: Connect to Metadata API
--  DONE: Fetch all datasets

-- TODO: Sort
--   TODO: Download files via GLOBUS API? (Not wh

-- TODO: Scan metadata store
--   TODO: Identify valid datasets
--   TODO: Update state in store

-- TODO: Integrate local database
--   TODO: Migrations
--   TODO: Access module

-- TODO: Admin Interface
--   DOING: Brick?
--   TODO: Web?
--   TODO: Show status

-- DONE: Preprocessing (Fake)

-- TODO: Place files in location when ready
--   DOING: Figure out where to put files for easy access with Han

main :: IO ()
main = TUI.main
