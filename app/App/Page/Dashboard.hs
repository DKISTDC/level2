module App.Page.Dashboard where

import Control.Monad.Trans (lift)
import NSO.Prelude
import Web.Htmx
import Web.Hyperbole (view)
import Rel8
import Web.Hyperbole.Htmx
import Web.Scotty hiding (text)
import NSO.Data.Dataset hiding (Id)
import Web.UI
import Web.UI.Url
import Data.Text.Lazy qualified as L
import Effectful
import Effectful.Rel8 (query, Rel8)
import Effectful.Request (GraphQL)

route :: ScottyM ()
route = do
  get "/" $ do
    -- u <- loadUser
    ms <- query () allDatasets
    view $ viewDashboard ms

  get "/dashboard" $ do
    -- u <- loadUser
    ms <- query () allDatasets
    view $ viewDashboard ms

  get "/dashboard/scan" $ do
    view $ viewScan Nothing

  post "/dashboard/scan" $ do
    ds <- query () allDatasets
    view $ viewScan $ Just ds

  -- get "/contact/:id/edit" $ do
  --   u <- loadUser
  --   view $ editContact u
  --
  -- post "/contact/:id/save" $ do
  --   u <- userFormData
  --   Users.save us u
  --   view $ viewContact u
 where
  -- loadUser :: ActionM User
  -- loadUser = do
  --   uid <- param "id"
  --   mu <- Users.load us uid
  --   maybe next pure mu
  --
  -- userFormData :: ActionM User
  -- userFormData = do
  --   uid <- param "id"
  --   firstName <- param "firstName"
  --   lastName <- param "lastName"
  --   email <- param "email"
  --   pure $ User uid firstName lastName email True

viewScan :: Maybe [Dataset Result] -> View ()
viewScan Nothing = do
  row (hxTarget This . hxSwap OuterHTML) $ do
    col (pad 10 . gap 10) $ do
      -- 1. Make a POST to the server
      -- 2. The server works
      -- 3. Prints out any NEW ones found... or the status of all of them..
      button (hxPost ("dashboard" // "scan")) "RUN SCAN"

viewScan (Just ds) = do
  row (hxTarget This . hxSwap OuterHTML) $ do
    col (pad 10 . gap 10) $ do
      el_ "SCANNED!"
      mapM_ datasetRow ds


viewDashboard :: [Dataset Result] -> View ()
viewDashboard ms = do
  row_ $ do
    col (pad 10 . gap 10) $ do
      -- they could automatically SET the target
      -- oh, but different buttons could do DIFFERENT things
      col (hxTarget (Query (Id "hi")) . hxSwap OuterHTML) $ do
        -- button says: Target ID, Swap with URL
        -- always swap OuterHTML?
        -- wait, but this is going to do the whole scan page!
        link ("dashboard" // "scan") id "Scan Datasets"

      el (att "id" "hi") $ do
        label (fontSize 32) "OPERATING PROGRAMS"

      mapM_ datasetRow ms


datasetRow :: Dataset Result -> View ()
datasetRow d = do
  row (gap 8) $ do
    el_ $ text $ cs $ show d.datasetId
    el_ $ text $ cs $ show d.programId
    el_ $ text $ cs $ show d.stokesParameters
    el_ $ text $ cs $ show d.createDate
    el_ $ text $ cs $ show d.wavelengthMin
    el_ $ text $ cs $ show d.wavelengthMax



