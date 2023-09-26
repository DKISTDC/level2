module App.Page.Dashboard where

import Control.Monad.Trans (lift)
import NSO.Prelude
import Web.Htmx
import Web.Hyperbole (view)
import Rel8
import Web.Hyperbole.Htmx
import Web.Scotty.Trans hiding (text)
import NSO.Data.Dataset hiding (Id)
import Web.UI
import Web.UI.Url
import Data.Text.Lazy qualified as L
import Effectful
import Effectful.Rel8 (query, Rel8)

route :: (IOE :> es, Rel8 :> es) => ScottyT L.Text (Eff es) ()
route = do
  get "/" $ do
    -- u <- loadUser
    ms <- lift $ query () allDatasets
    view $ viewDashboard ms

  get "/dashboard" $ do
    -- u <- loadUser
    ms <- lift $ query () allDatasets
    view $ viewDashboard ms

  get "/dashboard/scan" $ do
    view viewScan

  post "/dashboard/scan" $ do
    view $ el_ "SCANNING...."

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

viewScan :: View ()
viewScan = do
  row_ $ do
    col (pad 10 . gap 10 . hxTarget This . hxSwap InnerHTML) $ do
      button (hxPost ("dashboard" // "scan")) "RUN SCAN"
      

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

      forM_ ms $ \m -> do
        row (gap 8) $ do
          el_ $ text $ cs $ show m.datasetId
          el_ $ text $ cs $ show m.programId
          el_ $ text $ cs $ show m.stokesParameters
          el_ $ text $ cs $ show m.createDate
          el_ $ text $ cs $ show m.wavelengthMin
          el_ $ text $ cs $ show m.wavelengthMax



