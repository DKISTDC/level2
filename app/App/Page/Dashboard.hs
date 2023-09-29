module App.Page.Dashboard where

import App.Colors
import Effectful
import Effectful.Rel8 (Rel8, query)
import NSO.Data.Dataset hiding (Id)
import NSO.Prelude
import Rel8
import Web.Hyperbole
import Web.UI

data Route
  = Main
  | Scan
  deriving (Show, Eq, Generic, PageRoute)

route :: (Wai :> es, Rel8 :> es) => Route -> Eff es ()
route Main = do
  ms <- query () allDatasets
  view $ viewDashboard ms
route Scan = do
  ms <- query () allDatasets
  view $ viewScanRun ms

viewDashboard :: [Dataset Result] -> View ()
viewDashboard ms = do
  swapTarget InnerHTML $ do
    row_ $ col (pad 10 . gap 10) $ do
      button (action Scan) "Scan Datasets"

      label (fontSize 32) "OPERATING PROGRAMS"

      datasetsTable ms

viewScanRun :: [Dataset Identity] -> View ()
viewScanRun ds = do
  row_ $ do
    col (pad 10 . gap 10) $ do
      label (fontSize 32) "SCAN RESULTS"
      datasetsTable ds

datasetsTable :: [Dataset Result] -> View ()
datasetsTable ds = do
  table (border 1 . pad 0) ds $ do
    tcol cell (hd "Id") $ \d -> dcell d.datasetId.fromId
    tcol cell (hd "ObsId") $ \d -> dcell d.programId.fromId
    tcol cell (hd "Stokes") $ \d -> dcell $ cs $ show d.stokesParameters
    tcol cell (hd "Date") $ \d -> dcell $ cs $ show d.createDate
    tcol cell (hd "Wave Min") $ \d -> dcell $ cs $ show d.wavelengthMin
    tcol cell (hd "Wave Max") $ \d -> dcell $ cs $ show d.wavelengthMax
 where
  hd = el (bold . bg GrayLight . pad 4)
  cell = border 1
  dcell :: Text -> View ()
  dcell = el (pad 4) . text
