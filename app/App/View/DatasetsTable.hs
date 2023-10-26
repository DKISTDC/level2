module App.View.DatasetsTable where

import App.Colors
import NSO.Data.Dataset
import NSO.Data.Types
import NSO.Prelude
import Numeric (showFFloat)
import Web.UI

datasetsTable :: [Dataset] -> View c ()
datasetsTable ds = do
  let sorted = ds

  -- is there a way to do alternating rows here?
  table (odd (bg White) . even (bg Light)) sorted $ do
    tcol (hd "Id") $ \d -> cell d.datasetId.fromId
    tcol (hd "Create Date") $ \d -> cell . showTimestamp $ d.createDate
    tcol (hd "Start Time") $ \d -> cell . showTimestamp $ d.startTime
    tcol (hd "Instrument") $ \d -> cell . cs . show $ d.instrument
    tcol (hd "Stokes") $ \d -> cell . cs . show $ d.stokesParameters
    tcol (hd "Wave Min") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMin ""
    tcol (hd "Wave Max") $ \d -> cell . cs $ showFFloat (Just 1) d.wavelengthMax ""
    tcol (hd "Bounding Box") $ \d -> cell . cs $ maybe "" show d.boundingBox
    -- tcol (hd "Exposure Time") $ \d -> cell . cs . show $ d.exposureTime
    -- tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
    tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
 where
  -- tcol (hd "On Disk") $ \d -> cell . cs . show $ isOnDisk d.boundingBox

  -- tcol cell (hd "End Time") $ \d -> cell . cs . show $ d.endTime
  -- tcol cell (hd "peid") $ \d -> cell . cs $ d.primaryExperimentId
  -- tcol cell (hd "ppid") $ \d -> cell . cs $ d.primaryProposalId
  -- tcol cell (hd "ExperimentDescription") $ \d -> cell . cs . show $ d.experimentDescription

  hd :: View () () -> View Head ()
  hd = th (bold . pad 4 . bord . bg Light)

  cell :: Text -> View Dataset ()
  cell = td (pad 4 . bord) . text

  bord = border 1 . borderColor GrayLight

-- liveButton Collapse (bg Primary . color White) "Collapse"
