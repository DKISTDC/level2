module App.View.DatasetsTable where

import App.Colors
import App.View.Common (showTimestamp)
import App.View.Icons as Icons
import NSO.Data.Dataset
import NSO.Prelude
import Numeric (showFFloat)
import Web.UI
import Web.UI.Types

rowHeight :: PxRem
rowHeight = 30

datasetsTable :: [Dataset] -> View c ()
datasetsTable ds = do
  let sorted = ds

  -- is there a way to do alternating rows here?
  table (odd (bg White) . even (bg Light)) sorted $ do
    tcol (hd "Id") $ \d -> cell $ text . cs $ d.datasetId.fromId
    tcol (hd "Latest") $ \d -> cell $ latest d.latest
    tcol (hd "Create Date") $ \d -> cell $ text . cs . showTimestamp $ d.createDate
    tcol (hd "Start Time") $ \d -> cell $ text . cs . showTimestamp $ d.startTime
    tcol (hd "Instrument") $ \d -> cell $ text . cs . show $ d.instrument
    tcol (hd "Stokes") $ \d -> cell $ text . cs . show $ d.stokesParameters
    tcol (hd "Wave Min") $ \d -> cell $ text . cs $ showFFloat (Just 1) d.wavelengthMin ""
    tcol (hd "Wave Max") $ \d -> cell $ text . cs $ showFFloat (Just 1) d.wavelengthMax ""
    tcol (hd "Bounding Box") $ \d -> cell $ text . cs $ maybe "" show d.boundingBox
    -- tcol (hd "Exposure Time") $ \d -> cell . cs . show $ d.exposureTime
    -- tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
    tcol (hd "Frame Count") $ \d -> cell $ text . cs . show $ d.frameCount
 where
  -- tcol (hd "On Disk") $ \d -> cell . cs . show $ isOnDisk d.boundingBox

  -- tcol cell (hd "End Time") $ \d -> cell . cs . show $ d.endTime
  -- tcol cell (hd "peid") $ \d -> cell . cs $ d.primaryExperimentId
  -- tcol cell (hd "ppid") $ \d -> cell . cs $ d.primaryProposalId
  -- tcol cell (hd "ExperimentDescription") $ \d -> cell . cs . show $ d.experimentDescription

  hd :: View () () -> View Head ()
  hd = th (pad 4 . bord . bold . bg Light)

  cell :: View () () -> View Dataset ()
  cell = td (pad 4 . bord)

  bord = border 1 . borderColor GrayLight

  latest False = none
  latest True = do
    el (width 24 . height 24) Icons.checkCircle
