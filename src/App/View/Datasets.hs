{-# LANGUAGE UndecidableInstances #-}

module App.View.Datasets where

-- import App.Colors

import App.Colors
import App.Route as Route
import App.Style qualified as Style
import App.View.Common (showDate, showTimestamp)
import App.View.DataRow qualified as View
import Data.Ord (Down (..))
import NSO.Data.Datasets as Datasets
import NSO.Data.Qualify (boxRadius)
import NSO.Prelude
import NSO.Types.Common
import Numeric (showFFloat)
import Web.Atomic.CSS
import Web.Atomic.Types.ClassName (className)
import Web.Atomic.Types.Style as Atomic (ToStyle (style))
import Web.Hyperbole


data SortField
  = DatasetId
  | ByLatest
  | CreateDate
  | UpdateDate
  | StartTime
  | Instrument
  | Stokes
  | WaveMin
  | WaveMax
  deriving (Show, Read, Generic, ToParam, FromParam)


datasetsTable :: forall id. (ViewAction (Action id)) => (SortField -> Action id) -> SortField -> [Dataset] -> View id ()
datasetsTable sortBy srt ds = do
  let sorted = sortField srt ds
  datasetsTableUnsorted sortBy sorted
 where
  sortField :: SortField -> ([Dataset] -> [Dataset])
  sortField DatasetId = sortOn (.datasetId)
  sortField ByLatest = sortOn (Down . (.scanDate))
  sortField CreateDate = sortOn (Down . (.createDate))
  sortField UpdateDate = sortOn (Down . (.updateDate))
  sortField StartTime = sortOn (Down . (.startTime))
  sortField Instrument = sortOn (.instrument)
  sortField Stokes = sortOn (.stokesParameters)
  sortField WaveMin = sortOn (.wavelengthMin)
  sortField WaveMax = sortOn (.wavelengthMax)


datasetsTableUnsorted :: forall id. (ViewAction (Action id)) => (SortField -> Action id) -> [Dataset] -> View id ()
datasetsTableUnsorted sortBy ds = do
  -- is there a way to do alternating rows here?
  table ds ~ View.table $ do
    tcol (hd $ sortBtn DatasetId "Id") $ \d -> cell $ appRoute (Route.Datasets $ Route.Dataset d.datasetId) ~ Style.link $ text . cs $ d.datasetId.fromId
    tcol (hd $ sortBtn CreateDate "Create Date") $ \d -> cell $ text . cs . showTimestamp $ d.createDate
    tcol (hd $ sortBtn StartTime "Start Time") $ \d -> cell $ text . cs . showTimestamp $ d.startTime
    tcol (hd "Embargo") $ \d -> cell $ text $ embargo d
    tcol (hd $ sortBtn Instrument "Instrument") $ \d -> cell $ text . cs . show $ d.instrument
    tcol (hd $ sortBtn Stokes "Stokes") $ \d -> cell $ text . cs . show $ d.stokesParameters
    tcol (hd $ sortBtn WaveMin "Wave Min") $ \d -> cell $ text . cs $ showFFloat (Just 1) d.wavelengthMin ""
    tcol (hd $ sortBtn WaveMax "Wave Max") $ \d -> cell $ text . cs $ showFFloat (Just 1) d.wavelengthMax ""
    -- tcol (hd "Bounding Box") $ \d -> cell $ text . cs $ maybe "" show d.boundingBox
    tcol (hd "Bounding Box Radius") $ \d -> cell $ radiusBoundingBox d.boundingBox
    -- tcol (hd "Exposure Time") $ \d -> cell . cs . show $ d.exposureTime
    -- tcol (hd "Frame Count") $ \d -> cell . cs . show $ d.frameCount
    tcol (hd "Frame Count") $ \d -> cell $ text . cs . show $ d.frameCount
    tcol (hd "Health Good") $ \d -> cell $ text . cs . show $ fromMaybe 0 d.health.good
    tcol (hd "GOS Open") $ \d -> cell $ text . cs . show $ fromMaybe 0 d.gosStatus.open
    tcol (hd "AOLocked") $ \d -> cell $ text . cs . show $ d.aoLocked
 where
  -- tcol (hd "R0 Fried") $ \d -> cell $ text $ textFriedParameter d

  -- tcol (hd "On Disk") $ \d -> cell . cs . show $ isOnDisk d.boundingBox

  -- tcol cell (hd "End Time") $ \d -> cell . cs . show $ d.endTime
  -- tcol cell (hd "peid") $ \d -> cell . cs $ d.primaryExperimentId
  -- tcol cell (hd "ppid") $ \d -> cell . cs $ d.primaryProposalId
  -- tcol cell (hd "ExperimentDescription") $ \d -> cell . cs . show $ d.experimentDescription

  embargo :: Dataset -> Text
  embargo d =
    case d.embargo of
      (Just utc) -> showDate utc
      Nothing -> "-"

  sortBtn :: SortField -> Text -> View id ()
  sortBtn st t =
    button (sortBy st) ~ Style.link $ text t

  hd = View.hd
  cell = View.cell


radiusBoundingBox :: Maybe BoundingBox -> View c ()
radiusBoundingBox Nothing = none
radiusBoundingBox (Just b) = row ~ gap 5 $ do
  space
  forM_ (boundingPoints b) $ \c ->
    code (cs $ showFFloat (Just 0) (boxRadius c) "") ~ Style.code
  space


-- rowHeight :: PxRem
-- rowHeight = 30

textFriedParameter :: Dataset -> Text
textFriedParameter d =
  cs $ maybe "" ((\f -> showFFloat (Just 1) f "") . (* 100) . (.med)) d.friedParameter


-- the span is: 0 ... min .... max
boxPlot :: (Float -> String) -> Float -> Distribution -> View c ()
boxPlot val mx d =
  el ~ height 40 $ do
    row ~ bg (light Light) . height 24 . width 1000 . gap 0 $ do
      area 0 d.min
      area d.min d.p25 ~ bg Gray
      area d.p25 d.med ~ bg (light Primary)
      area d.med d.p75 ~ bg (light Primary)
      area d.p75 d.max ~ bg Gray
 where
  -- area d.max mx

  -- el ~ off 0 $ text $ cs $ val d.min
  -- el ~ off (d.p25 / d.max) $ text $ cs $ val d.p25
  -- el ~ off (d.med / d.max) $ text $ cs $ val d.med
  -- el ~ off (d.p75 / d.max) $ text $ cs $ val d.p75
  -- el ~ off (d.max) $ text $ cs $ val d.max

  area p n = do
    let w = (n - p) / mx
    el ~ flexWidth w . border (R 1) . pad (R 1) . position Relative $ do
      el ~ textAlign AlignRight . position Absolute . top 20 . right (-13) $ text $ cs $ val n

  flexWidth w =
    utility
      ("fw-" <> className (cs (showFFloat (Just 3) w "")))
      [ "flex-basis" :. Atomic.style (Pct w)
      , "flex-shrink" :. "0"
      , "flex-grow" :. "0"
      ]
