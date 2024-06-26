module App.View.DatasetsTable where

-- import App.Colors
import App.Route as Route
import App.Style qualified as Style
import App.View.Common (code, showDate, showTimestamp)
import App.View.DataRow qualified as View
import App.View.Icons as Icons
import Data.Ord (Down (..))
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets as Datasets
import NSO.Data.Qualify (boxRadius)
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Numeric (showFFloat)
import Web.Hyperbole


data ProgramDatasets = ProgramDatasets (Id InstrumentProgram)
  deriving (Show, Read, ViewId)


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
  deriving (Show, Read, ViewAction)


instance HyperView ProgramDatasets where
  type Action ProgramDatasets = SortField


actionSort :: (Datasets :> es) => ProgramDatasets -> SortField -> Eff es (View ProgramDatasets ())
actionSort (ProgramDatasets i) s = do
  ds <- send $ Query (ByProgram i)
  pure $ datasetsTable s ds


datasetsTable :: SortField -> [Dataset] -> View ProgramDatasets ()
datasetsTable s ds = do
  let sorted = sortField s ds

  -- is there a way to do alternating rows here?
  table View.table sorted $ do
    tcol (hd $ sortBtn ByLatest "Latest") $ \d -> cell $ datasetLatest d.latest
    tcol (hd $ sortBtn DatasetId "Id") $ \d -> cell $ route (Route.Dataset d.datasetId) Style.link $ text . cs $ d.datasetId.fromId
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

  sortBtn :: SortField -> Text -> View ProgramDatasets ()
  sortBtn st t =
    button st Style.link (text t)

  hd = View.hd
  cell = View.cell

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


datasetLatest :: Bool -> View c ()
datasetLatest False = none
datasetLatest True = row id $ do
  space
  el (width 24 . height 24) Icons.checkCircle
  space


radiusBoundingBox :: Maybe BoundingBox -> View c ()
radiusBoundingBox Nothing = none
radiusBoundingBox (Just b) = row (gap 5) $ do
  space
  forM_ (boundingPoints b) $ \c ->
    code . cs $ showFFloat (Just 0) (boxRadius c) ""
  space

-- rowHeight :: PxRem
-- rowHeight = 30
