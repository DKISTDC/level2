module App.Page.Dataset where

import App.Colors
import App.Route
import App.View.Common (showTimestamp)
import App.View.DatasetsTable as DatasetsTable
import Data.Aeson (ToJSON, encode)
import Effectful.Rel8
import NSO.Data.Dataset as Dataset
import NSO.Prelude
import Web.Hyperbole
import Web.View

page :: (Hyperbole :> es, Rel8 :> es) => Id Dataset -> Page es ()
page di = load $ do
  ds <- Dataset.queryById di

  pure $ appLayout Experiments $ do
    col (pad 20 . gap 20) $ do
      el (fontSize 24 . bold) $ do
        text "Dataset: "
        text di.fromId

      mapM_ viewDataset ds

viewDataset :: Dataset -> View c ()
viewDataset d =
  col (bg White . gap 10 . pad 10) $ do
    field "Latest" $ DatasetsTable.latest d.latest
    field "Scan Date" $ text $ showTimestamp d.scanDate
    field "Instrument" $ text $ cs $ show d.instrument
    field "Instrument Program Id" $ link (Program d.instrumentProgramId) lnk $ text d.instrumentProgramId.fromId
    field "Experiment Id" $ link (Experiment d.primaryExperimentId) lnk $ text d.primaryExperimentId.fromId
    field "Proposal Id" $ text d.primaryProposalId.fromId
    field "Stokes Parameters" $ text $ cs $ show d.stokesParameters
    field "Create Date" $ text $ showTimestamp d.createDate
    field "Update Date" $ text $ showTimestamp d.updateDate
    field "Wavelength Min" $ text $ cs $ show d.wavelengthMin
    field "Wavelength Max" $ text $ cs $ show d.wavelengthMax
    field "Start Time" $ text $ showTimestamp d.startTime
    field "End Time" $ text $ showTimestamp d.endTime
    field "Frame Count" $ text $ cs $ show d.frameCount
    field "Exposure Time" $ text $ cs $ show d.exposureTime
    field "Bounding Box" $ boundingBox d.boundingBox
    field "AO Locked" $ text $ cs $ show d.aoLocked
    field "Health" $ json d.health
    field "GOS Status" $ json d.gosStatus
    field "Light Level" $ json d.lightLevel
    -- field "Polarimetric Accuracy" $ json d.polarimetricAccuracy
    field "friedParameter" $ json d.friedParameter
 where
  lnk = color Primary . hover (color PrimaryLight)

field :: Text -> View c () -> View c ()
field nm cnt =
  row (gap 10) $ do
    row (width 180) $ do
      space
      label bold (text nm)
    el_ cnt

boundingBox :: Maybe BoundingBox -> View c ()
boundingBox Nothing = none
boundingBox (Just b) = code $ cs $ show b

json :: (ToJSON a) => a -> View c ()
json a = code $ cs $ encode a

code :: Text -> View c ()
code = pre (fontSize 14)
