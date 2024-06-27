module App.Page.Dataset where

import App.Colors
import App.Globus
import App.Route
import App.Style qualified as Style
import App.View.Common (showDate, showTimestamp)
import App.View.DatasetsTable (datasetLatest, radiusBoundingBox)
import App.View.Layout
import Data.Aeson (ToJSON, encode)
import Data.Ord (Down (..))
import Effectful.Dispatch.Dynamic
import NSO.Data.Datasets as Datasets
import NSO.Prelude
import Web.Hyperbole


page :: (Hyperbole :> es, Datasets :> es, Auth :> es) => Id Dataset -> Page es Response
page di = load $ do
  ds <- send $ Datasets.Query (ById di)

  let sorted = sortOn (Down . (.scanDate)) ds

  appLayout Proposals $ do
    col Style.page $ do
      el Style.header $ do
        text "Dataset: "
        text di.fromId

      mapM_ viewDataset sorted


viewDataset :: Dataset -> View c ()
viewDataset d =
  col Style.card $ do
    el (Style.cardHeader Secondary . bold) "Dataset Details"
    col (gap 10 . pad 10) $ do
      dataField "Latest" $ datasetLatest d.latest
      dataField "Scan Date" $ text $ showTimestamp d.scanDate
      dataField "Embargo" $ text $ cs $ maybe "-" showDate d.embargo
      dataField "Instrument" $ text $ cs $ show d.instrument
      dataField "Instrument Program Id" $ route (Proposal d.primaryProposalId $ Program d.instrumentProgramId) Style.link $ text d.instrumentProgramId.fromId
      dataField "Proposal Id" $ route (Proposal d.primaryProposalId PropRoot) Style.link $ text d.primaryProposalId.fromId
      dataField "Experiment Id" $ text d.primaryExperimentId.fromId
      dataField "Stokes Parameters" $ text $ cs $ show d.stokesParameters
      dataField "Create Date" $ text $ showTimestamp d.createDate
      dataField "Update Date" $ text $ showTimestamp d.updateDate
      dataField "Wavelength Min" $ text $ cs $ show d.wavelengthMin
      dataField "Wavelength Max" $ text $ cs $ show d.wavelengthMax
      dataField "Start Time" $ text $ showTimestamp d.startTime
      dataField "End Time" $ text $ showTimestamp d.endTime
      dataField "Frame Count" $ text $ cs $ show d.frameCount
      dataField "Exposure Time" $ text $ cs $ show d.exposureTime
      dataField "Bounding Box" $ boundingBox d.boundingBox
      dataField "Bounding Box Radius" $ radiusBoundingBox d.boundingBox
      dataField "AO Locked" $ text $ cs $ show d.aoLocked
      dataField "Health" $ json d.health
      dataField "GOS Status" $ json d.gosStatus
      dataField "Light Level" $ json d.lightLevel
      -- dataField "Polarimetric Accuracy" $ json d.polarimetricAccuracy
      dataField "friedParameter" $ json d.friedParameter


dataField :: Text -> View c () -> View c ()
dataField nm cnt =
  row (gap 10) $ do
    row (width 180) $ do
      space
      el bold (text nm)
    el_ cnt


boundingBox :: Maybe BoundingBox -> View c ()
boundingBox Nothing = none
boundingBox (Just b) = code $ cs $ show b


json :: (ToJSON a) => a -> View c ()
json a = code $ cs $ encode a


code :: Text -> View c ()
code = pre (fontSize 14)
