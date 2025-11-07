module App.Page.Dataset where

import App.Colors
import App.Effect.Auth
import App.Route
import App.Style qualified as Style
import App.View.Common (showDate, showTimestamp)
import App.View.Datasets (radiusBoundingBox)
import App.View.Layout
import Data.Aeson (encode)
import Data.Ord (Down (..))
import Data.Text qualified as T
import NSO.Data.Datasets as Datasets
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Wavelength (spectralLineName)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI (uriToText)


page :: (Hyperbole :> es, Datasets :> es, Auth :> es) => Id Dataset -> Page es '[]
page di = do
  ds <- Datasets.find (ByIds [di])

  let sorted = sortOn (Down . (.scanDate)) ds

  appLayout Proposals $ do
    col ~ Style.page $ do
      el ~ Style.header $ do
        text "Dataset: "
        text di.fromId

      mapM_ viewDataset sorted


viewDataset :: Dataset -> View c ()
viewDataset d =
  col ~ Style.card $ do
    el ~ Style.cardHeader Secondary . bold $ "Dataset Details"
    col ~ gap 10 . pad 10 $ do
      dataField "Scan Date" $ text $ showTimestamp d.scanDate
      dataField "Embargo" $ text $ cs $ maybe "-" showDate d.embargo
      dataField "Instrument" $ text $ cs $ show d.instrument
      dataField "Instrument Program Id" $ appRoute (Proposal d.primaryProposalId $ Program d.instrumentProgramId Prog) ~ Style.link $ text d.instrumentProgramId.fromId
      dataField "Proposal Id" $ appRoute (Proposal d.primaryProposalId PropRoot) ~ Style.link $ text d.primaryProposalId.fromId
      dataField "Experiment Id" $ text d.primaryExperimentId.fromId
      dataField "bucket" $ text d.bucket.bucketName
      dataField "Stokes Parameters" $ text $ cs $ show d.stokesParameters
      dataField "Create Date" $ text $ showTimestamp d.createDate
      dataField "Update Date" $ text $ showTimestamp d.updateDate
      dataField "Spectral Lines" $ text $ T.intercalate "," $ fmap spectralLineName d.spectralLines
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
      tag "iframe" @ src embedMovieUrl . att "frameborder" "0" . att "allow" "autoplay; fullscreen" . att "allowfullscreen" "" . att "width" "800" . att "height" "600" $ none
 where
  -- https://vimeo.com/1114181863
  -- https://player.vimeo.com/video/**yourvideonumberhere**
  embedMovieUrl = T.replace "vimeo.com" "player.vimeo.com/video" $ uriToText d.browseMovieUrl.uri


dataField :: Text -> View c () -> View c ()
dataField nm cnt =
  row ~ gap 10 $ do
    row ~ width 180 $ do
      space
      el ~ bold $ text nm
    el cnt


boundingBox :: Maybe BoundingBox -> View c ()
boundingBox Nothing = none
boundingBox (Just b) = code ~ Style.code $ cs $ show b


json :: (ToJSON a) => a -> View c ()
json a = code ~ Style.code $ cs $ encode a
