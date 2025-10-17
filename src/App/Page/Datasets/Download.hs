module App.Page.Datasets.Download where

import App.Effect.Transfer
import App.Effect.Transfer qualified as Transfer
import App.Route as Route
import Effectful
import Effectful.Log hiding (Info)
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs hiding (programInversions)
import NSO.Files
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (Proposal)
import Network.Globus (Task)
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData (fromQueryData)
import Web.Hyperbole.Data.URI (Query, queryString)


data ActiveDownload = ActiveDownload
  { downloadTaskId :: Maybe (Id Task)
  }
  deriving (Generic, ToQuery, FromQuery)


submitDatasetDownload :: (Hyperbole :> es, Log :> es, Datasets :> es, Inversions :> es, Transfer :> es) => Id Proposal -> Id InstrumentProgram -> Eff es Response
submitDatasetDownload propId progId = do
  log Debug $ dump "Submit Download" (propId, progId)
  tfrm <- formData @TransferForm
  tfls <- formData @DownloadFolder
  ds <- Datasets.find (Datasets.ByProgram progId)
  taskId <- Transfer.userDownloadDatasets tfrm tfls ds
  let dwn = ActiveDownload (Just taskId)
  redirect $ activeDownloadQuery dwn $ routeUri (Route.Proposal propId $ Route.Program progId Route.Prog)
 where
  setUrlQuery :: Query -> URI -> URI
  setUrlQuery q URI{uriAuthority, uriScheme, uriPath} =
    URI{uriScheme, uriAuthority, uriPath, uriQuery = queryString q, uriFragment = ""}

  activeDownloadQuery :: ActiveDownload -> URI -> URI
  activeDownloadQuery ad =
    setUrlQuery (fromQueryData $ toQuery ad)
