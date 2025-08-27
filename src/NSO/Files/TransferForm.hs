module NSO.Files.TransferForm where

import NSO.Files.RemoteFolder
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import Web.FormUrlEncoded qualified as FUE
import Web.Hyperbole


data User


-- Transfer Forms -----------------------------------------------------------------------

-- this is a globus thing!
data TransferForm = TransferForm
  { label :: Text
  , path :: Path User Dir ()
  , endpoint_id :: Text
  }
  deriving (Generic, FromForm)
instance Show TransferForm where
  show tf = "TransferForm " <> unwords [show tf.label, show tf.path, show tf.endpoint_id]


data DownloadFolder = DownloadFolder
  { folder :: Maybe (Path User Dir ())
  }
  deriving (Generic)
instance FromForm DownloadFolder where
  fromForm f = do
    DownloadFolder <$> FUE.parseMaybe "folder[0]" f


transferFormFolder :: TransferForm -> DownloadFolder -> Path User Dir TransferForm
transferFormFolder tform df =
  let Path fp = tform.path
   in -- If they didn't select a folder, use the current folder
      case df.folder of
        Just f -> tform.path </> f
        Nothing -> Path fp


remoteTransfer :: TransferForm -> RemoteFolder User a
remoteTransfer tform =
  let Path p = tform.path
   in RemoteFolder (Tagged tform.endpoint_id) (Path p)


remoteDatasets :: TransferForm -> DownloadFolder -> RemoteFolder User Dataset
remoteDatasets tform df =
  let Path dest = transferFormFolder tform df
   in RemoteFolder (Tagged tform.endpoint_id) $ Path dest
