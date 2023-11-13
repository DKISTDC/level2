-- {-# OPTIONS_GHC -ddump-splices #-}

module NSO.Metadata where

import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Morpheus.Client hiding (fetch)
import Data.Morpheus.Client.CodeGen.Internal (OperationType (..))
import Data.String.Interpolate
import Effectful.Request
import GHC.Generics
import NSO.Metadata.Types
import NSO.Prelude
import Network.HTTP.Req

-- NOTE: These fields may be used to identify data

-- """Time of acquisition for the earliest frame in the data set"""
-- startTime: DateTime
--
-- """Time of acquisition for the latest frame in the data set"""
-- endTime: DateTime
--
-- """Id of the parameters section of the input dataset"""
-- inputDatasetParametersPartId: Int
--
-- """Id of the observe frames section of the input dataset"""
-- inputDatasetObserveFramesPartId: Int
--
-- """Id of the calibration frames section of the input dataset"""
-- inputDatasetCalibrationFramesPartId: Int
--
-- """Number of frames in the data set when it was created"""
-- originalFrameCount: Int
--
-- """The experiment id the observation data was collected under"""
-- primaryExperimentId: String
--
-- """The proposal id the observation data was collected under"""
-- primaryProposalId: String
--
-- """
-- The mode length of time that the CCD was exposed for within the dataset
-- """
-- exposureTime: Float

newtype AllDatasets = AllDatasets {datasetInventories :: [DatasetInventory]}
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON)

instance RequestType AllDatasets where
  type RequestArgs AllDatasets = ()
  __name _ = "AllDatasets"
  __query _ =
    let fields = genQueryFields @DatasetInventory Proxy
     in [i| query AllDatasets { datasetInventories { #{fields} } } |]
  __type _ = OPERATION_QUERY

mockRequest :: Text -> ByteString -> IO ByteString
mockRequest "http://internal-api-gateway.service.prod.consul/graphql" _ =
  L.readFile "deps/datasets2.json"
mockRequest url _ = do
  error $ "URL Not Mocked: " <> show url

metadata :: Service
metadata = Service $ http "internal-api-gateway.service.prod.consul" /: "graphql"
