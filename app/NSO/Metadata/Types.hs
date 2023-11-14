{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module NSO.Metadata.Types where

import Data.Aeson (FromJSON)
import Data.List qualified as L
import Data.Morpheus.Client hiding (fetch)
import Data.Time.Format (FormatTime)
import Data.Time.Format.ISO8601
import GHC.Generics
import GHC.TypeLits
import NSO.Data.Dataset
import NSO.Prelude

newtype DateTime = DateTime {utc :: UTCTime}
  deriving (Show, Eq, Generic)
  deriving newtype (ISO8601, FormatTime)

instance EncodeScalar DateTime where
  encodeScalar (DateTime x) = String $ cs $ iso8601Show x

instance DecodeScalar DateTime where
  -- dates do not have the UTC suffix
  decodeScalar (String s) = iso8601ParseM $ cs $ s <> "Z"
  decodeScalar _ = Left "Cannot decode DateTime"

newtype JSONString = JSONString Text
  deriving (Show, Eq, Generic)
  deriving newtype (EncodeScalar, DecodeScalar)

-- | this only defines a few basic types
declareGlobalTypes "deps/metadata.graphql"

data DatasetInventory = DatasetInventory
  -- { asdfObjectKey :: Text
  -- , averageDatasetSpatialSampling :: Double
  -- , averageDatasetSpectralSampling :: Double
  -- , averageDatasetTemporalSampling :: Double
  { boundingBox :: BoundingBox
  , -- , browseMovieObjectKey :: Text
    -- , browseMovieUrl :: Text
    -- , bucket :: Text
    -- calibrationDocumentationUrl :: Text
    -- , contributingExperimentIds :: [Text]
    -- contributingProposalIds :: [Text]
    createDate :: DateTime
  , datasetId :: Text
  , -- , datasetInventoryId :: Int
    -- , datasetSize :: Int
    endTime :: DateTime
  , experimentDescription :: Text
  , exposureTime :: Double
  , frameCount :: Int
  , -- , hasAllStokes :: Bool
    -- , hasSpectralAxis :: Bool
    -- , hasTemporalAxis :: Bool
    -- , headerDataUnitCreationDate :: DateTime
    -- , headerDocumentationUrl :: Text
    -- , headerVersion :: Text
    -- , highLevelSoftwareVersion :: Text
    -- , infoUrl :: Text
    -- , inputDatasetCalibrationFramesPartId :: Int
    -- inputDatasetObserveFramesPartId :: Int
    -- , inputDatasetParametersPartId :: Int
    instrumentName :: Text
  , instrumentProgramExecutionId :: Text
  , -- , isActive :: Bool
    -- , isEmbargoed :: Bool
    observingProgramExecutionId :: Text
  , -- , originalFrameCount :: Int
    primaryExperimentId :: Text
  , primaryProposalId :: Text
  , -- , qualityAverageFriedParameter :: Double
    -- , qualityAveragePolarimetricAccuracy :: Double
    -- , qualityReportObjectKey :: Text
    -- , recipeId :: Int
    -- , recipeInstanceId :: Int
    -- , recipeRunId :: Int
    startTime :: DateTime
  , stokesParameters :: StokesParameters
  , -- , targetTypes :: [Text]
    updateDate :: DateTime
  , wavelengthMax :: Double
  , wavelengthMin :: Double
  , -- , workflowName :: Text
    -- , workflowVersion :: Text
    health :: Health
  , gosStatus :: GOSStatus
  , aoLocked :: Int
  , -- , polarimetricAccuracy :: Distribution
    lightLevel :: Distribution
  , friedParameter :: Distribution
  }
  deriving (Generic, Show, Eq, FromJSON)

data NestedFields = NestedFields String [NestedFields]

-- | Return the field names for a given object by proxy
class FieldNames f where
  fieldNames :: Proxy f -> [NestedFields]

instance (FieldNames f, FieldNames g) => FieldNames (f :*: g) where
  fieldNames _ = fieldNames (Proxy :: Proxy f) ++ fieldNames (Proxy :: Proxy g)

instance (FieldNames f, FieldNames g) => FieldNames (f :+: g) where
  fieldNames _ = fieldNames (Proxy :: Proxy f) ++ fieldNames (Proxy :: Proxy g)

instance (KnownSymbol name, FieldNames f) => FieldNames (M1 S ('MetaSel ('Just name) _1 _2 _3) f) where
  fieldNames _ = [NestedFields (symbolVal (Proxy :: Proxy name)) []]

-- instance (KnownSymbol name, FieldNames f) => FieldNames (M1 S ('MetaSel ('Just name) _1 _2 _3) f) where
--   fieldNames _ = [NestedFields (symbolVal (Proxy :: Proxy name)) (fieldNames @f Proxy)]

instance (FieldNames f) => FieldNames (M1 D meta f) where
  fieldNames _ = fieldNames (Proxy :: Proxy f)

instance (FieldNames f) => FieldNames (M1 C meta f) where
  fieldNames _ = fieldNames (Proxy :: Proxy f)

instance FieldNames U1 where
  fieldNames _ = []

instance FieldNames (K1 R f) where
  fieldNames _ = []

-- class DataName f where
--   dataName :: Proxy f -> String
--
-- instance (Datatype d) => DataName (M1 D d f) where
--   dataName _ = datatypeName (undefined :: M1 D d f a)

-- recordFields :: forall a. (Generic a, FieldNames (Rep a)) => Proxy a -> [String]
-- recordFields _ = fmap fst $ fieldNames @(Rep a) Proxy

genQueryFields :: forall a. (Generic a, FieldNames (Rep a)) => Proxy a -> String
genQueryFields _ = allFields $ fieldNames @(Rep a) Proxy
 where
  allFields :: [NestedFields] -> String
  allFields = L.intercalate "\n" . fmap fields

  fields :: NestedFields -> String
  fields (NestedFields nm []) = nm
  fields (NestedFields nm fs) = nm ++ "{" ++ allFields fs ++ "}"
