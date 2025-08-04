module NSO.InterserviceBus where


-- import Effectful
-- import Effectful.Dispatch.Dynamic
-- import Network.AMQP.Worker

-- data InterserviceBus :: Effect where
--   CatalogFrames :: Inversion -> InterserviceBus m []

--   CreateInversion :: Inversion -> MetadataInversions m [InversionInventory]
-- type instance DispatchOf InterserviceBus = 'Dynamic

-- https://nso.atlassian.net/wiki/spaces/DPD/pages/3670465/06+-+Interservice+Bus
-- catalog.frame.m
-- { "bucket": "<bucket name>", "objectName": "<object name>", "incrementDatasetCatalogReceiptCount": <True/False>, "conversationId" : "<generated\passed thru uuid>" }
--
-- catalog.object.m
-- { "bucket": "<bucket name>", "objectName": "<object name>", "objectType": "<object_type>", "incrementDatasetCatalogReceiptCount": <True/False>, "groupId": "<group_id>", "groupName": "<group_name>", "conversationId" : "<generated\passed thru uuid>" }
