module App.Mock where

import App.Config
import App.Error

import NSO.Prelude

-- import App.Page.Dashboard qualified as Dashboard
-- import App.Page.Dataset qualified as Dataset
-- import App.Page.Experiment qualified as Experiment
-- import App.Page.Experiments qualified as Experiments
-- import App.Page.Program qualified as Program
-- import App.Page.Scan qualified as Scan
-- import Effectful
-- import Effectful.Debug
-- import Effectful.Dispatch.Dynamic
-- import Effectful.Error.Static
-- import Effectful.GenRandom
-- import Effectful.Reader.Static
-- import Effectful.State.Static.Local
-- import Effectful.Time

import NSO.Data.Datasets
import NSO.Data.Scan
import NSO.DataStore.Datasets as Datasets
import NSO.DataStore.Inversions as Inversions
import NSO.Metadata as Metadata
import NSO.Types.Common

import NSO.Types.InstrumentProgram

-- import NSO.Types.Status
import NSO.Types.Wavelength
import Network.Wai.Handler.Warp as Warp (run)
import Web.Hyperbole


-- import App.Route as Route

--
--
-- runMockDatasets
--   :: (IOE :> es)
--   => [Dataset]
--   -> Eff (Datasets : es) a
--   -> Eff es a
-- runMockDatasets ds = reinterpret (evalState ds) $ \_ -> \case
--   Query Latest -> pure latest
--   Query (ByExperiment eid) -> byExperiment eid <$> get
--   Query (Datasets.ByProgram pid) -> byProgram pid <$> get
--   Query (ById did) -> byId did <$> get
--   Datasets.Create d -> modify (create d)
--   Modify SetOld ids -> modify (update setOld ids)
--  where
--   latest = filter (\d -> d.latest) ds
--   byExperiment eid = filter (\d -> d.primaryExperimentId == eid)
--   byProgram pid = filter (\d -> d.instrumentProgramId == pid)
--   byId did = filter (\d -> d.datasetId == did)
--   create ds' = (<> ds')
--   setOld d = d{latest = False}
--   update f ids = map (updateOne f ids)
--   updateOne f ids d =
--     if d.datasetId `elem` ids
--       then f d
--       else d
--
--
-- runMockInversions
--   :: (IOE :> es, Time :> es, GenRandom :> es)
--   => [Inversion]
--   -> Eff (Inversions : es) a
--   -> Eff es a
-- runMockInversions ds = reinterpret (evalState ds) $ \_ -> \case
--   Inversions.All -> AllInversions <$> get
--   Inversions.ByProgram pid -> byProgram pid <$> get
--   Inversions.Create pid -> create pid
--  where
--   byProgram pid = filter (\d -> d.programId == pid)
--   create pid = do
--     i <- Inversions.empty pid
--     modify (i :)
--     pure i
--
--
-- mockDatasets :: Services -> IO [Dataset]
-- mockDatasets services = do
--   res <- runAll $ do
--     scanDatasetInventory services.metadata
--   case res of
--     Left e -> fail $ show e
--     Right ds -> pure ds
--  where
--   runAll =
--     runEff
--       . runErrorNoCallStack @RequestError
--       . runRequestMock Metadata.mockRequest
--       . runTime
--       . runGraphQL

mockApp :: IO ()
mockApp = do
  putStrLn "MOCK APP"

--  (services, _) <- initServices
--  ds <- mockDatasets services
--  Warp.run 3000 $ application document (runApp services ds . router)
-- where
--  router :: (Hyperbole :> es, Datasets :> es, Inversions :> es, Time :> es, GenRandom :> es, Reader Services :> es, GraphQL :> es, Error RequestError :> es) => AppRoute -> Eff es ()
--  router Dashboard = page Dashboard.page
--  router Experiments = page Experiments.page
--  router (Experiment eid) = page $ Experiment.page eid
--  router (Program pid) = page $ Program.page pid
--  router (Route.Dataset di) = page $ Dataset.page di
--  router Scan = page Scan.page
--
--  runApp services ds =
--    runTime
--      . runErrorNoCallStackWith @RequestError onRequestError
--      . runErrorNoCallStackWith @AppError onAppError
--      . runReader services
--      . runGenRandom
--      . runMockDatasets ds
--      . runMockInversions []
--      . runRequestMock Metadata.mockRequest
--      . runDebugIO
--      . runGraphQL
--
--  onRequestError = print
--  onAppError = print
