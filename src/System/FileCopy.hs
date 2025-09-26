module System.FileCopy where

import NSO.Prelude
import System.Directory
import System.FilePath


copyRecursive :: (MonadIO m) => FilePath -> FilePath -> m (Either String ())
copyRecursive src dest = liftIO $ do
  -- putStrLn $ "COPY RECURSE \n  " <> src <> " => \n  " <> dest
  createDirectoryIfMissing True $ takeDirectory dest
  isFile <- doesFileExist src
  isDir <- doesDirectoryExist src
  case (isFile, isDir) of
    (True, _) -> do
      -- putStrLn "  -single file"
      copyFile src dest
      pure $ Right ()
    (_, True) -> do
      -- putStrLn "  -dir"
      createDirectoryIfMissing True dest
      files <- listDirectory src
      forM_ files $ \file -> do
        copyRecursive (src </> file) (dest </> file)
      pure $ Right ()
    _ -> do
      return $ Left $ "ERROR: src does not exist: " <> src
