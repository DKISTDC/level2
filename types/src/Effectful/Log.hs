{-# LANGUAGE LambdaCase #-}

module Effectful.Log where

import Data.Char (isAlpha)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString)
import Data.String.Conversions
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import System.Console.ANSI qualified as ANSI
import Prelude


data Log :: Effect where
  Log :: LogLevel -> String -> Log m ()
  Context :: String -> m a -> Log m a
  RowSet :: RowId -> String -> Log m ()
  RowDone :: RowId -> Log m ()
  Render :: Log m ()


data LogLevel
  = Debug
  | Info
  | Err


instance Show LogLevel where
  show Debug = "DEBUG"
  show Info = "INFO "
  show Err = "ERROR"


type instance DispatchOf Log = 'Dynamic


newtype ThreadName = ThreadName Text
  deriving (Show)
  deriving newtype (IsString)


runLogger
  :: (IOE :> es, Concurrent :> es, Reader LogRows :> es)
  => ThreadName
  -> Eff (Log : es) a
  -> Eff es a
runLogger (ThreadName tname) = reinterpret (runReader @(Maybe String) Nothing) $ \env -> \case
  Log lvl msg -> do
    mctx <- ask @(Maybe String)
    liftIO $ do
      now <- datetime <$> getCurrentTime
      let nm = padSpace 8 $ take 8 $ cs tname
      putStrLn [i|| #{nm} | #{now} | #{lvl} |#{messageContext mctx} #{msg} |]

    displayRows
  Context ctx m -> do
    localSeqUnlift env $ \unlift -> local (const $ Just ctx) (unlift m)
  RowSet rid s -> do
    _ <- modifyState $ \st -> st{rows = Map.insert rid s st.rows}
    displayRows
  RowDone rid -> do
    mr <- getRow rid
    _ <- modifyState $ \st -> st{rows = Map.delete rid st.rows}
    liftIO $ do
      putStr $ "● [" <> rid <> "] "
      case mr of
        Nothing -> putStrLn ""
        Just r -> putStrLn r
    displayRows
  Render -> do
    displayRows
 where
  messageContext Nothing = ""
  messageContext (Just ctx) = " [" <> ctx <> "]"

  datetime :: UTCTime -> String
  datetime =
    map noLetter . take 23 . iso8601Show

  noLetter c =
    if isAlpha c
      then ' '
      else c

  displayRows :: (Concurrent :> es, IOE :> es, Reader LogRows :> es) => Eff es ()
  displayRows = do
    st <- modifyState incrementCount
    liftIO $ do
      ANSI.clearFromCursorToLineEnd
      putStrLn ""
      ANSI.clearFromCursorToLineEnd
      mapM_ (displayRow st) $ Map.toList st.rows
      ANSI.cursorUp (length st.rows + 1)

  displayRow :: RowState -> (RowId, String) -> IO ()
  displayRow st (rid, r) = do
    let anim = animation st.count
    putStrLn $ anim : ' ' : " [" <> rid <> "] " <> r
    ANSI.clearFromCursorToLineEnd

  animation :: Int -> Char
  animation n =
    -- ◢◣◤◥
    -- ◐◓◑◒
    "◐◓◑◒" !! (n `mod` 4)

  getRow :: (Concurrent :> es, Reader LogRows :> es) => RowId -> Eff es (Maybe String)
  getRow rid = do
    rows <- ask @LogRows
    st <- atomically $ readTMVar rows
    pure $ Map.lookup rid st.rows

  modifyState :: (Concurrent :> es, Reader LogRows :> es) => (RowState -> RowState) -> Eff es RowState
  modifyState f = do
    rows <- ask
    atomically $ do
      st :: RowState <- readTMVar rows
      let st' = f st
      writeTMVar rows st'
      pure st'

  incrementCount :: RowState -> RowState
  incrementCount st = st{count = st.count + 1}


type LogRows = TMVar RowState
type RowId = String


data RowState = RowState
  { count :: Int
  , rows :: Map RowId String
  }


initRows :: (Concurrent :> es) => Eff es LogRows
initRows = newTMVarIO $ RowState 0 mempty


dump :: (Show a) => String -> a -> String
dump n a = n <> ": " <> show a


log :: (Log :> es) => LogLevel -> String -> Eff es ()
log ll = send . Log ll


type LogContext = String


logContext :: (Log :> es) => String -> Eff es a -> Eff es a
logContext ctx eff = send $ Context ctx eff


-- debug :: (Log :> es) => String ->
-- debug = send . Log Debug

-- delay :: (Debug :> es) => Milliseconds -> Eff es ()
-- delay = send . Delay

padSpace :: Int -> String -> String
padSpace n s =
  s <> replicate (n - length s) ' '
