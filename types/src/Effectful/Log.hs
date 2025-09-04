{-# LANGUAGE LambdaCase #-}

module Effectful.Log where

import Data.Char (isAlpha)
import Data.String (IsString)
import Data.String.Conversions
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Prelude


data Log :: Effect where
  Log :: LogLevel -> String -> Log m ()
  Context :: String -> m a -> Log m a


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
  :: (IOE :> es)
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
      pure ()
  Context ctx m -> do
    localSeqUnlift env $ \unlift -> local (const $ Just ctx) (unlift m)
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
