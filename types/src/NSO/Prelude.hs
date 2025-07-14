module NSO.Prelude
  ( module Prelude
  , (&)
  , (<|>)
  , cs
  , module Control.Monad.IO.Class
  , module Data.Maybe
  , module Data.String
  , identity
  , Generic
  , Map
  , Text
  , UTCTime
  , LocalTime
  , Tagged (..)
  , Type

    -- * List functions
  , module Data.List
  , module Data.List.NonEmpty
  , module Data.Ord
  , module Data.Proxy

    -- * Monadic functions
  , module Control.Monad

    -- * Effects
  , Eff
  , (:>)
  , send

    -- * Lifted IO
  , putStrLn
  , print
  , putStr
  , Identity
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, guard, unless, when, zipWithM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.List (find, group, groupBy, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), head, last, nonEmpty, reverse, sort, sortWith, (!!))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime)
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics (Generic)
import Prelude hiding (Real, even, head, last, log, odd, print, putStr, putStrLn, readFile, reverse, truncate, writeFile, (!!))
import Prelude qualified


identity :: a -> a
identity x = x


print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print


putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . Prelude.putStrLn


putStr :: (MonadIO m) => String -> m ()
putStr = liftIO . Prelude.putStr
