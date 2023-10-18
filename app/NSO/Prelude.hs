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
import Data.List (find, group, groupBy, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), head, last, nonEmpty, reverse, sort, sortBy, (!!))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import GHC.Generics (Generic)
import Prelude hiding (Real, head, last, print, putStr, putStrLn, readFile, reverse, writeFile, (!!))
import Prelude qualified

identity :: a -> a
identity x = x

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . Prelude.putStrLn

putStr :: (MonadIO m) => String -> m ()
putStr = liftIO . Prelude.putStr
