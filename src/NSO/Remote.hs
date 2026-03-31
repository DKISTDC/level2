module NSO.Remote
  ( Level1
  , User (..)
  , CurrentAccess (..)
  , Publish
  , Remote
  , Ingest
  , Output
  ) where

import Data.Aeson (FromJSON, ToJSON)
import NSO.Files.RemoteFolder (Remote (..))
import NSO.Files.Scratch (Ingest, Output)
import NSO.Prelude
import Network.Globus
import Web.Hyperbole.Data.Param


data Level1
data Publish


data User = User
  { email :: UserEmail
  , access :: CurrentAccess
  }
  deriving (ToJSON, FromJSON, Generic, FromParam, ToParam)


data CurrentAccess = CurrentAccess
  { token :: Token Access
  , expires :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToParam, FromParam)
