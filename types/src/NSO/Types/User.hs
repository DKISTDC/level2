module NSO.Types.User where

import Data.Aeson (FromJSON, ToJSON)
import Effectful.Globus hiding (Id)
import NSO.Prelude
import Network.Globus
import Web.Hyperbole.Data.Param


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
