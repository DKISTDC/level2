module NSO.Files.RemoteFolder where

import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import NSO.Prelude
import NSO.Types.Common as App


data Source
data Dest


-- | The location of standard files on different systems.
data RemoteFolder sys a = RemoteFolder
  { collection :: Globus.Id Collection
  , directory :: Path sys Dir a
  }
  deriving (Show)
