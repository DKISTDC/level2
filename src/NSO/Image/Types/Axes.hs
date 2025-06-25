module NSO.Image.Types.Axes
  ( Depth
  , SlitX
  , FrameY
  , Stokes
  )
where

import NSO.Prelude
import NSO.Types.Common (Stokes)
import Telescope.Asdf.GWCS (ToAxes (..))
import Telescope.Data.KnownText


data Depth deriving (Generic, ToAxes)
data SlitX
data FrameY
instance KnownText Depth where
  knownText = "OpticalDepth"
instance KnownText SlitX where
  knownText = "SlitX"
instance KnownText FrameY where
  knownText = "FrameY"
