module NSO.Image.Asdf.FileManager where

import NSO.Image.Asdf.Ref
import NSO.Image.Fits.Frame (HDUIndex (..))
import NSO.Prelude
import Telescope.Asdf
import Telescope.Asdf.NDArray (DataType (..))
import Telescope.Data.Axes (Axes, Major (Row))


data FileManager = FileManager
  { datatype :: DataType
  , fileuris :: Ref "fileuris"
  , shape :: Axes Row
  , target :: HDUIndex
  }
  deriving (Generic)
instance ToAsdf FileManager where
  schema _ = "asdf://dkist.nso.edu/tags/file_manager-1.0.0"


fileManager :: Axes Row -> HDUIndex -> FileManager
fileManager axes hix =
  FileManager{datatype = Float64, fileuris = Ref, shape = axes, target = hix}
