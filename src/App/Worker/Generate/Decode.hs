module App.Worker.Generate.Decode where

import App.Worker.Generate.Error (GenerateError (..))
import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static
import NSO.Files as Files
import NSO.Files.Scratch as Scratch
import NSO.Prelude
import NSO.Types.Common
import Telescope.Data.Parser (ParseError)
import Telescope.Fits (Fits (..))
import Telescope.Fits.Encoding qualified as Fits


readFits :: (Scratch :> es, Error GenerateError :> es) => Path Scratch File a -> Eff es Fits
readFits path = do
  inp <- Scratch.readFile path
  runErrorNoCallStackWith (throwError . ParseError path.filePath) $ fitsDecode inp


fitsDecode :: (Error ParseError :> es) => BS.ByteString -> Eff es Fits
fitsDecode inp =
  case Fits.runFitsParse Fits.parseFits inp of
    Left e -> throwError e
    Right f -> pure f
