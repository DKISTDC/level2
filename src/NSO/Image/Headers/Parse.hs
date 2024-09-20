module NSO.Image.Headers.Parse
  ( Parser
  , runParser
  , requireKey
  , parseThrow
  , lookupKey
  , runParseError
  , ParseError
  )
where

import Effectful
import Effectful.Error.Static
import NSO.Prelude
import Telescope.Data.Parser
import Telescope.Fits as Fits
import Telescope.Fits.Header (parseKeyword)


-- | If the key exists, parse it
lookupKey :: (Monad m, FromKeyword a) => Text -> Header -> m (Maybe a)
lookupKey k h = pure $ do
  val <- lookupKeyword k h
  either (const Nothing) pure $ do
    runParser $ parseKeywordValue val


requireKey :: (Error ParseError :> es, FromKeyword a) => Text -> Header -> Eff es a
requireKey k h = parseThrow $ parseKeyword k h


parseThrow :: (Error ParseError :> es) => Parser a -> Eff es a
parseThrow parse =
  case runParser parse of
    Left e -> throwError e
    Right a -> pure a


runParseError :: (Error err :> es) => (ParseError -> err) -> Eff (Error ParseError : es) a -> Eff es a
runParseError f = runErrorNoCallStackWith @ParseError (throwError . f)
