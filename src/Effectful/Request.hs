{-# LANGUAGE OverloadedStrings #-}

module Effectful.Request where

import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client as Morpheus
import Data.Morpheus.Types (GQLError)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import NSO.Prelude
import Network.HTTP.Req
import Text.URI (mkURI)


newtype Service = Service (Url 'Http)


service :: Text -> Maybe Service
service inp = do
  uri <- mkURI inp
  url <- fst <$> useHttpURI uri
  pure $ Service url


data Request :: Effect where
  Post :: Url scheme -> Option scheme -> ByteString -> Request m ByteString


type instance DispatchOf Request = 'Dynamic


data GraphQL :: Effect where
  Fetch
    :: (ToJSON (Args a), FromJSON a, RequestType a)
    => Service
    -> Args a
    -> GraphQL m a


type instance DispatchOf GraphQL = 'Dynamic


httpsText :: Text -> Url 'Https
httpsText t = https $ T.drop 8 t


runRequest
  :: (IOE :> es)
  => Eff (Request : es) a
  -> Eff es a
runRequest = interpret $ \_ -> \case
  Post url opt inp -> liftIO $ requestIO url opt inp


runRequestMock
  :: (IOE :> es)
  => (Text -> ByteString -> IO ByteString)
  -> Eff (Request : es) a
  -> Eff es a
runRequestMock mock = interpret $ \_ -> \case
  Post url _ inp -> liftIO $ mock (renderUrl url) inp


requestIO
  :: Url scheme
  -> Option scheme
  -> ByteString
  -> IO ByteString
requestIO url opt inp =
  runReq defaultHttpConfig $ do
    responseBody <$> req POST url (ReqBodyLbs inp) lbsResponse opt


runGraphQL
  :: (Request :> es, Error RequestError :> es)
  => Eff (GraphQL : es) a
  -> Eff es a
runGraphQL = interpret $ \_ -> \case
  Fetch (Service url) args -> do
    er <- Morpheus.fetch (sendRequest url) args
    either (throwError . toError) pure er
 where
  sendRequest url inp = do
    let headers = header "Content-Type" "application/json"
    send $ Post url headers inp

  toError FetchErrorNoResult = FetchNoResult
  toError (FetchErrorProducedErrors ges _) = FetchGQLErrors ges
  toError (FetchErrorParseFailure s) = FetchParseFailure s


data RequestError
  = FetchParseFailure String
  | FetchGQLErrors (NonEmpty GQLError)
  | FetchNoResult
  | ParseError String
  deriving (Show, Exception)


fetch
  :: (ToJSON (Args a), FromJSON a, RequestType a, GraphQL :> es)
  => Service
  -> Args a
  -> Eff es a
fetch srvc args = do
  send $ Fetch srvc args
