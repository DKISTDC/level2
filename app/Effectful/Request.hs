{-# LANGUAGE OverloadedStrings #-}

module Effectful.Request where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text
import Data.Text qualified as T

-- import Data.ByteString.Lazy.Char8 qualifed as L
import Effectful
import Effectful.Dispatch.Dynamic
import Network.HTTP.Req

import Data.Aeson
import Data.Morpheus.Client

data Request :: Effect where
  Post :: Url scheme -> ByteString -> Option scheme -> Request m ByteString

type instance DispatchOf Request = 'Dynamic

httpsText :: Text -> Url 'Https
httpsText t = https $ T.drop 8 t

runRequest
  :: (IOE :> es)
  => Eff (Request : es) a
  -> Eff es a
runRequest = interpret $ \_ -> \case
  Post url inp opt -> liftIO $ do
    runReq defaultHttpConfig $ do
      responseBody <$> req POST url (ReqBodyLbs inp) lbsResponse opt

runRequestMock
  :: (IOE :> es)
  => (Text -> ByteString -> IO ByteString)
  -> Eff (Request : es) a
  -> Eff es a
runRequestMock run = interpret $ \_ -> \case
  Post url inp _ -> liftIO $ run (renderUrl url) inp

data GraphQL :: Effect where
  Fetch
    :: (ToJSON (Args a), FromJSON a, RequestType a)
    => Service
    -> Args a
    -> GraphQL m (Either (FetchError a) a)

type instance DispatchOf GraphQL = 'Dynamic

newtype Service = Service (Url 'Http)

runGraphQL
  :: (Request :> es)
  => Eff (GraphQL : es) a
  -> Eff es a
runGraphQL = interpret $ \_ -> \case
  Fetch (Service url) args -> do
    fetch (sendRequest url) args
 where
  sendRequest url inp = do
    let headers = header "Content-Type" "application/json"
    send $ Post url inp headers
