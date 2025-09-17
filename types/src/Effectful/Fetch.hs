module Effectful.Fetch where

import Data.ByteString.Lazy (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import Network.HTTP.Client as Http
import Network.HTTP.Types
import Network.URI
import Prelude


data FetchResponse = FetchResponse
  { body :: ByteString
  , headers :: [Header]
  , status :: Status
  }


data Fetch :: Effect where
  Fetch :: Method -> URI -> [Header] -> RequestBody -> Fetch m FetchResponse
type instance DispatchOf Fetch = 'Dynamic


runFetchHttp
  :: (IOE :> es)
  => Manager
  -> Eff (Fetch : es) a
  -> Eff es a
runFetchHttp mgr = interpret $ \_ -> \case
  Fetch m u hs body -> do
    r <- liftIO $ requestFromURI u
    let req = r{method = m, requestHeaders = hs, requestBody = body}
    res <- liftIO $ Http.httpLbs req mgr
    pure $
      FetchResponse
        { body = responseBody res
        , headers = responseHeaders res
        , status = responseStatus res
        }


runFetchMock
  :: (IOE :> es)
  => (Method -> URI -> [Header] -> RequestBody -> IO FetchResponse)
  -> Eff (Fetch : es) a
  -> Eff es a
runFetchMock mock = interpret $ \_ -> \case
  Fetch m u hs body -> do
    liftIO $ mock m u hs body


get :: (Fetch :> es) => URI -> [Header] -> Eff es FetchResponse
get u hs = send $ Fetch methodGet u hs ""


post :: (Fetch :> es) => URI -> [Header] -> RequestBody -> Eff es FetchResponse
post u hs body = send $ Fetch methodPost u hs body
