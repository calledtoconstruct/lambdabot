{-# LANGUAGE PatternGuards #-}

-- | URL Utility Functions
module Lambdabot.Util.Browser (
  doHttpRequest,
  HttpResponseHandler,
  doHttpRequest',
  AlternateHttpResponseHandler,
  RequestOverride
) where

import Lambdabot.Config (MonadConfig (getConfig))
import Lambdabot.Config.Reference (configAllowRedirects, configMaxRedirects, configProxy)
import Lambdabot.Monad (MonadLB (..))
import Lambdabot.Util (io)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe (fromJust)
import Network.HTTP.Client.Conduit (Request (proxy, redirectCount))
import qualified Network.HTTP.Simple as S (Request, getResponseBody, getResponseHeaders, getResponseStatusCode, httpLBS)
import Network.HTTP.Types.Header (HeaderName)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument)

type HttpResponseHandler a = Int -> Cursor -> [(HeaderName, B.ByteString)] -> a
type AlternateHttpResponseHandler a = Int -> LB.ByteString -> [(HeaderName, B.ByteString)] -> a
type RequestOverride = Maybe (S.Request -> S.Request)

-- | Run a browser action with some standardized settings
doHttpRequest :: (MonadLB m, MonadConfig m) => S.Request -> RequestOverride -> HttpResponseHandler a -> m a
doHttpRequest request' maybeOverride handler = do
  proxyAddress <- getConfig configProxy
  allowRedirects <- getConfig configAllowRedirects
  maxRedirects <- getConfig configMaxRedirects
  let request =
        request'
          { proxy = proxyAddress
          , redirectCount = if allowRedirects then fromJust maxRedirects else 0
          }
  result <- case maybeOverride of
    Just override -> io $ S.httpLBS $ override request
    Nothing -> io $ S.httpLBS request
  let statusCode = S.getResponseStatusCode result
  let body = S.getResponseBody result
  let document = parseLBS body
  let cursor = fromDocument document
  let responseHeaders = S.getResponseHeaders result
  return $ handler statusCode cursor responseHeaders

doHttpRequest' :: (MonadLB m, MonadConfig m) => S.Request -> RequestOverride -> AlternateHttpResponseHandler a -> m a
doHttpRequest' request' maybeOverride handler = do
  proxyAddress <- getConfig configProxy
  allowRedirects <- getConfig configAllowRedirects
  maxRedirects <- getConfig configMaxRedirects
  let request =
        request'
          { proxy = proxyAddress
          , redirectCount = if allowRedirects then fromJust maxRedirects else 0
          }
  result <- case maybeOverride of
    Just override -> io $ S.httpLBS $ override request
    Nothing -> io $ S.httpLBS request
  let statusCode = S.getResponseStatusCode result
  let body = S.getResponseBody result
  let responseHeaders = S.getResponseHeaders result
  return $ handler statusCode body responseHeaders
