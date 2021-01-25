{-# LANGUAGE PatternGuards #-}

-- | URL Utility Functions
module Lambdabot.Util.Browser (
  doHttpRequest,
  doHttpRequest'
) where

import Lambdabot.Config (MonadConfig (getConfig))
import Lambdabot.Config.Reference (configAllowRedirects, configMaxRedirects, configProxy)
import Lambdabot.Monad (MonadLB (..))
import Lambdabot.Util (io)

import qualified Network.HTTP.Simple as S (Request, getResponseBody, getResponseHeaders, getResponseStatusCode, httpLBS)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Client.Conduit (Request(redirectCount, proxy))
import Data.Maybe (fromJust)
import Text.XML.Cursor (Cursor, fromDocument)
import Text.HTML.DOM (parseLBS)

-- | Run a browser action with some standardized settings
doHttpRequest :: (MonadLB m, MonadConfig m) => S.Request -> (Int -> Cursor -> [(HeaderName, B.ByteString)] -> a) -> m a
doHttpRequest request' handler = do
  proxyAddress <- getConfig configProxy
  allowRedirects <- getConfig configAllowRedirects
  maxRedirects <- getConfig configMaxRedirects
  let request = request' {
    proxy = proxyAddress,
    redirectCount = if allowRedirects then fromJust maxRedirects else 0
  }
  result <- io $ S.httpLBS request
  let statusCode = S.getResponseStatusCode result
  let body = S.getResponseBody result
  let document = parseLBS body
  let cursor = fromDocument document
  let responseHeaders = S.getResponseHeaders result
  return $ handler statusCode cursor responseHeaders

doHttpRequest' :: (MonadLB m, MonadConfig m) => S.Request -> (Int -> LB.ByteString -> [(HeaderName, B.ByteString)] -> a) -> m a
doHttpRequest' request' handler = do
  proxyAddress <- getConfig configProxy
  allowRedirects <- getConfig configAllowRedirects
  maxRedirects <- getConfig configMaxRedirects
  let request = request' {
    proxy = proxyAddress,
    redirectCount = if allowRedirects then fromJust maxRedirects else 0
  }
  result <- io $ S.httpLBS request
  let statusCode = S.getResponseStatusCode result
  let body = S.getResponseBody result
  let responseHeaders = S.getResponseHeaders result
  return $ handler statusCode body responseHeaders
