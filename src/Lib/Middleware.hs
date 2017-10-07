module Lib.Middleware
  ( healthz
  , forceHttpsProxy
  ) where

import Data.List (lookup)
import Network.HTTP.Types (hLocation, methodGet, status200, status301, status307)
import Network.Wai

import Lib.Prelude

-- | Return 200 OK for requests to /healthz.
--
-- This is mainly useful when added before other middleware, in order to bypass
-- it.
healthz :: Middleware
healthz app req sendResponse =
  case pathInfo req of
    ["healthz"] -> sendResponse ok
    _ -> app req sendResponse
  where ok = responseBuilder status200 [] mempty

-- | For requests that don't appear secure, but appear to come from a reverse
-- proxy, redirect to https.
forceHttpsProxy :: Middleware
forceHttpsProxy app req sendResponse =
  case (isHttp req, redirectResponse req) of
    (True, Just resp) -> sendResponse resp
    _                 -> app req sendResponse
  where isHttp = maybe False (== "http")
          . lookup "x-forwarded-proto"
          . requestHeaders

redirectResponse :: Request -> Maybe Response
redirectResponse req = do
  host <- requestHeaderHost req
  return $ responseBuilder status [(hLocation, location host)] mempty
  where
    location h = "https://" <> h <> rawPathInfo req <> rawQueryString req
    status
        | requestMethod req == methodGet = status301
        | otherwise = status307
