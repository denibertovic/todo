{-# LANGUAGE OverloadedStrings #-}

-- | WAI middleware for bounding request bodies and rate-limiting
-- public endpoints. This is the first line of defense when the
-- sync server is exposed on the public internet; it runs before
-- Servant even sees the request.
module Middleware
  ( -- * Body size limit
    requestSizeLimiter
    -- * Rate limiting
  , RateLimiter
  , newRateLimiter
  , registerRateLimiter
    -- * Exposed for tests
  , checkAllowed
  , maxRequestsPerWindow
  ) where

import           RIO
import qualified RIO.Text as T
import qualified RIO.Map  as Map

import qualified Data.ByteString.Char8   as BS8
import           Data.Time.Clock         (NominalDiffTime, UTCTime,
                                          diffUTCTime, getCurrentTime)
import           Network.HTTP.Types      (hContentLength, status413, status429)
import           Network.Socket          (SockAddr)
import           Network.Wai             (Middleware, pathInfo, remoteHost,
                                          requestHeaders, responseLBS)

-- | Reject requests whose declared @Content-Length@ header exceeds
-- the given limit. Requests without a @Content-Length@ header are
-- allowed through — Warp's @setMaximumBodyFlush@ handles those.
requestSizeLimiter :: Word64 -> Middleware
requestSizeLimiter maxBytes app req respond =
  case lookup hContentLength (requestHeaders req) of
    Just hv
      | Just n <- readMaybe (BS8.unpack hv) :: Maybe Word64
      , n > maxBytes ->
          respond $ responseLBS status413
            [("Content-Type", "text/plain")]
            "Request body too large"
    _ -> app req respond

-- | Sliding-window rate limiter state. Keyed by remote address (host
-- part of the 'SockAddr'), each entry stores the timestamps of recent
-- allowed requests. Entries older than 'windowSeconds' are pruned on
-- every access so the map cannot grow without bound for long-lived
-- processes serving many clients.
newtype RateLimiter = RateLimiter (IORef (Map.Map Text [UTCTime]))

-- | Requests allowed per IP inside the sliding window. The @/register@
-- endpoint is rarely used (once per new device), so a tight limit is
-- fine.
maxRequestsPerWindow :: Int
maxRequestsPerWindow = 10

-- | Sliding window length, in seconds.
windowSeconds :: NominalDiffTime
windowSeconds = 60

-- | Construct a fresh rate limiter with no recorded requests.
newRateLimiter :: IO RateLimiter
newRateLimiter = RateLimiter <$> newIORef Map.empty

-- | Rate-limit @POST /register@ by remote IP. Non-register requests
-- pass through untouched. When the limit is exceeded we reply @429@
-- so clients and load balancers can apply their usual backoff.
registerRateLimiter :: RateLimiter -> Middleware
registerRateLimiter limiter app req respond
  | pathInfo req == ["register"] = do
      allowed <- checkAllowed limiter (remoteHost req)
      if allowed
        then app req respond
        else respond $ responseLBS status429
          [("Content-Type", "text/plain")]
          "Too many registration attempts, slow down"
  | otherwise = app req respond

-- | Check-and-record: returns 'True' if the caller may proceed and
-- appends a timestamp to their bucket; returns 'False' otherwise.
-- Expired entries are pruned atomically under the same IORef update.
checkAllowed :: RateLimiter -> SockAddr -> IO Bool
checkAllowed (RateLimiter ref) addr = do
  now <- getCurrentTime
  let key = T.pack (show addr)
  atomicModifyIORef' ref $ \m ->
    let current   = fromMaybe [] (Map.lookup key m)
        fresh     = filter (\t -> diffUTCTime now t <= windowSeconds) current
        overLimit = length fresh >= maxRequestsPerWindow
    in if overLimit
       then (Map.insert key fresh m, False)
       else (Map.insert key (now : fresh) m, True)
