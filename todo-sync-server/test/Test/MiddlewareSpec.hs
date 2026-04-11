{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @/register@ rate limiter and the body-size
-- middleware. Both run before Servant sees the request, so their
-- behavior is what protects the server on the public internet.
--
-- The rate limiter is tested by calling 'checkAllowed' directly —
-- it's exposed from "Middleware" specifically so unit tests can
-- poke at the sliding-window state without spinning up a real
-- Warp listener.
module Test.MiddlewareSpec where

import           RIO

import           Control.Monad           (replicateM)
import           Network.Socket          (SockAddr (..), tupleToHostAddress)
import           Test.Hspec

import           Middleware

-- | Auto-discovered by hspec-discover.
spec :: Spec
spec = do

  describe "checkAllowed (rate limiter)" $ do
    it "allows the first request from a fresh IP" $ do
      limiter <- newRateLimiter
      ok <- checkAllowed limiter addr1
      ok `shouldBe` True

    it "allows up to maxRequestsPerWindow requests from one IP" $ do
      -- We shouldn't start rejecting until the N+1th request, where
      -- N == maxRequestsPerWindow. Exercise exactly N calls and
      -- expect all True.
      limiter <- newRateLimiter
      results <- replicateM maxRequestsPerWindow (checkAllowed limiter addr1)
      all id results `shouldBe` True

    it "rejects the (max+1)th request in the same window" $ do
      limiter <- newRateLimiter
      -- Burn through the quota.
      _ <- replicateM maxRequestsPerWindow (checkAllowed limiter addr1)
      -- The next one must be rejected.
      denied <- checkAllowed limiter addr1
      denied `shouldBe` False

    it "isolates buckets per remote address" $ do
      -- Limiting addr1 must not affect addr2. This is the "one
      -- noisy client can't DoS others" property.
      limiter <- newRateLimiter
      _ <- replicateM maxRequestsPerWindow (checkAllowed limiter addr1)
      -- addr1 is now burned; addr2 should still be fresh.
      addr1Denied <- checkAllowed limiter addr1
      addr2Ok     <- checkAllowed limiter addr2
      addr1Denied `shouldBe` False
      addr2Ok     `shouldBe` True

    it "tracks N-1 good requests without exhausting the quota" $ do
      limiter <- newRateLimiter
      -- N-1 calls — still within quota.
      _ <- replicateM (maxRequestsPerWindow - 1) (checkAllowed limiter addr1)
      -- One more should still succeed (the Nth).
      last' <- checkAllowed limiter addr1
      last' `shouldBe` True

-- --- helpers -----------------------------------------------------

-- | Two distinct IPv4 @SockAddr@s used as rate-limiter bucket keys.
addr1 :: SockAddr
addr1 = SockAddrInet 0 (tupleToHostAddress (192, 0, 2, 1))

addr2 :: SockAddr
addr2 = SockAddrInet 0 (tupleToHostAddress (192, 0, 2, 2))
