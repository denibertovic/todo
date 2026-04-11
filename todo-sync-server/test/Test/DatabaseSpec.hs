{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the security-critical database layer: invite code
-- lifecycle, atomic register-with-invite, and the associated
-- guarantees documented in @Database.hs@.
--
-- These tests drive a real SQLite database on disk (created in a
-- temp directory that is torn down between specs), not a mock.
-- The whole point of these tests is to catch regressions in the
-- SQL transaction boundaries — a mock would hide exactly the class
-- of bug we care about.
module Test.DatabaseSpec where

import           RIO
import qualified RIO.Text as T

import           Data.Time.Clock         (NominalDiffTime, UTCTime,
                                          addUTCTime, diffUTCTime,
                                          getCurrentTime)
import           Database.SQLite.Simple  (Only (..), execute, query_)
import           System.FilePath         ((</>))
import           Test.Hspec

import           Database
import           Types                   (DeviceId (..))

-- | Auto-discovered by hspec-discover.
spec :: Spec
spec = do

  describe "createInviteCodeWithExpiry" $ do
    it "inserts a row and returns (code, expiry)" $
      withTempDb $ \dbPath -> do
        (code, expiresAt) <- createInviteCodeWithExpiry dbPath (3600 :: NominalDiffTime)
        T.null code `shouldBe` False
        now <- getCurrentTime
        -- The returned expiry should be ~1 hour from now.
        let delta = realToFrac (diffUTCTime expiresAt now) :: Double
        (delta > 3500 && delta < 3700) `shouldBe` True

  describe "registerDeviceWithInviteCode" $ do

    it "accepts a fresh valid invite and creates a device row" $
      withTempDb $ \dbPath -> do
        (code, _) <- createInviteCodeWithExpiry dbPath 3600
        result <- withDb dbPath $ \conn ->
          registerDeviceWithInviteCode conn "laptop" code
        result `shouldSatisfy` isJust
        -- Exactly one device row should exist.
        deviceCount dbPath `shouldReturn` 1

    it "rejects an unknown invite code and creates NO device row" $
      -- This is the atomic-transaction regression guard referenced
      -- in the Phase 0 server fix at Database.hs:271-295. Before
      -- that fix, the device row was inserted *before* the invite
      -- code was validated, so a bad invite code left an orphaned
      -- device behind. Here we assert the opposite: a bad code
      -- leaves the devices table empty.
      withTempDb $ \dbPath -> do
        result <- withDb dbPath $ \conn ->
          registerDeviceWithInviteCode conn "intruder" "not-a-real-code"
        result `shouldBe` Nothing
        deviceCount dbPath `shouldReturn` 0

    it "rejects an expired invite code" $
      withTempDb $ \dbPath -> do
        -- Insert an invite directly with an expires_at timestamp in
        -- the past, bypassing createInviteCodeWithExpiry which would
        -- only give us forward-dated expiries.
        now <- getCurrentTime
        let past = addUTCTime (-3600) now  -- 1 hour ago
        withDb dbPath $ \conn ->
          execute conn
            "INSERT INTO invite_codes (code, created_at, expires_at) VALUES (?, ?, ?)"
            ("expired-code" :: T.Text, past, past)
        result <- withDb dbPath $ \conn ->
          registerDeviceWithInviteCode conn "laptop" "expired-code"
        result `shouldBe` Nothing
        deviceCount dbPath `shouldReturn` 0

    it "rejects an already-used invite code (replay attack guard)" $
      withTempDb $ \dbPath -> do
        (code, _) <- createInviteCodeWithExpiry dbPath 3600
        -- First use succeeds.
        firstUse <- withDb dbPath $ \conn ->
          registerDeviceWithInviteCode conn "laptop" code
        firstUse `shouldSatisfy` isJust
        -- Second use with the same code must fail — it's been
        -- stamped used_at and marked used_by_device_id.
        secondUse <- withDb dbPath $ \conn ->
          registerDeviceWithInviteCode conn "attacker" code
        secondUse `shouldBe` Nothing
        -- Only one device row, not two.
        deviceCount dbPath `shouldReturn` 1

    it "allows two different devices to register with two different codes" $
      -- Sanity check to make sure we're not over-rejecting.
      withTempDb $ \dbPath -> do
        (code1, _) <- createInviteCodeWithExpiry dbPath 3600
        (code2, _) <- createInviteCodeWithExpiry dbPath 3600
        r1 <- withDb dbPath $ \conn ->
          registerDeviceWithInviteCode conn "laptop" code1
        r2 <- withDb dbPath $ \conn ->
          registerDeviceWithInviteCode conn "phone" code2
        r1 `shouldSatisfy` isJust
        r2 `shouldSatisfy` isJust
        deviceCount dbPath `shouldReturn` 2

  describe "getDeviceByToken" $ do
    it "returns the device row for a freshly-registered device" $
      withTempDb $ \dbPath -> do
        (code, _) <- createInviteCodeWithExpiry dbPath 3600
        Just (_, authToken) <- withDb dbPath $ \conn ->
          registerDeviceWithInviteCode conn "laptop" code
        found <- withDb dbPath $ \conn -> getDeviceByToken conn authToken
        found `shouldSatisfy` isJust

    it "returns Nothing for an unknown token" $
      withTempDb $ \dbPath -> do
        found <- withDb dbPath $ \conn -> getDeviceByToken conn "bogus-token"
        found `shouldBe` Nothing

-- --- helpers -----------------------------------------------------

-- | Run an action with a fresh temp SQLite database, fully
-- initialized with the schema. The temp directory is cleaned up
-- automatically on both success and exception.
withTempDb :: (FilePath -> IO a) -> IO a
withTempDb action =
  withSystemTempDirectory "todo-sync-test" $ \dir -> do
    let dbPath = dir </> "t.db"
    initDatabase dbPath
    action dbPath

-- | Count the rows in the devices table. Used to verify the atomic
-- transaction guarantees — a rejected registration must leave the
-- devices table exactly as it was.
deviceCount :: FilePath -> IO Int
deviceCount dbPath = withDb dbPath $ \conn -> do
  rows <- query_ conn "SELECT COUNT(*) FROM devices" :: IO [Only Int]
  return $ case rows of
    [Only n] -> n
    _        -> error "SELECT COUNT(*) returned unexpected shape"

