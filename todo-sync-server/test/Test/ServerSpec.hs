{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | HTTP-level tests for the sync server. Exercises the WAI
-- 'Application' directly via 'hspec-wai' — no real sockets, no
-- Warp, no threads — which gives us fast, deterministic coverage
-- of the handler logic, Servant routing, JSON wire format, and
-- the full middleware stack all the way through.
--
-- Every test item gets a fresh temp-file SQLite database via the
-- per-item 'with' fixture in 'hspec-wai'. The DB path is stashed
-- in an 'IORef' that the fixture rewrites on each invocation, so
-- tests that need to seed state (e.g. insert an invite code
-- directly) can call @liftIO $ readIORef dbRef >>= …@ to poke at
-- the same database the request handlers are pointed at.
module Test.ServerSpec where

import           RIO
import qualified RIO.Text as T

import           Data.Aeson              (decode, object, (.=))
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import           Data.Time.Clock         (addUTCTime, getCurrentTime)
import           Database.SQLite.Simple  (execute)
import           Network.HTTP.Types      (HeaderName, hAuthorization,
                                          hContentLength, hContentType,
                                          status200)
import           Network.Wai             (Application)
import           Network.Wai.Test        (SResponse (..))
import           Servant                 (serve)
import           System.FilePath         ((</>))
import           System.IO.Temp          (createTempDirectory,
                                          getCanonicalTemporaryDirectory)
import           Test.Hspec
import           Test.Hspec.Wai

import           Database
import           Middleware
import           Server                  (SyncAPI, syncServer)
import           Types                   (CreateInviteResponse (..),
                                          RegisterResponse (..))

-- | In-module mutable reference to the current test DB path. The
-- fixture rewrites this on every @it@ so tests always read back
-- the DB the fresh app is actually pointed at.
type DbRef = IORef FilePath

-- | Build a fresh 'Application' backed by a new temp-file SQLite
-- database. Called once per @it@ via 'with', so every test starts
-- with no state leakage from its siblings.
freshApp :: DbRef -> IO Application
freshApp dbRef = do
  tmpRoot <- getCanonicalTemporaryDirectory
  dir <- createTempDirectory tmpRoot "sync-wai"
  let dbPath = dir </> "t.db"
  initDatabase dbPath
  writeIORef dbRef dbPath
  limiter <- newRateLimiter
  return (testApp dbPath limiter)

-- | Production middleware stack reconstructed from exported
-- building blocks so the test doesn't depend on private
-- 'Server' internals. Body-size limit + rate limiter + Servant.
testApp :: FilePath -> RateLimiter -> Application
testApp dbPath limiter =
    registerRateLimiter limiter
  $ requestSizeLimiter testMaxBodyBytes
  $ serve (Proxy :: Proxy SyncAPI) (syncServer dbPath)

-- | Must match 'Server.maxRequestBodyBytes'. Duplicated locally
-- because 'Server' doesn't export the constant.
testMaxBodyBytes :: Word64
testMaxBodyBytes = 1024 * 1024

-- --- common request helpers ----------------------------------------

jsonHeaders :: [(HeaderName, ByteString)]
jsonHeaders = [(hContentType, "application/json")]

bearerHeaders :: Text -> [(HeaderName, ByteString)]
bearerHeaders token =
  [ (hContentType, "application/json")
  , (hAuthorization, "Bearer " <> encodeUtf8 token)
  ]

-- | Build a @/register@ request body with a given invite code.
registerBody :: Text -> Text -> BL.ByteString
registerBody deviceName inviteCode =
  Aeson.encode $ object
    [ "device_name" .= deviceName
    , "invite_code" .= inviteCode
    ]

-- | Build a minimal @/sync@ request body with no operations.
emptySyncBody :: Text -> BL.ByteString
emptySyncBody deviceIdText =
  Aeson.encode $ object
    [ "device_id"  .= deviceIdText
    , "cursor"     .= Aeson.Null
    , "operations" .= ([] :: [Aeson.Value])
    ]

-- | Seed a fresh invite code directly into the DB via 'Database'.
-- Returns the code text.
seedInvite :: DbRef -> IO Text
seedInvite ref = do
  path <- readIORef ref
  fst <$> createInviteCodeWithExpiry path 3600

-- | Seed an invite code whose @expires_at@ is in the past.
-- Used to test the expiry-rejection branch without having to
-- actually wait.
seedExpiredInvite :: DbRef -> IO Text
seedExpiredInvite ref = do
  path <- readIORef ref
  now <- getCurrentTime
  let past = addUTCTime (-3600) now
      -- Using show of a UTCTime here just gives us a unique-ish
      -- suffix so multiple tests don't collide if they both seed
      -- expired invites into the same database.
      code = "expired-invite-" <> T.pack (show now)
  withDb path $ \conn ->
    execute conn
      "INSERT INTO invite_codes (code, created_at, expires_at) VALUES (?, ?, ?)"
      (code, past, past)
  return code

-- | Register a fresh device by seeding an invite then POSTing
-- @/register@. Returns the device's auth token for use in later
-- @/sync@ or @/invite@ calls within the same test.
registerFreshDevice :: DbRef -> WaiSession () Text
registerFreshDevice ref = do
  code <- liftIO (seedInvite ref)
  resp <- request "POST" "/register" jsonHeaders (registerBody "laptop" code)
  case decode (simpleBody resp) :: Maybe RegisterResponse of
    Just rr -> return (rresAuthToken rr)
    Nothing -> do
      liftIO $ expectationFailure $
        "register response did not decode: " <> show (simpleBody resp)
      error "unreachable after expectationFailure"

-- --- spec ----------------------------------------------------------

-- | Auto-discovered by hspec-discover.
spec :: Spec
spec = do
  dbRef <- runIO (newIORef "")

  with (freshApp dbRef) $ do

    describe "GET /health" $ do
      it "returns 200 unauthenticated" $
        get "/health" `shouldRespondWith` 200

      it "returns a JSON body with status=ok" $ do
        resp <- get "/health"
        liftIO $ simpleBody resp `shouldBe` "{\"status\":\"ok\"}"

    describe "POST /register" $ do

      it "accepts a fresh valid invite code with 200" $ do
        code <- liftIO (seedInvite dbRef)
        request "POST" "/register" jsonHeaders (registerBody "laptop" code)
          `shouldRespondWith` 200

      it "rejects an unknown invite code with 403" $
        request "POST" "/register" jsonHeaders (registerBody "attacker" "not-a-real-code")
          `shouldRespondWith` 403

      it "rejects an expired invite code with 403" $ do
        code <- liftIO (seedExpiredInvite dbRef)
        request "POST" "/register" jsonHeaders (registerBody "laptop" code)
          `shouldRespondWith` 403

      it "rejects an already-used invite code with 403 (replay guard)" $ do
        code <- liftIO (seedInvite dbRef)
        request "POST" "/register" jsonHeaders (registerBody "laptop" code)
          `shouldRespondWith` 200
        request "POST" "/register" jsonHeaders (registerBody "attacker" code)
          `shouldRespondWith` 403

      it "rejects bodies over 1 MiB with 413 (body size middleware)" $ do
        -- Build an oversize payload and include a matching
        -- Content-Length header so the middleware sees it before
        -- the handler. Contents are junk — the size check runs
        -- first and short-circuits.
        let oversize = BL.fromStrict (BS.replicate (fromIntegral testMaxBodyBytes + 1) 0x20)
            clHeader = (hContentLength, fromString (show (BL.length oversize)))
        request "POST" "/register" (clHeader : jsonHeaders) oversize
          `shouldRespondWith` 413

    describe "POST /sync" $ do

      it "rejects requests with no Authorization header with 401" $
        request "POST" "/sync" jsonHeaders (emptySyncBody fakeDeviceId)
          `shouldRespondWith` 401

      it "rejects requests with an invalid bearer token with 401" $
        request "POST" "/sync"
          (bearerHeaders "not-a-real-token")
          (emptySyncBody fakeDeviceId)
          `shouldRespondWith` 401

      it "rejects requests with an empty bearer token with 401" $
        request "POST" "/sync" (bearerHeaders "") (emptySyncBody fakeDeviceId)
          `shouldRespondWith` 401

      it "accepts a valid token in the Authorization header with 200" $ do
        token <- registerFreshDevice dbRef
        request "POST" "/sync" (bearerHeaders token) (emptySyncBody fakeDeviceId)
          `shouldRespondWith` 200

      it "accepts a valid token in the request body (backcompat path) with 200" $ do
        token <- registerFreshDevice dbRef
        let body = Aeson.encode $ object
              [ "device_id"  .= fakeDeviceId
              , "cursor"     .= Aeson.Null
              , "operations" .= ([] :: [Aeson.Value])
              , "auth_token" .= token
              ]
        request "POST" "/sync" jsonHeaders body
          `shouldRespondWith` 200

    describe "POST /invite" $ do

      it "rejects requests with no Authorization header with 401" $
        request "POST" "/invite" jsonHeaders "{}"
          `shouldRespondWith` 401

      it "rejects requests with an invalid bearer token with 401" $
        request "POST" "/invite" (bearerHeaders "not-a-real-token") "{}"
          `shouldRespondWith` 401

      it "returns a new invite code for a valid bearer token" $ do
        token <- registerFreshDevice dbRef
        resp <- request "POST" "/invite" (bearerHeaders token) "{}"
        liftIO $ do
          simpleStatus resp `shouldBe` status200
          case decode (simpleBody resp) :: Maybe CreateInviteResponse of
            Nothing -> expectationFailure $
              "could not decode CreateInviteResponse: " <> show (simpleBody resp)
            Just CreateInviteResponse{..} ->
              T.null cresInviteCode `shouldBe` False

      it "mints a usable invite code — phone can register with it end-to-end" $ do
        -- Canonical onboarding flow: laptop mints an invite, phone
        -- registers with it. Makes sure the /invite → /register
        -- handoff works in one go.
        laptopToken <- registerFreshDevice dbRef
        inviteResp <- request "POST" "/invite" (bearerHeaders laptopToken) "{}"
        mintedCode <-
          case decode (simpleBody inviteResp) :: Maybe CreateInviteResponse of
            Just r  -> return (cresInviteCode r)
            Nothing -> do
              liftIO $ expectationFailure "invite response did not decode"
              error "unreachable"
        request "POST" "/register" jsonHeaders (registerBody "phone" mintedCode)
          `shouldRespondWith` 200

      it "honors an explicit expires_in_hours override" $ do
        token <- registerFreshDevice dbRef
        request "POST" "/invite" (bearerHeaders token) "{\"expires_in_hours\":1}"
          `shouldRespondWith` 200

-- --- fixtures ------------------------------------------------------

-- | A UUID that is syntactically valid but never corresponds to a
-- real registered device. Used in requests where @device_id@
-- matters for JSON shape but not for auth (auth flows through the
-- bearer token, not the body field).
fakeDeviceId :: Text
fakeDeviceId = "00000000-0000-0000-0000-000000000000"
