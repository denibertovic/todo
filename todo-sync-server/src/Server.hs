{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Server
  ( -- * Server
    runServer
  , SyncAPI
  , syncServer
  ) where

import           RIO hiding (Handler)
import qualified RIO.Text as T
import           Prelude (putStrLn)

import           Data.Time.Clock          (NominalDiffTime)
import           Network.Wai.Handler.Warp (Settings, defaultSettings,
                                          runSettings, setMaximumBodyFlush,
                                          setPort)
import           Servant

import           Database
import           Middleware
import           Types

-- | Maximum number of operations returned in a single /sync response.
-- Clients are expected to keep calling /sync while @has_more@ is true.
defaultPageSize :: Int
defaultPageSize = 500

-- | Maximum request body size, in bytes (1 MiB).
--
-- A full 500-op sync page is roughly ~100 KB, so 1 MiB gives
-- comfortable headroom while still bounding memory usage from
-- misbehaving clients.
maxRequestBodyBytes :: Word64
maxRequestBodyBytes = 1024 * 1024

-- | Default expiry for invite codes minted via @POST /invite@ when
-- the caller doesn't supply an explicit override.
defaultInviteExpiryHours :: Int
defaultInviteExpiryHours = 24

-- | The Sync API type.
--
-- * @/sync@ requires a bearer token in the @Authorization@ header.
-- * @/register@ uses a single-use invite code instead.
-- * @/invite@ also requires a bearer token: any registered device
--   can mint a fresh invite code for onboarding another device.
-- * @/health@ is unauthenticated.
type SyncAPI =
       "sync"
         :> Header "Authorization" T.Text
         :> ReqBody '[JSON] SyncRequest
         :> Post '[JSON] SyncResponse
  :<|> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] RegisterResponse
  :<|> "invite"
         :> Header "Authorization" T.Text
         :> ReqBody '[JSON] CreateInviteRequest
         :> Post '[JSON] CreateInviteResponse
  :<|> "health" :> Get '[JSON] HealthResponse

-- | API proxy for type inference
syncAPI :: Proxy SyncAPI
syncAPI = Proxy

-- | Server implementation
syncServer :: FilePath -> Server SyncAPI
syncServer dbPath =
       handleSync dbPath
  :<|> handleRegister dbPath
  :<|> handleCreateInvite dbPath
  :<|> handleHealth

-- | Extract a bearer token from an @Authorization@ header value.
-- Returns @Nothing@ if the header is absent or malformed.
bearerToken :: Maybe T.Text -> Maybe T.Text
bearerToken Nothing = Nothing
bearerToken (Just header) =
  let trimmed = T.strip header
  in case T.stripPrefix "Bearer " trimmed of
       Just tok -> let t = T.strip tok in if T.null t then Nothing else Just t
       Nothing  -> Nothing

-- | Handle sync request.
--
-- Authentication is mandatory. The token is preferred from the
-- @Authorization: Bearer <token>@ header, falling back to the
-- @auth_token@ body field for older clients. A missing or invalid
-- token results in @401@ with no side effects — the request body is
-- not written to the operation log until auth has succeeded.
--
-- Returns at most 'defaultPageSize' operations per call; if more are
-- available, @has_more@ is true and the client should re-issue /sync
-- with the returned @cursor@.
handleSync :: FilePath -> Maybe T.Text -> SyncRequest -> Handler SyncResponse
handleSync dbPath authHeader SyncRequest{..} = do
  -- Validate auth token (mandatory). Prefer Authorization header;
  -- fall back to the body field for backwards compatibility.
  let mToken = bearerToken authHeader <|> srAuthToken
  token <- case mToken of
    Just t | not (T.null t) -> return t
    _ -> throwError err401 { errBody = "Missing auth token" }

  maybeDevice <- liftIO $ withDb dbPath $ \conn ->
    getDeviceByToken conn token
  device <- case maybeDevice of
    Just d  -> return d
    Nothing -> throwError err401 { errBody = "Invalid auth token" }

  -- Update device last seen
  liftIO $ withDb dbPath $ \conn ->
    updateDeviceLastSeen conn (dbDeviceId device)

  -- Store incoming operations
  liftIO $ withDb dbPath $ \conn ->
    insertOperations conn srOperations

  -- Fetch one page of operations after the client's cursor
  page <- liftIO $ withDb dbPath $ \conn ->
    getOperationsAfterCursor conn srCursor defaultPageSize

  return SyncResponse
    { sresOperations = poOperations page
    , sresCursor     = poNextCursor page
    , sresHasMore    = poHasMore page
    }

-- | Handle device registration.
--
-- The invite code is validated and the device row is created in a
-- single database transaction, so an invalid/expired code cannot leave
-- an orphaned device behind.
handleRegister :: FilePath -> RegisterRequest -> Handler RegisterResponse
handleRegister dbPath RegisterRequest{..} = do
  result <- liftIO $ withDb dbPath $ \conn ->
    registerDeviceWithInviteCode conn rrDeviceName rrInviteCode
  case result of
    Nothing ->
      throwError err403 { errBody = "Invalid or expired invite code" }
    Just (deviceId, authToken) ->
      return RegisterResponse
        { rresDeviceId  = deviceId
        , rresAuthToken = authToken
        }

-- | Mint a new invite code on behalf of an already-registered
-- device. The bearer token is validated the same way it is for
-- @/sync@ — any registered device can create invites, which is
-- the intended authorization model for a single-user sync setup.
-- For multi-tenant deployments this would need to grow a
-- per-device permission bit; out of scope here.
handleCreateInvite :: FilePath -> Maybe T.Text -> CreateInviteRequest -> Handler CreateInviteResponse
handleCreateInvite dbPath authHeader CreateInviteRequest{..} = do
  token <- case bearerToken authHeader of
    Just t | not (T.null t) -> return t
    _ -> throwError err401 { errBody = "Missing auth token" }

  maybeDevice <- liftIO $ withDb dbPath $ \conn ->
    getDeviceByToken conn token
  device <- case maybeDevice of
    Just d  -> return d
    Nothing -> throwError err401 { errBody = "Invalid auth token" }

  -- Update last-seen so the admin CLI's `list-invite-codes` view
  -- reflects recent activity of the calling device.
  liftIO $ withDb dbPath $ \conn ->
    updateDeviceLastSeen conn (dbDeviceId device)

  let hours = fromMaybe defaultInviteExpiryHours cirExpiresInHours
      expirySeconds = fromIntegral (hours * 3600) :: NominalDiffTime
  (code, expiresAt) <- liftIO $ createInviteCodeWithExpiry dbPath expirySeconds
  return CreateInviteResponse
    { cresInviteCode = code
    , cresExpiresAt  = expiresAt
    }

-- | Handle health check
handleHealth :: Handler HealthResponse
handleHealth = return HealthResponse { hrStatus = "ok" }

-- | Create WAI application. Middleware is layered outside-in:
--
-- * 'registerRateLimiter' throttles @/register@ attempts per remote IP
-- * 'requestSizeLimiter' rejects request bodies over 'maxRequestBodyBytes'
--
-- Both run before Servant touches the request.
syncApp :: FilePath -> RateLimiter -> Application
syncApp dbPath limiter =
    registerRateLimiter limiter
  $ requestSizeLimiter maxRequestBodyBytes
  $ serve syncAPI (syncServer dbPath)

-- | Warp settings with a hard cap on body size for the idle flush
-- path. The real enforcement happens in 'requestSizeLimiter', but
-- having Warp itself bail out early on unreasonably large uploads is
-- cheap defense in depth.
warpSettings :: Int -> Settings
warpSettings port =
    setPort port
  $ setMaximumBodyFlush (Just (fromIntegral maxRequestBodyBytes))
  $ defaultSettings

-- | Run the server
runServer :: ServerConfig -> IO ()
runServer ServerConfig{..} = do
  putStrLn $ "Initializing database at: " <> scDbPath
  initDatabase scDbPath

  limiter <- newRateLimiter

  putStrLn $ "Starting sync server on port " <> show scPort
  runSettings (warpSettings scPort) (syncApp scDbPath limiter)
