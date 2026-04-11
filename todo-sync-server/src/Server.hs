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

import           Control.Monad.IO.Class  (liftIO)
import           Network.Wai             (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant

import           Database
import           Types

-- | Maximum number of operations returned in a single /sync response.
-- Clients are expected to keep calling /sync while @has_more@ is true.
defaultPageSize :: Int
defaultPageSize = 500

-- | The Sync API type
type SyncAPI =
       "sync" :> ReqBody '[JSON] SyncRequest :> Post '[JSON] SyncResponse
  :<|> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] RegisterResponse
  :<|> "health" :> Get '[JSON] HealthResponse

-- | API proxy for type inference
syncAPI :: Proxy SyncAPI
syncAPI = Proxy

-- | Server implementation
syncServer :: FilePath -> Server SyncAPI
syncServer dbPath =
       handleSync dbPath
  :<|> handleRegister dbPath
  :<|> handleHealth

-- | Handle sync request.
--
-- Returns at most 'defaultPageSize' operations per call; if more are
-- available, @has_more@ is true and the client should re-issue /sync
-- with the returned @cursor@.
handleSync :: FilePath -> SyncRequest -> Handler SyncResponse
handleSync dbPath SyncRequest{..} = do
  -- Validate auth token if provided
  when (isJust srAuthToken) $ do
    maybeDevice <- liftIO $ withDb dbPath $ \conn ->
      getDeviceByToken conn (fromMaybe "" srAuthToken)
    when (isNothing maybeDevice) $
      throwError err401 { errBody = "Invalid auth token" }

    -- Update device last seen
    liftIO $ withDb dbPath $ \conn -> do
      case maybeDevice of
        Just device -> updateDeviceLastSeen conn (dbDeviceId device)
        Nothing     -> return ()

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

-- | Handle health check
handleHealth :: Handler HealthResponse
handleHealth = return HealthResponse { hrStatus = "ok" }

-- | Create WAI application
syncApp :: FilePath -> Application
syncApp dbPath = serve syncAPI (syncServer dbPath)

-- | Run the server
runServer :: ServerConfig -> IO ()
runServer ServerConfig{..} = do
  putStrLn $ "Initializing database at: " <> scDbPath
  initDatabase scDbPath

  putStrLn $ "Starting sync server on port " <> show scPort
  run scPort (syncApp scDbPath)
