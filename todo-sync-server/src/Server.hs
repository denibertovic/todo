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
import qualified Data.UUID as UUID

import           Control.Monad.IO.Class  (liftIO)
import           Data.Time.Clock         (getCurrentTime)
import           Network.Wai             (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant

import           Database
import           Types

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

-- | Handle sync request
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

  -- Get operations since last sync
  ops <- liftIO $ withDb dbPath $ \conn ->
    getOperationsSince conn srLastSync

  -- Return response
  now <- liftIO getCurrentTime
  return SyncResponse
    { sresOperations = ops
    , sresSyncTime   = now
    }

-- | Handle device registration
handleRegister :: FilePath -> RegisterRequest -> Handler RegisterResponse
handleRegister dbPath RegisterRequest{..} = do
  -- First create the device to get the device ID
  (deviceId, authToken) <- liftIO $ withDb dbPath $ \conn ->
    createDevice conn rrDeviceName

  -- Validate and consume the invite code
  let DeviceId deviceUuid = deviceId
      deviceIdText = UUID.toText deviceUuid
  valid <- liftIO $ withDb dbPath $ \conn ->
    validateAndConsumeInviteCode conn rrInviteCode deviceIdText

  unless valid $ do
    -- TODO: ideally we'd rollback the device creation, but for now just reject
    throwError err403 { errBody = "Invalid or expired invite code" }

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
