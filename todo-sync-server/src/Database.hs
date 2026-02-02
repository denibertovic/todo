{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database
  ( -- * Connection Management
    initDatabase
  , withDb
    -- * Device Operations
  , createDevice
  , getDeviceByToken
  , updateDeviceLastSeen
    -- * Invite Code Operations
  , createInviteCode
  , validateAndConsumeInviteCode
  , listInviteCodes
    -- * Operation Storage
  , insertOperations
  , getOperationsSince
  , getAllOperations
    -- * Types
  , DbDevice(..)
  , DbOperation(..)
  , DbInviteCode(..)
  ) where

import           RIO
import qualified RIO.Text as T

import           Data.Aeson              (Value (..))
import qualified Data.Aeson              as JSON
import qualified Data.Aeson.KeyMap       as KM
import qualified Data.ByteString.Lazy    as BL
import           Data.Time.Clock         (UTCTime, getCurrentTime, addUTCTime, NominalDiffTime)
import           Data.UUID               (UUID)
import qualified Data.UUID               as UUID
import qualified Data.UUID.V4            as UUID
import           Database.SQLite.Simple
import           Prelude                 (head)

import           Types

-- | Device record from database
data DbDevice = DbDevice
  { dbDeviceId    :: !T.Text
  , dbDeviceName  :: !T.Text
  , dbAuthToken   :: !T.Text
  , dbLastSeen    :: !(Maybe UTCTime)
  } deriving (Eq, Show)

instance FromRow DbDevice where
  fromRow = DbDevice <$> field <*> field <*> field <*> field

instance ToRow DbDevice where
  toRow DbDevice{..} = toRow (dbDeviceId, dbDeviceName, dbAuthToken, dbLastSeen)

-- | Operation record from database
data DbOperation = DbOperation
  { dbOpId        :: !T.Text
  , dbOpDeviceId  :: !T.Text
  , dbOpType      :: !T.Text
  , dbOpItemId    :: !T.Text
  , dbOpPayload   :: !T.Text
  , dbOpCreatedAt :: !UTCTime
  } deriving (Eq, Show)

instance FromRow DbOperation where
  fromRow = DbOperation <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DbOperation where
  toRow DbOperation{..} = toRow
    (dbOpId, dbOpDeviceId, dbOpType, dbOpItemId, dbOpPayload, dbOpCreatedAt)

-- | Invite code record from database
data DbInviteCode = DbInviteCode
  { dbInviteCode      :: !T.Text
  , dbInviteCreatedAt :: !UTCTime
  , dbInviteExpiresAt :: !UTCTime
  , dbInviteUsedAt    :: !(Maybe UTCTime)
  , dbInviteUsedBy    :: !(Maybe T.Text)
  } deriving (Eq, Show)

instance FromRow DbInviteCode where
  fromRow = DbInviteCode <$> field <*> field <*> field <*> field <*> field

-- | Initialize the database schema
initDatabase :: FilePath -> IO ()
initDatabase dbPath = withDb dbPath $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS devices (\
    \  id TEXT PRIMARY KEY,\
    \  name TEXT NOT NULL,\
    \  auth_token TEXT NOT NULL UNIQUE,\
    \  last_seen TIMESTAMP\
    \)"

  execute_ conn
    "CREATE TABLE IF NOT EXISTS operations (\
    \  id TEXT PRIMARY KEY,\
    \  device_id TEXT NOT NULL,\
    \  operation_type TEXT NOT NULL,\
    \  item_id TEXT NOT NULL,\
    \  payload TEXT,\
    \  created_at TIMESTAMP NOT NULL,\
    \  FOREIGN KEY (device_id) REFERENCES devices(id)\
    \)"

  execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_ops_created ON operations(created_at)"

  execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_ops_device ON operations(device_id)"

  execute_ conn
    "CREATE TABLE IF NOT EXISTS invite_codes (\
    \  code TEXT PRIMARY KEY,\
    \  created_at TIMESTAMP NOT NULL,\
    \  expires_at TIMESTAMP NOT NULL,\
    \  used_at TIMESTAMP,\
    \  used_by_device_id TEXT,\
    \  FOREIGN KEY (used_by_device_id) REFERENCES devices(id)\
    \)"

-- | Run an action with a database connection
withDb :: FilePath -> (Connection -> IO a) -> IO a
withDb dbPath action = do
  conn <- open dbPath
  result <- action conn
  close conn
  return result

-- | Create a new device and return its ID and auth token
createDevice :: Connection -> T.Text -> IO (DeviceId, T.Text)
createDevice conn deviceName = do
  deviceUuid <- UUID.nextRandom
  tokenUuid <- UUID.nextRandom
  let deviceId = UUID.toText deviceUuid
      authToken = UUID.toText tokenUuid

  execute conn
    "INSERT INTO devices (id, name, auth_token, last_seen) VALUES (?, ?, ?, ?)"
    (deviceId, deviceName, authToken, Nothing :: Maybe UTCTime)

  return (DeviceId deviceUuid, authToken)

-- | Get device by auth token
getDeviceByToken :: Connection -> T.Text -> IO (Maybe DbDevice)
getDeviceByToken conn token = do
  results <- query conn
    "SELECT id, name, auth_token, last_seen FROM devices WHERE auth_token = ?"
    (Only token)
  return $ case results of
    [device] -> Just device
    _        -> Nothing

-- | Update device last seen timestamp
updateDeviceLastSeen :: Connection -> T.Text -> IO ()
updateDeviceLastSeen conn deviceId = do
  now <- getCurrentTime
  execute conn
    "UPDATE devices SET last_seen = ? WHERE id = ?"
    (now, deviceId)

-- | Insert multiple operations
insertOperations :: Connection -> [Value] -> IO ()
insertOperations conn ops = do
  now <- getCurrentTime
  forM_ ops $ \op -> do
    case extractOpFields op of
      Just (opId, deviceId, opType, itemId) -> do
        let payloadText = decodeUtf8Lenient $ BL.toStrict $ JSON.encode op
        execute conn
          "INSERT OR IGNORE INTO operations (id, device_id, operation_type, item_id, payload, created_at) \
          \VALUES (?, ?, ?, ?, ?, ?)"
          (opId, deviceId, opType, itemId, payloadText, now)
      Nothing -> return ()

-- | Extract fields from operation JSON
extractOpFields :: Value -> Maybe (T.Text, T.Text, T.Text, T.Text)
extractOpFields val = case val of
  Object obj -> do
    opId <- getTextField obj "op_id"
    deviceId <- getTextField obj "device_id"
    opType <- getTextField obj "type"
    itemId <- getTextField obj "item_id"
    return (opId, deviceId, opType, itemId)
  _ -> Nothing
  where
    getTextField obj key = case KM.lookup (fromString $ T.unpack key) obj of
      Just (String t) -> Just t
      _ -> Nothing

-- | Get operations since a given timestamp
getOperationsSince :: Connection -> Maybe UTCTime -> IO [Value]
getOperationsSince conn maybeTime = do
  results <- case maybeTime of
    Nothing -> query_ conn
      "SELECT payload FROM operations ORDER BY created_at ASC"
    Just time -> query conn
      "SELECT payload FROM operations WHERE created_at > ? ORDER BY created_at ASC"
      (Only time)
  return $ catMaybes $ map parsePayload results
  where
    parsePayload :: Only T.Text -> Maybe Value
    parsePayload (Only payload) =
      JSON.decode $ BL.fromStrict $ encodeUtf8 payload

-- | Get all operations
getAllOperations :: Connection -> IO [Value]
getAllOperations conn = getOperationsSince conn Nothing

-- | Create a new invite code with given expiry duration (in seconds)
createInviteCode :: FilePath -> NominalDiffTime -> IO T.Text
createInviteCode dbPath expirySeconds = withDb dbPath $ \conn -> do
  codeUuid <- UUID.nextRandom
  let code = UUID.toText codeUuid
  now <- getCurrentTime
  let expiresAt = addUTCTime expirySeconds now

  execute conn
    "INSERT INTO invite_codes (code, created_at, expires_at) VALUES (?, ?, ?)"
    (code, now, expiresAt)

  return code

-- | Validate an invite code and mark it as used. Returns Nothing if invalid/expired/used.
validateAndConsumeInviteCode :: Connection -> T.Text -> T.Text -> IO Bool
validateAndConsumeInviteCode conn code deviceId = do
  now <- getCurrentTime
  -- Check if code exists, not expired, and not used
  results <- query conn
    "SELECT code, created_at, expires_at, used_at, used_by_device_id \
    \FROM invite_codes \
    \WHERE code = ? AND expires_at > ? AND used_at IS NULL"
    (code, now) :: IO [DbInviteCode]

  case results of
    [_] -> do
      -- Mark as used
      execute conn
        "UPDATE invite_codes SET used_at = ?, used_by_device_id = ? WHERE code = ?"
        (now, deviceId, code)
      return True
    _ -> return False

-- | List all invite codes (for admin purposes)
listInviteCodes :: FilePath -> IO [DbInviteCode]
listInviteCodes dbPath = withDb dbPath $ \conn -> do
  query_ conn
    "SELECT code, created_at, expires_at, used_at, used_by_device_id \
    \FROM invite_codes ORDER BY created_at DESC"
