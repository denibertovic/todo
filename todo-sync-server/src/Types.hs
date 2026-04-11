{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types
  ( -- * Identifiers
    DeviceId(..)
  , OperationId(..)
  , ItemId(..)
    -- * API Types
  , SyncRequest(..)
  , SyncResponse(..)
  , SyncCursor(..)
  , RegisterRequest(..)
  , RegisterResponse(..)
  , HealthResponse(..)
    -- * Operation
  , Operation(..)
    -- * Server Config
  , ServerConfig(..)
  , defaultServerConfig
  ) where

import           RIO
import qualified RIO.Text as T

import           Data.Aeson            (FromJSON (..), ToJSON (..), Value (..),
                                        object, withObject, withText, (.:), (.:?),
                                        (.=))
import qualified Data.Aeson            as JSON
import           Data.Time.Clock       (UTCTime)
import           Data.UUID             (UUID)
import qualified Data.UUID             as UUID

-- | Unique identifier for each device
newtype DeviceId = DeviceId UUID
  deriving (Eq, Ord, Show, Generic)

instance ToJSON DeviceId where
  toJSON (DeviceId uuid) = JSON.String $ UUID.toText uuid

instance FromJSON DeviceId where
  parseJSON = withText "DeviceId" $ \t ->
    case UUID.fromText t of
      Nothing   -> fail "Invalid UUID format for DeviceId"
      Just uuid -> return $ DeviceId uuid

-- | Unique identifier for each operation
newtype OperationId = OperationId UUID
  deriving (Eq, Ord, Show, Generic)

instance ToJSON OperationId where
  toJSON (OperationId uuid) = JSON.String $ UUID.toText uuid

instance FromJSON OperationId where
  parseJSON = withText "OperationId" $ \t ->
    case UUID.fromText t of
      Nothing   -> fail "Invalid UUID format for OperationId"
      Just uuid -> return $ OperationId uuid

-- | Unique identifier for each todo item
newtype ItemId = ItemId UUID
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ItemId where
  toJSON (ItemId uuid) = JSON.String $ UUID.toText uuid

instance FromJSON ItemId where
  parseJSON = withText "ItemId" $ \t ->
    case UUID.fromText t of
      Nothing   -> fail "Invalid UUID format for ItemId"
      Just uuid -> return $ ItemId uuid

-- | Generic operation stored as JSON
data Operation = Operation
  { operationId        :: !OperationId
  , operationDeviceId  :: !DeviceId
  , operationItemId    :: !ItemId
  , operationType      :: !T.Text
  , operationPayload   :: !Value
  , operationTimestamp :: !UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON Operation where
  toJSON Operation{..} = object
    [ "op_id"     .= operationId
    , "device_id" .= operationDeviceId
    , "item_id"   .= operationItemId
    , "type"      .= operationType
    , "payload"   .= operationPayload
    , "timestamp" .= operationTimestamp
    ]

instance FromJSON Operation where
  parseJSON = withObject "Operation" $ \o -> do
    operationId        <- o .: "op_id"
    operationDeviceId  <- o .: "device_id"
    operationItemId    <- o .: "item_id"
    operationType      <- o .: "type"
    operationPayload   <- o .: "payload"
    operationTimestamp <- o .: "timestamp"
    return Operation{..}

-- | Pagination cursor for the /sync endpoint.
--
-- Operations are totally ordered on the server by
-- @(created_at, op_id)@, where @created_at@ is the insertion time and
-- @op_id@ is the operation's UUID. Using a compound cursor is what lets
-- us paginate correctly when many operations share the same insertion
-- time (which is the common case, since 'insertOperations' stamps every
-- op in a batch with a single @now@).
data SyncCursor = SyncCursor
  { scTimestamp :: !UTCTime
  , scOpId      :: !OperationId
  } deriving (Eq, Show, Generic)

instance ToJSON SyncCursor where
  toJSON SyncCursor{..} = object
    [ "timestamp" .= scTimestamp
    , "op_id"     .= scOpId
    ]

instance FromJSON SyncCursor where
  parseJSON = withObject "SyncCursor" $ \o -> do
    scTimestamp <- o .: "timestamp"
    scOpId      <- o .: "op_id"
    return SyncCursor{..}

-- | Sync request from client
data SyncRequest = SyncRequest
  { srDeviceId   :: !DeviceId
  , srCursor     :: !(Maybe SyncCursor)
  , srOperations :: ![Value]  -- Raw JSON operations
  , srAuthToken  :: !(Maybe T.Text)
  } deriving (Eq, Show, Generic)

instance ToJSON SyncRequest where
  toJSON SyncRequest{..} = object
    [ "device_id"  .= srDeviceId
    , "cursor"     .= srCursor
    , "operations" .= srOperations
    , "auth_token" .= srAuthToken
    ]

instance FromJSON SyncRequest where
  parseJSON = withObject "SyncRequest" $ \o -> do
    srDeviceId   <- o .: "device_id"
    srCursor     <- o .:? "cursor"
    srOperations <- o .: "operations"
    srAuthToken  <- o .:? "auth_token"
    return SyncRequest{..}

-- | Sync response to client.
--
-- @sresCursor@ is the cursor the client should send on its next request
-- to pick up where this response left off. @sresHasMore@ is true when
-- there are further operations beyond this page, in which case the
-- client should immediately re-issue /sync with the new cursor.
data SyncResponse = SyncResponse
  { sresOperations :: ![Value]  -- Raw JSON operations
  , sresCursor     :: !(Maybe SyncCursor)
  , sresHasMore    :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON SyncResponse where
  toJSON SyncResponse{..} = object
    [ "operations" .= sresOperations
    , "cursor"     .= sresCursor
    , "has_more"   .= sresHasMore
    ]

instance FromJSON SyncResponse where
  parseJSON = withObject "SyncResponse" $ \o -> do
    sresOperations <- o .: "operations"
    sresCursor     <- o .:? "cursor"
    sresHasMore    <- o .: "has_more"
    return SyncResponse{..}

-- | Device registration request
data RegisterRequest = RegisterRequest
  { rrDeviceName  :: !T.Text
  , rrInviteCode  :: !T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON RegisterRequest where
  toJSON RegisterRequest{..} = object
    [ "device_name" .= rrDeviceName
    , "invite_code" .= rrInviteCode
    ]

instance FromJSON RegisterRequest where
  parseJSON = withObject "RegisterRequest" $ \o -> do
    rrDeviceName <- o .: "device_name"
    rrInviteCode <- o .: "invite_code"
    return RegisterRequest{..}

-- | Device registration response
data RegisterResponse = RegisterResponse
  { rresDeviceId  :: !DeviceId
  , rresAuthToken :: !T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON RegisterResponse where
  toJSON RegisterResponse{..} = object
    [ "device_id"  .= rresDeviceId
    , "auth_token" .= rresAuthToken
    ]

instance FromJSON RegisterResponse where
  parseJSON = withObject "RegisterResponse" $ \o -> do
    rresDeviceId  <- o .: "device_id"
    rresAuthToken <- o .: "auth_token"
    return RegisterResponse{..}

-- | Health check response
data HealthResponse = HealthResponse
  { hrStatus :: !T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON HealthResponse where
  toJSON HealthResponse{..} = object ["status" .= hrStatus]

instance FromJSON HealthResponse where
  parseJSON = withObject "HealthResponse" $ \o -> do
    hrStatus <- o .: "status"
    return HealthResponse{..}

-- | Server configuration
data ServerConfig = ServerConfig
  { scPort     :: !Int
  , scDbPath   :: !FilePath
  } deriving (Eq, Show)

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { scPort   = 8080
  , scDbPath = "todo-sync.db"
  }
