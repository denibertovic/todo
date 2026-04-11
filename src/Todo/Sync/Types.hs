{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Sync.Types
  ( -- * Core Identifiers
    ItemId(..)
  , DeviceId(..)
  , OperationId(..)
    -- * Operations
  , Operation(..)
  , OperationType(..)
    -- * LWW Register for conflict resolution
  , LWWRegister(..)
  , mergeLWW
    -- * Sync State
  , SyncState(..)
  , SyncItem(..)
  , emptySyncState
    -- * API Types
  , SyncRequest(..)
  , SyncResponse(..)
  , SyncCursor(..)
  , RegisterRequest(..)
  , RegisterResponse(..)
  , HealthResponse(..)
    -- * Utilities
  , generateItemId
  , generateDeviceId
  , generateOperationId
  , operationItemId
  , operationTimestamp
  , operationDeviceId
  ) where

import           RIO
import qualified RIO.Text as T

import           Data.Aeson          (FromJSON (..), FromJSONKey (..),
                                      ToJSON (..), ToJSONKey (..), Value (..),
                                      object, withObject, withText, (.:), (.:?),
                                      (.=))
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Map.Strict     as Map
import           Data.Time.Clock     (UTCTime)
import           Data.UUID           (UUID)
import qualified Data.UUID           as UUID
import qualified Data.UUID.V4        as UUID

import           Todo.Types          (Priority, TodoItem)

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

instance ToJSONKey ItemId where
  toJSONKey = JSON.toJSONKeyText (\(ItemId uuid) -> UUID.toText uuid)

instance FromJSONKey ItemId where
  fromJSONKey = JSON.FromJSONKeyTextParser $ \t ->
    case UUID.fromText t of
      Nothing   -> fail "Invalid UUID format for ItemId key"
      Just uuid -> return $ ItemId uuid

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

-- | Operation type enum for serialization
data OperationType
  = OpTypeAdd
  | OpTypeComplete
  | OpTypeDelete
  | OpTypeSetPriority
  | OpTypeModifyDescription
  | OpTypeUncomplete
  deriving (Eq, Show, Generic)

instance ToJSON OperationType where
  toJSON OpTypeAdd               = "add"
  toJSON OpTypeComplete          = "complete"
  toJSON OpTypeDelete            = "delete"
  toJSON OpTypeSetPriority       = "set_priority"
  toJSON OpTypeModifyDescription = "modify_description"
  toJSON OpTypeUncomplete        = "uncomplete"

instance FromJSON OperationType where
  parseJSON = withText "OperationType" $ \case
    "add"                -> return OpTypeAdd
    "complete"           -> return OpTypeComplete
    "delete"             -> return OpTypeDelete
    "set_priority"       -> return OpTypeSetPriority
    "modify_description" -> return OpTypeModifyDescription
    "uncomplete"         -> return OpTypeUncomplete
    other                -> fail $ "Unknown operation type: " <> T.unpack other

-- | Operation log entry - the core CRDT unit
data Operation
  = OpAdd
      { opId          :: !OperationId
      , opItemId      :: !ItemId
      , opItem        :: !TodoItem
      , opTimestamp   :: !UTCTime
      , opDeviceId    :: !DeviceId
      }
  | OpComplete
      { opId          :: !OperationId
      , opItemId      :: !ItemId
      , opTimestamp   :: !UTCTime
      , opDeviceId    :: !DeviceId
      }
  | OpUncomplete
      { opId          :: !OperationId
      , opItemId      :: !ItemId
      , opTimestamp   :: !UTCTime
      , opDeviceId    :: !DeviceId
      }
  | OpDelete
      { opId          :: !OperationId
      , opItemId      :: !ItemId
      , opTimestamp   :: !UTCTime
      , opDeviceId    :: !DeviceId
      }
  | OpSetPriority
      { opId          :: !OperationId
      , opItemId      :: !ItemId
      , opPriority    :: !(Maybe Priority)
      , opTimestamp   :: !UTCTime
      , opDeviceId    :: !DeviceId
      }
  | OpModifyDescription
      { opId          :: !OperationId
      , opItemId      :: !ItemId
      , opDescription :: !T.Text
      , opTimestamp   :: !UTCTime
      , opDeviceId    :: !DeviceId
      }
  deriving (Eq, Show, Generic)

instance ToJSON Operation where
  toJSON op = object $ common ++ specific
    where
      common =
        [ "op_id"     .= opId op
        , "item_id"   .= operationItemId op
        , "timestamp" .= operationTimestamp op
        , "device_id" .= operationDeviceId op
        ]
      specific = case op of
        OpAdd{..}               -> ["type" .= OpTypeAdd, "item" .= opItem]
        OpComplete{}            -> ["type" .= OpTypeComplete]
        OpUncomplete{}          -> ["type" .= OpTypeUncomplete]
        OpDelete{}              -> ["type" .= OpTypeDelete]
        OpSetPriority{..}       -> ["type" .= OpTypeSetPriority, "priority" .= opPriority]
        OpModifyDescription{..} -> ["type" .= OpTypeModifyDescription, "description" .= opDescription]

instance FromJSON Operation where
  parseJSON = withObject "Operation" $ \o -> do
    opType    <- o .: "type"
    opId'     <- o .: "op_id"
    itemId'   <- o .: "item_id"
    timestamp <- o .: "timestamp"
    deviceId' <- o .: "device_id"
    case opType of
      OpTypeAdd -> do
        item <- o .: "item"
        return $ OpAdd opId' itemId' item timestamp deviceId'
      OpTypeComplete ->
        return $ OpComplete opId' itemId' timestamp deviceId'
      OpTypeUncomplete ->
        return $ OpUncomplete opId' itemId' timestamp deviceId'
      OpTypeDelete ->
        return $ OpDelete opId' itemId' timestamp deviceId'
      OpTypeSetPriority -> do
        priority <- o .:? "priority"
        return $ OpSetPriority opId' itemId' priority timestamp deviceId'
      OpTypeModifyDescription -> do
        desc <- o .: "description"
        return $ OpModifyDescription opId' itemId' desc timestamp deviceId'

instance Ord Operation where
  compare a b = compare (operationTimestamp a, operationDeviceId a)
                        (operationTimestamp b, operationDeviceId b)

-- | Last-Writer-Wins register for conflict resolution
data LWWRegister a = LWWRegister
  { lwwValue  :: !a
  , lwwTime   :: !UTCTime
  , lwwDevice :: !DeviceId
  } deriving (Eq, Show, Generic, Functor)

instance ToJSON a => ToJSON (LWWRegister a) where
  toJSON LWWRegister{..} = object
    [ "value"     .= lwwValue
    , "timestamp" .= lwwTime
    , "device_id" .= lwwDevice
    ]

instance FromJSON a => FromJSON (LWWRegister a) where
  parseJSON = withObject "LWWRegister" $ \o -> do
    lwwValue  <- o .: "value"
    lwwTime   <- o .: "timestamp"
    lwwDevice <- o .: "device_id"
    return LWWRegister{..}

-- | Merge two LWW registers - most recent timestamp wins, device ID as tiebreaker
mergeLWW :: LWWRegister a -> LWWRegister a -> LWWRegister a
mergeLWW a b
  | lwwTime a > lwwTime b = a
  | lwwTime a < lwwTime b = b
  | otherwise = if lwwDevice a > lwwDevice b then a else b

-- | A synchronized todo item with CRDT metadata
data SyncItem = SyncItem
  { siItemId      :: !ItemId
  , siItem        :: !TodoItem
  , siCompleted   :: !Bool
  , siDeleted     :: !Bool
  , siPriority    :: !(LWWRegister (Maybe Priority))
  , siDescription :: !(LWWRegister T.Text)
  , siCreatedAt   :: !UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON SyncItem
instance FromJSON SyncItem

-- | Local sync state.
--
-- @ssServerCursor@ is the opaque cursor returned by the server's last
-- /sync response; we send it back on the next sync to pick up where we
-- left off. It replaces the old @ssLastSync@ timestamp.
data SyncState = SyncState
  { ssItems        :: !(Map.Map ItemId SyncItem)  -- ^ Current materialized state
  , ssOperations   :: ![Operation]                -- ^ Operation log (newest first)
  , ssServerCursor :: !(Maybe SyncCursor)         -- ^ Cursor for the next /sync call
  , ssDeviceId     :: !DeviceId                   -- ^ This device's ID
  , ssPendingOps   :: ![Operation]                -- ^ Operations not yet synced
  } deriving (Eq, Show, Generic)

instance ToJSON SyncState where
  toJSON SyncState{..} = object
    [ "ssItems"        .= ssItems
    , "ssOperations"   .= ssOperations
    , "ssServerCursor" .= ssServerCursor
    , "ssDeviceId"     .= ssDeviceId
    , "ssPendingOps"   .= ssPendingOps
    ]

-- | Tolerant decoder: falls back to @Nothing@ for @ssServerCursor@ if
-- the on-disk state was written by an older version that stored
-- @ssLastSync@ instead. The CRDT is idempotent, so the next sync will
-- simply re-pull all operations once and move on.
instance FromJSON SyncState where
  parseJSON = withObject "SyncState" $ \o -> do
    ssItems        <- o .:  "ssItems"
    ssOperations   <- o .:  "ssOperations"
    ssServerCursor <- o .:? "ssServerCursor"
    ssDeviceId     <- o .:  "ssDeviceId"
    ssPendingOps   <- o .:  "ssPendingOps"
    return SyncState{..}

-- | Create an empty sync state for a new device
emptySyncState :: DeviceId -> SyncState
emptySyncState deviceId = SyncState
  { ssItems        = Map.empty
  , ssOperations   = []
  , ssServerCursor = Nothing
  , ssDeviceId     = deviceId
  , ssPendingOps   = []
  }

-- | Pagination cursor for the /sync endpoint. Must match the
-- server-side definition in 'Types.SyncCursor'.
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

-- | Sync API request
data SyncRequest = SyncRequest
  { srDeviceId   :: !DeviceId
  , srCursor     :: !(Maybe SyncCursor)
  , srOperations :: ![Operation]
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

-- | Sync API response. The server may return up to a bounded number
-- of operations per call; when @sresHasMore@ is true the client should
-- immediately re-issue /sync using @sresCursor@ as @srCursor@.
data SyncResponse = SyncResponse
  { sresOperations :: ![Operation]
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
    sresOperations <- o .:  "operations"
    sresCursor     <- o .:? "cursor"
    sresHasMore    <- o .:  "has_more"
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

-- | Generate a new random ItemId
generateItemId :: IO ItemId
generateItemId = ItemId <$> UUID.nextRandom

-- | Generate a new random DeviceId
generateDeviceId :: IO DeviceId
generateDeviceId = DeviceId <$> UUID.nextRandom

-- | Generate a new random OperationId
generateOperationId :: IO OperationId
generateOperationId = OperationId <$> UUID.nextRandom

-- | Extract the item ID from any operation
operationItemId :: Operation -> ItemId
operationItemId OpAdd{..}               = opItemId
operationItemId OpComplete{..}          = opItemId
operationItemId OpUncomplete{..}        = opItemId
operationItemId OpDelete{..}            = opItemId
operationItemId OpSetPriority{..}       = opItemId
operationItemId OpModifyDescription{..} = opItemId

-- | Extract the timestamp from any operation
operationTimestamp :: Operation -> UTCTime
operationTimestamp OpAdd{..}               = opTimestamp
operationTimestamp OpComplete{..}          = opTimestamp
operationTimestamp OpUncomplete{..}        = opTimestamp
operationTimestamp OpDelete{..}            = opTimestamp
operationTimestamp OpSetPriority{..}       = opTimestamp
operationTimestamp OpModifyDescription{..} = opTimestamp

-- | Extract the device ID from any operation
operationDeviceId :: Operation -> DeviceId
operationDeviceId OpAdd{..}               = opDeviceId
operationDeviceId OpComplete{..}          = opDeviceId
operationDeviceId OpUncomplete{..}        = opDeviceId
operationDeviceId OpDelete{..}            = opDeviceId
operationDeviceId OpSetPriority{..}       = opDeviceId
operationDeviceId OpModifyDescription{..} = opDeviceId
