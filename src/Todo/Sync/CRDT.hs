{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Sync.CRDT
  ( -- * Operation Application
    applyOperation
  , applyOperations
    -- * Operation Merging
  , mergeOperations
  , deduplicateOperations
    -- * State Materialization
  , materialize
  , materializeToTodos
    -- * Operation Generation
  , createAddOp
  , createCompleteOp
  , createUncompleteOp
  , createDeleteOp
  , createSetPriorityOp
  , createModifyDescriptionOp
    -- * Diff Detection
  , diffToOperations
  , itemFingerprint
    -- * Utilities
  , syncItemToTodo
  , filterPendingOps
  ) where

import           RIO
import qualified RIO.Text as T

import qualified Crypto.Hash.SHA256    as SHA256
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map.Strict       as Map
import           Data.List             (sortBy, nubBy)
import           Data.Time.Clock       (UTCTime)
import qualified Data.Text.Encoding    as TE

import           Todo.Types            (Priority, TodoItem(..), Todo(..), Metadata)
import           Todo.Sync.Types

-- | Apply a single operation to the sync state
applyOperation :: SyncState -> Operation -> SyncState
applyOperation state op = state { ssItems = updatedItems, ssOperations = op : ssOperations state }
  where
    updatedItems = case op of
      OpAdd{..} ->
        let newItem = SyncItem
              { siItemId      = opItemId
              , siItem        = opItem
              , siCompleted   = False
              , siDeleted     = False
              , siPriority    = LWWRegister (tPriority opItem) opTimestamp opDeviceId
              , siDescription = LWWRegister (tDescription opItem) opTimestamp opDeviceId
              , siCreatedAt   = opTimestamp
              }
        in Map.insertWith mergeItems opItemId newItem (ssItems state)

      OpComplete{..} ->
        Map.adjust (\item -> item { siCompleted = True }) opItemId (ssItems state)

      OpUncomplete{..} ->
        Map.adjust (\item -> item { siCompleted = False }) opItemId (ssItems state)

      OpDelete{..} ->
        Map.adjust (\item -> item { siDeleted = True }) opItemId (ssItems state)

      OpSetPriority{..} ->
        Map.adjust (updatePriority opPriority opTimestamp opDeviceId) opItemId (ssItems state)

      OpModifyDescription{..} ->
        Map.adjust (updateDescription opDescription opTimestamp opDeviceId) opItemId (ssItems state)

    -- When adding an item that already exists, merge using LWW for conflicting fields
    mergeItems new old = old
      { siPriority    = mergeLWW (siPriority new) (siPriority old)
      , siDescription = mergeLWW (siDescription new) (siDescription old)
      -- Complete-wins: if either is completed, stay completed
      , siCompleted   = siCompleted new || siCompleted old
      -- Delete-wins: if either is deleted, stay deleted
      , siDeleted     = siDeleted new || siDeleted old
      }

    updatePriority pri timestamp deviceId item =
      let newReg = LWWRegister pri timestamp deviceId
      in item { siPriority = mergeLWW newReg (siPriority item) }

    updateDescription desc timestamp deviceId item =
      let newReg = LWWRegister desc timestamp deviceId
      in item { siDescription = mergeLWW newReg (siDescription item) }

-- | Apply multiple operations to sync state
applyOperations :: SyncState -> [Operation] -> SyncState
applyOperations = foldl' applyOperation

-- | Merge two operation lists, removing duplicates by operation ID
-- Operations are commutative and idempotent
mergeOperations :: [Operation] -> [Operation] -> [Operation]
mergeOperations ops1 ops2 = deduplicateOperations (ops1 ++ ops2)

-- | Remove duplicate operations based on operation ID
deduplicateOperations :: [Operation] -> [Operation]
deduplicateOperations = nubBy (\a b -> opId a == opId b)

-- | Materialize current state from an operation log
-- Starts from empty state and applies all operations in order
materialize :: DeviceId -> [Operation] -> Map.Map ItemId SyncItem
materialize deviceId ops =
  let sortedOps = sortBy compareByTime ops
      emptyState = emptySyncState deviceId
      finalState = applyOperations emptyState sortedOps
  in ssItems finalState
  where
    compareByTime a b = compare (operationTimestamp a) (operationTimestamp b)

-- | Convert materialized state to list of Todo items (excluding deleted)
materializeToTodos :: Map.Map ItemId SyncItem -> [Todo TodoItem]
materializeToTodos items =
  [ syncItemToTodo item
  | item <- Map.elems items
  , not (siDeleted item)
  ]

-- | Convert a SyncItem to a Todo TodoItem
syncItemToTodo :: SyncItem -> Todo TodoItem
syncItemToTodo SyncItem{..} =
  let item = siItem
        { tPriority    = lwwValue siPriority
        , tDescription = lwwValue siDescription
        }
  in if siCompleted
     then Completed item
     else Incomplete item

-- | Create an Add operation
createAddOp :: DeviceId -> ItemId -> TodoItem -> UTCTime -> IO Operation
createAddOp deviceId itemId item timestamp = do
  opId' <- generateOperationId
  return $ OpAdd opId' itemId item timestamp deviceId

-- | Create a Complete operation
createCompleteOp :: DeviceId -> ItemId -> UTCTime -> IO Operation
createCompleteOp deviceId itemId timestamp = do
  opId' <- generateOperationId
  return $ OpComplete opId' itemId timestamp deviceId

-- | Create an Uncomplete operation
createUncompleteOp :: DeviceId -> ItemId -> UTCTime -> IO Operation
createUncompleteOp deviceId itemId timestamp = do
  opId' <- generateOperationId
  return $ OpUncomplete opId' itemId timestamp deviceId

-- | Create a Delete operation
createDeleteOp :: DeviceId -> ItemId -> UTCTime -> IO Operation
createDeleteOp deviceId itemId timestamp = do
  opId' <- generateOperationId
  return $ OpDelete opId' itemId timestamp deviceId

-- | Create a SetPriority operation
createSetPriorityOp :: DeviceId -> ItemId -> Maybe Priority -> UTCTime -> IO Operation
createSetPriorityOp deviceId itemId priority timestamp = do
  opId' <- generateOperationId
  return $ OpSetPriority opId' itemId priority timestamp deviceId

-- | Create a ModifyDescription operation
createModifyDescriptionOp :: DeviceId -> ItemId -> T.Text -> UTCTime -> IO Operation
createModifyDescriptionOp deviceId itemId description timestamp = do
  opId' <- generateOperationId
  return $ OpModifyDescription opId' itemId description timestamp deviceId

-- | Compute a fingerprint for a todo item based on its content
-- Used for change detection when external edits occur
itemFingerprint :: Todo TodoItem -> T.Text
itemFingerprint todo =
  let item = case todo of
        Completed i  -> i
        Incomplete i -> i
      content = T.concat
        [ tDescription item
        , T.pack $ show $ tMetadata item
        ]
      hash = B16.encode $ SHA256.hash $ TE.encodeUtf8 content
  in TE.decodeUtf8 hash

-- | Diff old vs new todo lists to generate operations
-- This is used when detecting external file changes
diffToOperations
  :: DeviceId
  -> UTCTime
  -> Map.Map ItemId SyncItem   -- ^ Current sync state (with ItemIds)
  -> [Todo TodoItem]            -- ^ Old todos from file
  -> [Todo TodoItem]            -- ^ New todos from file
  -> IO [Operation]
diffToOperations deviceId timestamp currentState oldTodos newTodos = do
  let oldFingerprints = Map.fromList [(itemFingerprint t, t) | t <- oldTodos]
      newFingerprints = Map.fromList [(itemFingerprint t, t) | t <- newTodos]

      -- Items that exist in new but not old (added)
      addedFingerprints = Map.keys $ Map.difference newFingerprints oldFingerprints

      -- Items that exist in old but not new (removed or modified)
      removedFingerprints = Map.keys $ Map.difference oldFingerprints newFingerprints

  -- For added items, create OpAdd operations
  addOps <- forM addedFingerprints $ \fp -> do
    case Map.lookup fp newFingerprints of
      Just todo -> do
        let item = case todo of
              Completed i  -> i
              Incomplete i -> i
        itemId <- generateItemId
        createAddOp deviceId itemId item timestamp
      Nothing -> error "Impossible: fingerprint not found"

  -- For removed items, we need to find the corresponding ItemId
  -- and create appropriate operations (delete or complete)
  deleteOps <- catMaybes <$> forM removedFingerprints (\fp -> do
    case Map.lookup fp oldFingerprints of
      Just oldTodo -> do
        -- Find the ItemId for this item in our sync state
        let matchingItems = [ (itemId, si)
                            | (itemId, si) <- Map.toList currentState
                            , itemFingerprint (syncItemToTodo si) == fp
                            ]
        case matchingItems of
          ((itemId, _):_) -> Just <$> createDeleteOp deviceId itemId timestamp
          [] -> return Nothing  -- Item not in sync state, ignore
      Nothing -> return Nothing)

  -- Check for completion status changes
  completionOps <- catMaybes <$> forM (Map.toList currentState) (\(itemId, si) -> do
    let fp = itemFingerprint (syncItemToTodo si)
        oldCompleted = case Map.lookup fp oldFingerprints of
          Just (Completed _)  -> True
          Just (Incomplete _) -> False
          Nothing             -> siCompleted si
        newCompleted = case Map.lookup fp newFingerprints of
          Just (Completed _)  -> True
          Just (Incomplete _) -> False
          Nothing             -> siCompleted si

    if oldCompleted /= newCompleted
      then if newCompleted
           then Just <$> createCompleteOp deviceId itemId timestamp
           else Just <$> createUncompleteOp deviceId itemId timestamp
      else return Nothing)

  return $ addOps ++ deleteOps ++ completionOps

-- | Filter operations that haven't been synced yet
filterPendingOps :: Maybe UTCTime -> [Operation] -> [Operation]
filterPendingOps Nothing ops = ops
filterPendingOps (Just lastSync) ops =
  filter (\op -> operationTimestamp op > lastSync) ops
