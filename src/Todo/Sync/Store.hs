{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Sync.Store
  ( -- * Sync Directory Management
    getSyncDir
  , ensureSyncDir
    -- * State Persistence
  , loadSyncState
  , saveSyncState
  , initSyncState
    -- * Operation Log
  , appendOperation
  , loadOperationLog
  , saveOperationLog
    -- * Item Mapping
  , loadItemMap
  , saveItemMap
  , ItemMap
    -- * Migration
  , migrateExistingTodos
  ) where

import           RIO
import qualified RIO.Text as T
import           Prelude             (putStrLn)

import           Control.Monad         (unless)
import           Data.Aeson            (eitherDecodeFileStrict, encodeFile)
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Map.Strict       as Map
import           Data.Time.Clock       (UTCTime, getCurrentTime)
import           System.Directory      (createDirectoryIfMissing, doesFileExist,
                                        doesDirectoryExist)
import           System.FilePath       ((</>))

import           Todo.Types            (TodoItem(..), Todo(..))
import           Todo.Sync.Types
import           Todo.Sync.CRDT

-- | Type alias for item fingerprint to ItemId mapping
type ItemMap = Map.Map T.Text ItemId

-- | Get the sync directory path for a given todo directory
getSyncDir :: FilePath -> FilePath
getSyncDir todoDir = todoDir </> ".todo-sync"

-- | Ensure the sync directory exists
ensureSyncDir :: FilePath -> IO ()
ensureSyncDir todoDir = do
  let syncDir = getSyncDir todoDir
  createDirectoryIfMissing True syncDir

-- | Path to the sync state file
statePath :: FilePath -> FilePath
statePath todoDir = getSyncDir todoDir </> "state.json"

-- | Path to the operation log file
operationLogPath :: FilePath -> FilePath
operationLogPath todoDir = getSyncDir todoDir </> "operations.json"

-- | Path to the item map file (fingerprint -> ItemId)
itemMapPath :: FilePath -> FilePath
itemMapPath todoDir = getSyncDir todoDir </> "item_map.json"

-- | Load sync state from disk, or return Nothing if not initialized
loadSyncState :: FilePath -> IO (Maybe SyncState)
loadSyncState todoDir = do
  let path = statePath todoDir
  exists <- doesFileExist path
  if exists
    then do
      result <- eitherDecodeFileStrict path
      case result of
        Right state -> return $ Just state
        Left err    -> do
          putStrLn $ "Warning: Failed to load sync state: " <> err
          return Nothing
    else return Nothing

-- | Save sync state to disk
saveSyncState :: FilePath -> SyncState -> IO ()
saveSyncState todoDir state = do
  ensureSyncDir todoDir
  encodeFile (statePath todoDir) state

-- | Initialize sync state for a new device
initSyncState :: FilePath -> T.Text -> IO SyncState
initSyncState todoDir deviceName = do
  ensureSyncDir todoDir
  deviceId <- generateDeviceId
  let state = emptySyncState deviceId
  saveSyncState todoDir state
  return state

-- | Append an operation to the operation log
appendOperation :: FilePath -> Operation -> IO ()
appendOperation todoDir op = do
  ensureSyncDir todoDir
  ops <- loadOperationLog todoDir
  saveOperationLog todoDir (op : ops)

-- | Load operation log from disk
loadOperationLog :: FilePath -> IO [Operation]
loadOperationLog todoDir = do
  let path = operationLogPath todoDir
  exists <- doesFileExist path
  if exists
    then do
      result <- eitherDecodeFileStrict path
      case result of
        Right ops -> return ops
        Left err  -> do
          putStrLn $ "Warning: Failed to load operation log: " <> err
          return []
    else return []

-- | Save operation log to disk
saveOperationLog :: FilePath -> [Operation] -> IO ()
saveOperationLog todoDir ops = do
  ensureSyncDir todoDir
  encodeFile (operationLogPath todoDir) ops

-- | Load item map from disk
loadItemMap :: FilePath -> IO ItemMap
loadItemMap todoDir = do
  let path = itemMapPath todoDir
  exists <- doesFileExist path
  if exists
    then do
      result <- eitherDecodeFileStrict path
      case result of
        Right itemMap -> return itemMap
        Left err      -> do
          putStrLn $ "Warning: Failed to load item map: " <> err
          return Map.empty
    else return Map.empty

-- | Save item map to disk
saveItemMap :: FilePath -> ItemMap -> IO ()
saveItemMap todoDir itemMap = do
  ensureSyncDir todoDir
  encodeFile (itemMapPath todoDir) itemMap

-- | Migrate existing todos to the sync system
-- Assigns ItemIds to all existing todos and creates initial operations
migrateExistingTodos :: FilePath -> DeviceId -> [Todo TodoItem] -> IO (SyncState, ItemMap)
migrateExistingTodos todoDir deviceId todos = do
  now <- getCurrentTime

  -- Generate ItemIds and operations for each todo
  (ops, itemMap) <- foldM (processTodo now) ([], Map.empty) todos

  -- Create initial sync state
  let initialState = emptySyncState deviceId
      appliedState = applyOperations initialState (reverse ops)
      -- Set pending ops so they get sent to server on first sync
      finalState = appliedState { ssPendingOps = ssOperations appliedState }

  -- Save everything
  saveSyncState todoDir finalState
  saveItemMap todoDir itemMap
  saveOperationLog todoDir (ssOperations finalState)

  return (finalState, itemMap)
  where
    processTodo now (ops, itemMap) todo = do
      let item = case todo of
            Completed i  -> i
            Incomplete i -> i
          fp = itemFingerprint todo

      -- Check if we already have an ItemId for this fingerprint
      case Map.lookup fp itemMap of
        Just _  -> return (ops, itemMap)  -- Already exists, skip
        Nothing -> do
          itemId <- generateItemId
          addOp <- createAddOp deviceId itemId item now

          -- If completed, also create a complete operation
          completeOps <- case todo of
            Completed _ -> do
              completeOp <- createCompleteOp deviceId itemId now
              return [completeOp]
            Incomplete _ -> return []

          let newOps = completeOps ++ [addOp] ++ ops
              newItemMap = Map.insert fp itemId itemMap

          return (newOps, newItemMap)

-- | Update item map with new items from operations
updateItemMapFromOps :: ItemMap -> [Operation] -> [Todo TodoItem] -> ItemMap
updateItemMapFromOps itemMap ops todos =
  let newItems = [ (op, todo)
                 | op@OpAdd{} <- ops
                 , todo <- todos
                 , matchesOp op todo
                 ]
  in foldl' addToMap itemMap newItems
  where
    matchesOp OpAdd{..} todo =
      let item = case todo of
            Completed i  -> i
            Incomplete i -> i
      in tDescription opItem == tDescription item
    matchesOp _ _ = False

    addToMap m (OpAdd{..}, todo) = Map.insert (itemFingerprint todo) opItemId m
    addToMap m _ = m
