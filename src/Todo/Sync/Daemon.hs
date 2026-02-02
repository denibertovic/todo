{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Sync.Daemon
  ( -- * Daemon Control
    startSyncDaemon
  , stopSyncDaemon
  , SyncDaemon
    -- * Manual Sync
  , triggerSync
  , performSync
    -- * File Watching
  , watchTodoFiles
  ) where

import           RIO hiding (atomically, newTVarIO, readTVar, writeTVar, modifyTVar',
                            async, cancel, threadDelay)
import qualified RIO.Text as T

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.STM    (TVar, atomically, newTVarIO,
                                           readTVar, writeTVar)
import           Control.Concurrent.Async  (Async, async, cancel)
import           Control.Monad             (forever, void)
import           Data.Time.Clock           (getCurrentTime)
import           Prelude                   (putStrLn)
import           System.IO                 (stdout)
import           System.Directory          (doesFileExist)
import           System.FilePath           (takeFileName)
import           System.FSNotify           (Event(..), WatchManager, watchDir,
                                           startManager, stopManager)
import qualified Data.Text.IO              as TIO
import           Text.Parsec               (parse)

import           Todo.Types                (TodoConfig(..), SyncConfig(..), Todo, TodoItem)
import           Todo.Parser               (todoParser)
import           Todo.Sync.Types
import           Todo.Sync.CRDT
import           Todo.Sync.Store
import           Todo.Sync.Client

-- | Sync daemon state
data SyncDaemon = SyncDaemon
  { sdConfig       :: !TodoConfig
  , sdWatchManager :: !WatchManager
  , sdSyncThread   :: Async ()  -- Lazy! Set after async starts
  , sdState        :: !(TVar SyncState)
  , sdLastContent  :: !(TVar T.Text)
  , sdPendingSync  :: !(TVar Bool)
  }

-- | Start the sync daemon
startSyncDaemon :: TodoConfig -> IO SyncDaemon
startSyncDaemon config = do
  putStrLn "startSyncDaemon: starting..."
  hFlush stdout
  let syncCfg = todoSync config

  -- Load or initialize sync state
  maybeState <- loadSyncState (todoDir config)
  state <- case maybeState of
    Just s  -> return s
    Nothing -> initSyncState (todoDir config) (syncDeviceName syncCfg)

  -- Initialize TVars
  stateTVar <- newTVarIO state
  lastContentTVar <- newTVarIO ""
  pendingSyncTVar <- newTVarIO False

  -- Read initial file content and reconcile with sync state
  exists <- doesFileExist (todoFile config)
  when exists $ do
    content <- TIO.readFile (todoFile config)
    atomically $ writeTVar lastContentTVar content

    -- Reconcile: compare file content with sync state
    let parseResult = parse todoParser "todo.txt" content
    case parseResult of
      Left err -> putStrLn $ "Warning: Failed to parse todo.txt on startup: " <> show err
      Right fileTodos -> do
        now <- getCurrentTime
        currentState <- atomically $ readTVar stateTVar

        -- Generate operations for any differences between file and sync state
        let stateTodos = materializeToTodos (ssItems currentState)
        ops <- diffToOperations
                 (ssDeviceId currentState)
                 now
                 (ssItems currentState)
                 stateTodos
                 fileTodos

        unless (null ops) $ do
          putStrLn $ "Reconciling " <> show (length ops) <> " changes made while daemon was offline..."
          let newState = applyOperations currentState ops
              newState' = newState { ssPendingOps = ssPendingOps newState ++ ops }
          atomically $ writeTVar stateTVar newState'
          saveSyncState (todoDir config) newState'

          -- Sync immediately
          result <- doSync config stateTVar lastContentTVar
          case result of
            Left err -> putStrLn $ "Startup sync error: " <> show err
            Right _  -> putStrLn "Startup reconciliation complete."

  -- Start watch manager
  watchMgr <- startManager

  -- Start file watcher (pass components directly, not the daemon)
  watchTodoFiles config watchMgr stateTVar lastContentTVar pendingSyncTVar

  -- Create daemon structure (now fully initialized)
  daemonTVar <- newTVarIO undefined  -- Will hold the daemon for periodicSync

  let daemon = SyncDaemon
        { sdConfig       = config
        , sdWatchManager = watchMgr
        , sdSyncThread   = undefined  -- Set below
        , sdState        = stateTVar
        , sdLastContent  = lastContentTVar
        , sdPendingSync  = pendingSyncTVar
        }

  -- Start periodic sync thread
  putStrLn "startSyncDaemon: starting async thread..."
  hFlush stdout
  syncThread <- async $ periodicSync daemon
  putStrLn "startSyncDaemon: async thread started"
  hFlush stdout

  let finalDaemon = daemon { sdSyncThread = syncThread }
  putStrLn "startSyncDaemon: returning daemon"
  hFlush stdout
  return finalDaemon

-- | Stop the sync daemon
stopSyncDaemon :: SyncDaemon -> IO ()
stopSyncDaemon daemon = do
  stopManager (sdWatchManager daemon)
  cancel (sdSyncThread daemon)

-- | Watch todo files for changes
watchTodoFiles :: TodoConfig -> WatchManager -> TVar SyncState -> TVar T.Text -> TVar Bool -> IO ()
watchTodoFiles config watchMgr stateTVar lastContentTVar pendingSyncTVar = do
  let dir = todoDir config
      shouldWatch event = case event of
        Added path _ _     -> isTodoFile path
        Modified path _ _  -> isTodoFile path
        Removed path _ _   -> isTodoFile path
        Unknown path _ _ _ -> False
        _                  -> False  -- Handle any other event types

      isTodoFile path =
        let name = takeFileName path
        in name `elem` ["todo.txt", "done.txt"]

      handleEvent event = when (shouldWatch event) $ do
        handleFileChange config stateTVar lastContentTVar pendingSyncTVar

  void $ watchDir watchMgr dir (const True) handleEvent

-- | Handle a file change event
handleFileChange :: TodoConfig -> TVar SyncState -> TVar T.Text -> TVar Bool -> IO ()
handleFileChange config stateTVar lastContentTVar pendingSyncTVar = do
  let todoPath = todoFile config
  exists <- doesFileExist todoPath
  when exists $ do
    newContent <- TIO.readFile todoPath
    oldContent <- atomically $ readTVar lastContentTVar

    when (newContent /= oldContent) $ do
      atomically $ writeTVar lastContentTVar newContent

      -- Parse old and new content
      let parseResult = parse todoParser "todo.txt" newContent
      case parseResult of
        Left err -> putStrLn $ "Warning: Failed to parse todo.txt: " <> show err
        Right newTodos -> do
          -- Get current sync state
          state <- atomically $ readTVar stateTVar
          now <- getCurrentTime

          -- Generate operations from diff (compare sync state against file)
          let stateTodos = materializeToTodos (ssItems state)
          ops <- diffToOperations
                   (ssDeviceId state)
                   now
                   (ssItems state)
                   stateTodos
                   newTodos

          -- Apply operations and update state
          unless (null ops) $ do
            let newState = applyOperations state ops
                newState' = newState { ssPendingOps = ssPendingOps newState ++ ops }
            atomically $ writeTVar stateTVar newState'
            saveSyncState (todoDir config) newState'

            -- Trigger immediate sync
            putStrLn $ "File changed, syncing " <> show (length ops) <> " operations..."
            result <- doSync config stateTVar lastContentTVar
            case result of
              Left err -> putStrLn $ "Sync error: " <> show err
              Right _  -> do
                putStrLn "Sync complete."
                atomically $ writeTVar pendingSyncTVar False

-- | Periodic sync loop
periodicSync :: SyncDaemon -> IO ()
periodicSync daemon = do
  putStrLn "periodicSync: entered"
  hFlush stdout
  forever $ do
    let syncCfg = todoSync (sdConfig daemon)
        intervalMicros = syncIntervalSecs syncCfg * 1000000

    -- Wait for the interval
    putStrLn $ "Waiting " <> show (syncIntervalSecs syncCfg) <> " seconds..."
    hFlush stdout
    threadDelay intervalMicros

    -- Sync if enabled
    putStrLn $ "Sync enabled: " <> show (syncEnabled syncCfg)
    hFlush stdout
    when (syncEnabled syncCfg) $ do
      putStrLn "Periodic sync starting..."
      hFlush stdout
      -- Always sync periodically to pull remote changes
      result <- performSync daemon
      case result of
        Left err -> do
          putStrLn $ "Periodic sync error: " <> show err
          hFlush stdout
        Right _  -> do
          state <- atomically $ readTVar (sdState daemon)
          putStrLn $ "Periodic sync complete. Items: " <> show (length $ ssItems state)
          hFlush stdout
          atomically $ writeTVar (sdPendingSync daemon) False

-- | Trigger an immediate sync
triggerSync :: SyncDaemon -> IO (Either SyncError ())
triggerSync = performSync

-- | Perform a sync operation
performSync :: SyncDaemon -> IO (Either SyncError ())
performSync daemon = doSync (sdConfig daemon) (sdState daemon) (sdLastContent daemon)

-- | Core sync logic - can be called from file watcher or periodic sync
doSync :: TodoConfig -> TVar SyncState -> TVar T.Text -> IO (Either SyncError ())
doSync config stateTVar lastContentTVar = do
  let syncCfg = todoSync config

  if not (syncEnabled syncCfg)
    then return $ Left $ AuthError "Sync is not enabled"
    else do
      -- Get current state
      state <- atomically $ readTVar stateTVar

      -- Build sync request
      let pendingOps = ssPendingOps state
          syncReq = SyncRequest
            { srDeviceId   = ssDeviceId state
            , srLastSync   = ssLastSync state
            , srOperations = pendingOps
            , srAuthToken  = syncAuthToken syncCfg
            }

      -- Send sync request
      result <- syncWithServer (syncServerUrl syncCfg) (syncAuthToken syncCfg) syncReq
      case result of
        Left err -> return $ Left err
        Right SyncResponse{..} -> do
          putStrLn $ "Received " <> show (length sresOperations) <> " operations from server"
          -- Merge remote operations
          let mergedOps = mergeOperations (ssOperations state) sresOperations
              newState = state
                { ssOperations = mergedOps
                , ssLastSync   = Just sresSyncTime
                , ssPendingOps = []  -- Clear pending ops after successful sync
                , ssItems      = materialize (ssDeviceId state) mergedOps
                }

          -- Update state
          atomically $ writeTVar stateTVar newState
          saveSyncState (todoDir config) newState

          -- Write updated todos back to file
          let todos = materializeToTodos (ssItems newState)
          TIO.writeFile (todoFile config) $ T.pack $ show todos

          -- Update last content to avoid triggering file watcher
          newContent <- TIO.readFile (todoFile config)
          atomically $ writeTVar lastContentTVar newContent

          return $ Right ()
