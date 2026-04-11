{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Sync.StoreSpec where

import           RIO
import qualified RIO.Text as T
import           Prelude (head, read)

import qualified Data.Map.Strict       as Map
import           Data.Time.Calendar    (fromGregorian)
import           Data.Time.Clock       (UTCTime(..), secondsToDiffTime)
import           Data.UUID             (nil)
import           System.Directory      (doesDirectoryExist, doesFileExist)

import           Test.Hspec            (Spec, describe, it, shouldBe, shouldReturn)

import           Todo.Types            (TodoItem(..), Todo(..), Priority(..))
import           Todo.Sync.Types
import           Todo.Sync.Store
import           Todo.Sync.CRDT        (applyOperations)

-- | Required for auto-discovery
spec :: Spec
spec = describe "Sync Store" $ do

  describe "ensureSyncDir" $ do
    it "creates .todo-sync directory if it doesn't exist" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        ensureSyncDir tmpDir
        doesDirectoryExist (getSyncDir tmpDir) `shouldReturn` True

    it "getSyncDir returns correct path" $ do
      getSyncDir "/home/user/todo" `shouldBe` "/home/user/todo/.todo-sync"

  describe "SyncState persistence" $ do
    it "saveSyncState and loadSyncState roundtrip" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        let state = emptySyncState device1
        saveSyncState tmpDir state
        loaded <- loadSyncState tmpDir
        loaded `shouldBe` Just state

    it "loadSyncState returns Nothing for non-existent state" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        loaded <- loadSyncState tmpDir
        loaded `shouldBe` Nothing

    it "persists state with items" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        let syncItem = SyncItem
              { siItemId      = itemId1
              , siItem        = testItem
              , siCompleted   = False
              , siDeleted     = False
              , siPriority    = LWWRegister Nothing time1 device1
              , siDescription = LWWRegister "Test" time1 device1
              , siCreatedAt   = time1
              }
            state = SyncState
              { ssItems        = Map.singleton itemId1 syncItem
              , ssOperations   = []
              , ssServerCursor = Just (SyncCursor time1 opId1)
              , ssDeviceId     = device1
              , ssPendingOps   = []
              }
        saveSyncState tmpDir state
        loaded <- loadSyncState tmpDir
        loaded `shouldBe` Just state

  describe "initSyncState" $ do
    it "creates a new empty sync state" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        state <- initSyncState tmpDir "test-device"
        Map.null (ssItems state) `shouldBe` True
        ssServerCursor state `shouldBe` Nothing
        null (ssOperations state) `shouldBe` True

    it "saves the state to disk" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        _ <- initSyncState tmpDir "test-device"
        loaded <- loadSyncState tmpDir
        loaded `shouldSatisfy` isJust

  describe "Operation log persistence" $ do
    it "saveOperationLog and loadOperationLog roundtrip" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        ensureSyncDir tmpDir
        let ops = [ OpAdd opId1 itemId1 testItem time1 device1
                  , OpComplete opId2 itemId1 time2 device1
                  ]
        saveOperationLog tmpDir ops
        loaded <- loadOperationLog tmpDir
        loaded `shouldBe` ops

    it "loadOperationLog returns empty list for non-existent log" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        loaded <- loadOperationLog tmpDir
        loaded `shouldBe` []

    it "appendOperation adds to the log" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        ensureSyncDir tmpDir
        let op1 = OpAdd opId1 itemId1 testItem time1 device1
            op2 = OpComplete opId2 itemId1 time2 device1
        appendOperation tmpDir op1
        appendOperation tmpDir op2
        loaded <- loadOperationLog tmpDir
        -- Note: appendOperation prepends, so newest first
        length loaded `shouldBe` 2

  describe "Item map persistence" $ do
    it "saveItemMap and loadItemMap roundtrip" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        ensureSyncDir tmpDir
        let itemMap = Map.fromList
              [ ("fingerprint1", itemId1)
              , ("fingerprint2", itemId2)
              ]
        saveItemMap tmpDir itemMap
        loaded <- loadItemMap tmpDir
        loaded `shouldBe` itemMap

    it "loadItemMap returns empty map for non-existent file" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        loaded <- loadItemMap tmpDir
        loaded `shouldBe` Map.empty

  describe "migrateExistingTodos" $ do
    it "creates operations for existing todos" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        let todos = [ Incomplete testItem
                    , Incomplete testItem2
                    ]
        (state, itemMap) <- migrateExistingTodos tmpDir device1 todos
        -- Should have created add operations for each todo
        Map.size (ssItems state) `shouldBe` 2
        Map.size itemMap `shouldBe` 2

    it "creates complete operations for completed todos" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        let todos = [ Completed testItem ]
        (state, _) <- migrateExistingTodos tmpDir device1 todos
        -- The item should be marked as completed
        let items = Map.elems (ssItems state)
        length items `shouldBe` 1
        siCompleted (head items) `shouldBe` True

    it "saves state and item map to disk" $ do
      withSystemTempDirectory "todo-store-test" $ \tmpDir -> do
        let todos = [Incomplete testItem]
        _ <- migrateExistingTodos tmpDir device1 todos
        -- Verify files were created
        stateLoaded <- loadSyncState tmpDir
        stateLoaded `shouldSatisfy` isJust
        mapLoaded <- loadItemMap tmpDir
        Map.size mapLoaded `shouldBe` 1

-- Helper
shouldSatisfy :: (Show a) => a -> (a -> Bool) -> IO ()
shouldSatisfy x p = if p x then return () else error $ "Expected predicate to hold for: " ++ show x

-- Test fixtures

device1 :: DeviceId
device1 = DeviceId nil

itemId1 :: ItemId
itemId1 = ItemId nil

itemId2 :: ItemId
itemId2 = ItemId $ read "00000000-0000-0000-0000-000000000001"

opId1 :: OperationId
opId1 = OperationId nil

opId2 :: OperationId
opId2 = OperationId $ read "00000000-0000-0000-0000-000000000001"

time1 :: UTCTime
time1 = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)

time2 :: UTCTime
time2 = UTCTime (fromGregorian 2024 1 2) (secondsToDiffTime 0)

testItem :: TodoItem
testItem = TodoItem
  { tPriority    = Nothing
  , tDescription = "Test todo item"
  , tMetadata    = []
  , tCreatedAt   = Nothing
  , tCompletedAt = Nothing
  }

testItem2 :: TodoItem
testItem2 = TodoItem
  { tPriority    = Just A
  , tDescription = "Another test item"
  , tMetadata    = []
  , tCreatedAt   = Nothing
  , tCompletedAt = Nothing
  }
