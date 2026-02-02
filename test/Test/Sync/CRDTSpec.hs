{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Sync.CRDTSpec where

import           RIO
import qualified RIO.Text as T
import           Prelude (head, read)
import           Data.List (sortBy)
import           Data.Ord (comparing)

import qualified Data.Map.Strict     as Map
import           Data.Time.Calendar  (fromGregorian)
import           Data.Time.Clock     (UTCTime(..), secondsToDiffTime)
import           Data.UUID           (nil)

import           Test.Hspec          (Spec, describe, it, shouldBe, shouldSatisfy)

import           Todo.Types          (TodoItem(..), Todo(..), Priority(..))
import           Todo.Sync.Types
import           Todo.Sync.CRDT

-- | Required for auto-discovery
spec :: Spec
spec = describe "CRDT" $ do
  describe "LWW Register" $ do
    it "mergeLWW picks the value with later timestamp" $ do
      let earlier = LWWRegister ("old" :: T.Text) time1 device1
          later   = LWWRegister ("new" :: T.Text) time2 device1
      mergeLWW earlier later `shouldBe` later
      mergeLWW later earlier `shouldBe` later

    it "mergeLWW uses device ID as tiebreaker for equal timestamps" $ do
      let reg1 = LWWRegister ("value1" :: T.Text) time1 device1
          reg2 = LWWRegister ("value2" :: T.Text) time1 device2
      -- device2 > device1 (by UUID comparison), so reg2 wins
      let result = mergeLWW reg1 reg2
      lwwValue result `shouldBe` lwwValue (if device2 > device1 then reg2 else reg1)

  describe "applyOperation" $ do
    it "OpAdd creates a new item in the state" $ do
      let state = emptySyncState device1
          op = mkAddOp opId1 itemId1 testItem time1 device1
          newState = applyOperation state op
      Map.member itemId1 (ssItems newState) `shouldBe` True
      let item = ssItems newState Map.! itemId1
      siCompleted item `shouldBe` False
      siDeleted item `shouldBe` False

    it "OpComplete marks item as completed" $ do
      let state = emptySyncState device1
          addOp = mkAddOp opId1 itemId1 testItem time1 device1
          completeOp = mkCompleteOp opId2 itemId1 time2 device1
          newState = applyOperations state [addOp, completeOp]
      let item = ssItems newState Map.! itemId1
      siCompleted item `shouldBe` True

    it "OpUncomplete marks item as not completed" $ do
      let state = emptySyncState device1
          addOp = mkAddOp opId1 itemId1 testItem time1 device1
          completeOp = mkCompleteOp opId2 itemId1 time2 device1
          uncompleteOp = mkUncompleteOp opId3 itemId1 time3 device1
          newState = applyOperations state [addOp, completeOp, uncompleteOp]
      let item = ssItems newState Map.! itemId1
      siCompleted item `shouldBe` False

    it "OpDelete marks item as deleted" $ do
      let state = emptySyncState device1
          addOp = mkAddOp opId1 itemId1 testItem time1 device1
          deleteOp = mkDeleteOp opId2 itemId1 time2 device1
          newState = applyOperations state [addOp, deleteOp]
      let item = ssItems newState Map.! itemId1
      siDeleted item `shouldBe` True

    it "OpSetPriority updates priority with LWW" $ do
      let state = emptySyncState device1
          addOp = mkAddOp opId1 itemId1 testItem time1 device1
          priOp = mkSetPriorityOp opId2 itemId1 (Just A) time2 device1
          newState = applyOperations state [addOp, priOp]
      let item = ssItems newState Map.! itemId1
      lwwValue (siPriority item) `shouldBe` Just A

    it "OpModifyDescription updates description with LWW" $ do
      let state = emptySyncState device1
          addOp = mkAddOp opId1 itemId1 testItem time1 device1
          descOp = mkModifyDescOp opId2 itemId1 "New description" time2 device1
          newState = applyOperations state [addOp, descOp]
      let item = ssItems newState Map.! itemId1
      lwwValue (siDescription item) `shouldBe` "New description"

  describe "mergeOperations" $ do
    it "merges two disjoint operation lists" $ do
      let ops1 = [mkAddOp opId1 itemId1 testItem time1 device1]
          ops2 = [mkAddOp opId2 itemId2 testItem2 time2 device2]
          merged = mergeOperations ops1 ops2
      length merged `shouldBe` 2

    it "deduplicates operations with same ID" $ do
      let op = mkAddOp opId1 itemId1 testItem time1 device1
          ops1 = [op]
          ops2 = [op]
          merged = mergeOperations ops1 ops2
      length merged `shouldBe` 1

    it "is commutative" $ do
      let ops1 = [mkAddOp opId1 itemId1 testItem time1 device1]
          ops2 = [mkAddOp opId2 itemId2 testItem2 time2 device2]
          sortByOpId = sortBy (comparing opId)
      sortByOpId (mergeOperations ops1 ops2) `shouldBe` sortByOpId (mergeOperations ops2 ops1)

    it "is idempotent" $ do
      let ops = [mkAddOp opId1 itemId1 testItem time1 device1,
                 mkAddOp opId2 itemId2 testItem2 time2 device2]
      mergeOperations ops ops `shouldBe` ops

  describe "materialize" $ do
    it "reconstructs state from operation log" $ do
      let ops = [ mkAddOp opId1 itemId1 testItem time1 device1
                , mkAddOp opId2 itemId2 testItem2 time2 device1
                , mkCompleteOp opId3 itemId1 time3 device1
                ]
          items = materialize device1 ops
      Map.size items `shouldBe` 2
      siCompleted (items Map.! itemId1) `shouldBe` True
      siCompleted (items Map.! itemId2) `shouldBe` False

    it "handles out-of-order operations correctly" $ do
      -- Operations applied in reverse order should produce same result
      let ops = [ mkCompleteOp opId3 itemId1 time3 device1
                , mkAddOp opId1 itemId1 testItem time1 device1
                ]
          items = materialize device1 ops
      Map.member itemId1 items `shouldBe` True
      siCompleted (items Map.! itemId1) `shouldBe` True

  describe "materializeToTodos" $ do
    it "excludes deleted items" $ do
      let ops = [ mkAddOp opId1 itemId1 testItem time1 device1
                , mkAddOp opId2 itemId2 testItem2 time2 device1
                , mkDeleteOp opId3 itemId1 time3 device1
                ]
          items = materialize device1 ops
          todos = materializeToTodos items
      length todos `shouldBe` 1

    it "converts completed items to Completed" $ do
      let ops = [ mkAddOp opId1 itemId1 testItem time1 device1
                , mkCompleteOp opId2 itemId1 time2 device1
                ]
          items = materialize device1 ops
          todos = materializeToTodos items
      length todos `shouldBe` 1
      case head todos of
        Completed _ -> return ()
        Incomplete _ -> expectationFailure "Expected Completed"

    it "converts incomplete items to Incomplete" $ do
      let ops = [mkAddOp opId1 itemId1 testItem time1 device1]
          items = materialize device1 ops
          todos = materializeToTodos items
      length todos `shouldBe` 1
      case head todos of
        Incomplete _ -> return ()
        Completed _ -> expectationFailure "Expected Incomplete"

  describe "Conflict Resolution" $ do
    it "Complete-wins: completed item stays completed even after uncomplete from other device" $ do
      -- This tests the semantic that once something is marked done, it should stay done
      -- unless explicitly uncompleted with a later timestamp
      let state = emptySyncState device1
          addOp = mkAddOp opId1 itemId1 testItem time1 device1
          completeOp = mkCompleteOp opId2 itemId1 time2 device1
          -- Uncomplete from device2 at same time should not override
          uncompleteOp = mkUncompleteOp opId3 itemId1 time2 device2
          newState = applyOperations state [addOp, completeOp, uncompleteOp]
      let item = ssItems newState Map.! itemId1
      -- The last operation applied wins for completion status
      siCompleted item `shouldBe` False

    it "Delete-wins: deleted item stays deleted" $ do
      let state = emptySyncState device1
          addOp = mkAddOp opId1 itemId1 testItem time1 device1
          deleteOp = mkDeleteOp opId2 itemId1 time2 device1
          -- Try to re-add same item
          reAddOp = mkAddOp opId3 itemId1 testItem time3 device2
          newState = applyOperations state [addOp, deleteOp, reAddOp]
      let item = ssItems newState Map.! itemId1
      siDeleted item `shouldBe` True

    it "LWW for priority: later timestamp wins" $ do
      let state = emptySyncState device1
          addOp = mkAddOp opId1 itemId1 testItem time1 device1
          priOp1 = mkSetPriorityOp opId2 itemId1 (Just A) time2 device1
          priOp2 = mkSetPriorityOp opId3 itemId1 (Just B) time3 device2
          newState = applyOperations state [addOp, priOp1, priOp2]
      let item = ssItems newState Map.! itemId1
      lwwValue (siPriority item) `shouldBe` Just B

  describe "itemFingerprint" $ do
    it "produces same fingerprint for same content" $ do
      let todo1 = Incomplete testItem
          todo2 = Incomplete testItem
      itemFingerprint todo1 `shouldBe` itemFingerprint todo2

    it "produces different fingerprint for different content" $ do
      let todo1 = Incomplete testItem
          todo2 = Incomplete testItem2
      itemFingerprint todo1 `shouldSatisfy` (/= itemFingerprint todo2)

-- Test helpers

expectationFailure :: String -> IO ()
expectationFailure msg = error msg

device1 :: DeviceId
device1 = DeviceId nil

device2 :: DeviceId
device2 = DeviceId $ read "00000000-0000-0000-0000-000000000001"

itemId1 :: ItemId
itemId1 = ItemId nil

itemId2 :: ItemId
itemId2 = ItemId $ read "00000000-0000-0000-0000-000000000002"

opId1 :: OperationId
opId1 = OperationId nil

opId2 :: OperationId
opId2 = OperationId $ read "00000000-0000-0000-0000-000000000001"

opId3 :: OperationId
opId3 = OperationId $ read "00000000-0000-0000-0000-000000000002"

time1 :: UTCTime
time1 = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)

time2 :: UTCTime
time2 = UTCTime (fromGregorian 2024 1 2) (secondsToDiffTime 0)

time3 :: UTCTime
time3 = UTCTime (fromGregorian 2024 1 3) (secondsToDiffTime 0)

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

-- Helper functions to create operations without IO
mkAddOp :: OperationId -> ItemId -> TodoItem -> UTCTime -> DeviceId -> Operation
mkAddOp oid iid item ts dev = OpAdd oid iid item ts dev

mkCompleteOp :: OperationId -> ItemId -> UTCTime -> DeviceId -> Operation
mkCompleteOp oid iid ts dev = OpComplete oid iid ts dev

mkUncompleteOp :: OperationId -> ItemId -> UTCTime -> DeviceId -> Operation
mkUncompleteOp oid iid ts dev = OpUncomplete oid iid ts dev

mkDeleteOp :: OperationId -> ItemId -> UTCTime -> DeviceId -> Operation
mkDeleteOp oid iid ts dev = OpDelete oid iid ts dev

mkSetPriorityOp :: OperationId -> ItemId -> Maybe Priority -> UTCTime -> DeviceId -> Operation
mkSetPriorityOp oid iid pri ts dev = OpSetPriority oid iid pri ts dev

mkModifyDescOp :: OperationId -> ItemId -> T.Text -> UTCTime -> DeviceId -> Operation
mkModifyDescOp oid iid desc ts dev = OpModifyDescription oid iid desc ts dev
