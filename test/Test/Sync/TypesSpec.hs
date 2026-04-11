{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Sync.TypesSpec where

import           RIO
import qualified RIO.Text as T

import           Data.Aeson          (decode, encode, eitherDecode)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict     as Map
import           Data.Time.Calendar  (fromGregorian)
import           Data.Time.Clock     (UTCTime(..), secondsToDiffTime)
import           Data.UUID           (nil)
import qualified Data.UUID           as UUID

import           Test.Hspec          (Spec, describe, it, shouldBe, shouldSatisfy)

import           Todo.Types          (TodoItem(..), Priority(..))
import           Todo.Sync.Types

-- | Required for auto-discovery
spec :: Spec
spec = describe "Sync Types JSON" $ do

  describe "ItemId" $ do
    it "serializes to JSON string" $ do
      let itemId = ItemId nil
          json = encode itemId
      json `shouldBe` "\"00000000-0000-0000-0000-000000000000\""

    it "deserializes from JSON string" $ do
      let json = "\"00000000-0000-0000-0000-000000000000\""
          result = decode json :: Maybe ItemId
      result `shouldBe` Just (ItemId nil)

    it "roundtrips through JSON" $ do
      let itemId = ItemId nil
      decode (encode itemId) `shouldBe` Just itemId

  describe "DeviceId" $ do
    it "roundtrips through JSON" $ do
      let deviceId = DeviceId nil
      decode (encode deviceId) `shouldBe` Just deviceId

  describe "OperationId" $ do
    it "roundtrips through JSON" $ do
      let opId = OperationId nil
      decode (encode opId) `shouldBe` Just opId

  describe "OperationType" $ do
    it "serializes OpTypeAdd correctly" $ do
      encode OpTypeAdd `shouldBe` "\"add\""

    it "serializes OpTypeComplete correctly" $ do
      encode OpTypeComplete `shouldBe` "\"complete\""

    it "serializes OpTypeDelete correctly" $ do
      encode OpTypeDelete `shouldBe` "\"delete\""

    it "deserializes operation types correctly" $ do
      decode "\"add\"" `shouldBe` Just OpTypeAdd
      decode "\"complete\"" `shouldBe` Just OpTypeComplete
      decode "\"delete\"" `shouldBe` Just OpTypeDelete
      decode "\"set_priority\"" `shouldBe` Just OpTypeSetPriority
      decode "\"modify_description\"" `shouldBe` Just OpTypeModifyDescription
      decode "\"uncomplete\"" `shouldBe` Just OpTypeUncomplete

  describe "Operation" $ do
    it "roundtrips OpAdd through JSON" $ do
      let op = OpAdd opId1 itemId1 testItem time1 device1
      decode (encode op) `shouldBe` Just op

    it "roundtrips OpComplete through JSON" $ do
      let op = OpComplete opId1 itemId1 time1 device1
      decode (encode op) `shouldBe` Just op

    it "roundtrips OpUncomplete through JSON" $ do
      let op = OpUncomplete opId1 itemId1 time1 device1
      decode (encode op) `shouldBe` Just op

    it "roundtrips OpDelete through JSON" $ do
      let op = OpDelete opId1 itemId1 time1 device1
      decode (encode op) `shouldBe` Just op

    it "roundtrips OpSetPriority through JSON" $ do
      let op = OpSetPriority opId1 itemId1 (Just A) time1 device1
      decode (encode op) `shouldBe` Just op

    it "roundtrips OpSetPriority with Nothing through JSON" $ do
      let op = OpSetPriority opId1 itemId1 Nothing time1 device1
      decode (encode op) `shouldBe` Just op

    it "roundtrips OpModifyDescription through JSON" $ do
      let op = OpModifyDescription opId1 itemId1 "New desc" time1 device1
      decode (encode op) `shouldBe` Just op

  describe "LWWRegister" $ do
    it "roundtrips through JSON" $ do
      let reg = LWWRegister ("test" :: T.Text) time1 device1
      decode (encode reg) `shouldBe` Just reg

    it "roundtrips with Maybe value through JSON" $ do
      let reg = LWWRegister (Just A) time1 device1
      decode (encode reg) `shouldBe` Just reg

  describe "SyncItem" $ do
    it "roundtrips through JSON" $ do
      let item = SyncItem
            { siItemId      = itemId1
            , siItem        = testItem
            , siCompleted   = False
            , siDeleted     = False
            , siPriority    = LWWRegister Nothing time1 device1
            , siDescription = LWWRegister "Test" time1 device1
            , siCreatedAt   = time1
            }
      decode (encode item) `shouldBe` Just item

  describe "SyncState" $ do
    it "roundtrips empty state through JSON" $ do
      let state = emptySyncState device1
      decode (encode state) `shouldBe` Just state

    it "roundtrips state with items through JSON" $ do
      let syncItem = SyncItem
            { siItemId      = itemId1
            , siItem        = testItem
            , siCompleted   = False
            , siDeleted     = False
            , siPriority    = LWWRegister Nothing time1 device1
            , siDescription = LWWRegister "Test" time1 device1
            , siCreatedAt   = time1
            }
          cursor1 = SyncCursor time1 opId1
          state = SyncState
            { ssItems        = Map.singleton itemId1 syncItem
            , ssOperations   = []
            , ssServerCursor = Just cursor1
            , ssDeviceId     = device1
            , ssPendingOps   = []
            }
      decode (encode state) `shouldBe` Just state

  describe "SyncCursor" $ do
    it "roundtrips through JSON" $ do
      let cursor = SyncCursor time1 opId1
      decode (encode cursor) `shouldBe` Just cursor

  describe "SyncRequest" $ do
    it "roundtrips through JSON" $ do
      let req = SyncRequest
            { srDeviceId   = device1
            , srCursor     = Just (SyncCursor time1 opId1)
            , srOperations = [OpAdd opId1 itemId1 testItem time1 device1]
            , srAuthToken  = Just "token123"
            }
      decode (encode req) `shouldBe` Just req

    it "handles missing optional fields" $ do
      let req = SyncRequest
            { srDeviceId   = device1
            , srCursor     = Nothing
            , srOperations = []
            , srAuthToken  = Nothing
            }
      decode (encode req) `shouldBe` Just req

  describe "SyncResponse" $ do
    it "roundtrips through JSON with cursor + has_more" $ do
      let resp = SyncResponse
            { sresOperations = [OpComplete opId1 itemId1 time1 device1]
            , sresCursor     = Just (SyncCursor time1 opId1)
            , sresHasMore    = True
            }
      decode (encode resp) `shouldBe` Just resp

    it "roundtrips through JSON with no cursor" $ do
      let resp = SyncResponse
            { sresOperations = []
            , sresCursor     = Nothing
            , sresHasMore    = False
            }
      decode (encode resp) `shouldBe` Just resp

  describe "RegisterRequest" $ do
    it "roundtrips through JSON" $ do
      let req = RegisterRequest
            { rrDeviceName = "my-laptop"
            , rrInviteCode = "abc123-def456"
            }
      decode (encode req) `shouldBe` Just req

  describe "RegisterResponse" $ do
    it "roundtrips through JSON" $ do
      let resp = RegisterResponse
            { rresDeviceId  = device1
            , rresAuthToken = "auth-token-123"
            }
      decode (encode resp) `shouldBe` Just resp

  describe "CreateInviteRequest" $ do
    it "roundtrips through JSON with expiry override" $ do
      let req = CreateInviteRequest { cirExpiresInHours = Just 6 }
      decode (encode req) `shouldBe` Just req

    it "roundtrips through JSON without expiry override" $ do
      let req = CreateInviteRequest { cirExpiresInHours = Nothing }
      decode (encode req) `shouldBe` Just req

    it "decodes a body with no fields as Nothing" $ do
      -- The CLI sends {} when the user doesn't pass --expires-in.
      -- Make sure the server accepts that without complaining.
      let result = decode "{}" :: Maybe CreateInviteRequest
      result `shouldBe` Just (CreateInviteRequest { cirExpiresInHours = Nothing })

  describe "CreateInviteResponse" $ do
    it "roundtrips through JSON" $ do
      let resp = CreateInviteResponse
            { cresInviteCode = "11111111-2222-3333-4444-555555555555"
            , cresExpiresAt  = time1
            }
      decode (encode resp) `shouldBe` Just resp

    it "uses snake_case wire field names" $ do
      -- Regression guard: the server and Android client both
      -- expect 'invite_code' / 'expires_at', not the raw Haskell
      -- field names.
      let resp = CreateInviteResponse
            { cresInviteCode = "abc"
            , cresExpiresAt  = time1
            }
          encoded = BL.toStrict $ encode resp
      BS.isInfixOf "invite_code" encoded `shouldBe` True
      BS.isInfixOf "expires_at" encoded `shouldBe` True

  describe "HealthResponse" $ do
    it "roundtrips through JSON" $ do
      let resp = HealthResponse { hrStatus = "ok" }
      decode (encode resp) `shouldBe` Just resp

  describe "TodoItem JSON" $ do
    it "roundtrips TodoItem through JSON" $ do
      decode (encode testItem) `shouldBe` Just testItem

    it "roundtrips TodoItem with priority through JSON" $ do
      let item = testItem { tPriority = Just A }
      decode (encode item) `shouldBe` Just item

-- Test fixtures

device1 :: DeviceId
device1 = DeviceId nil

itemId1 :: ItemId
itemId1 = ItemId nil

opId1 :: OperationId
opId1 = OperationId nil

time1 :: UTCTime
time1 = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)

testItem :: TodoItem
testItem = TodoItem
  { tPriority    = Nothing
  , tDescription = "Test todo item"
  , tMetadata    = []
  , tCreatedAt   = Nothing
  , tCompletedAt = Nothing
  }
