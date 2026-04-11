{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo.Sync.Client
  ( -- * Sync Operations
    syncWithServer
  , syncAllPages
  , registerDevice
  , createInvite
  , checkServerHealth
    -- * Client Types
  , SyncError(..)
  , SyncResult
  ) where

import           RIO hiding (try)
import qualified RIO.Text as T

import           Control.Exception     (SomeException, try)
import           Data.Aeson            (encode, eitherDecode)
import qualified Data.ByteString.Lazy  as BL
import           Network.HTTP.Client   (Manager, Request, Response,
                                        httpLbs, newManager, parseRequest,
                                        requestBody, requestHeaders, method,
                                        responseBody, responseStatus,
                                        RequestBody(..))
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types    (Status(..), statusCode)

import           Todo.Sync.Types

-- | Errors that can occur during sync
data SyncError
  = NetworkError T.Text
  | ServerError Int T.Text
  | ParseError T.Text
  | AuthError T.Text
  deriving (Eq, Show)

-- | Result of a sync operation
type SyncResult a = Either SyncError a

-- | Create a new HTTP manager
createManager :: IO Manager
createManager = newManager tlsManagerSettings

-- | Build a request with common headers
buildRequest :: T.Text -> T.Text -> Maybe T.Text -> BL.ByteString -> IO Request
buildRequest baseUrl path authToken body = do
  let url = T.unpack $ baseUrl <> path
  req <- parseRequest url
  return $ req
    { method = "POST"
    , requestBody = RequestBodyLBS body
    , requestHeaders =
        [ ("Content-Type", "application/json")
        , ("Accept", "application/json")
        ] ++ maybe [] (\t -> [("Authorization", "Bearer " <> encodeUtf8 t)]) authToken
    }

-- | Sync operations with the server
syncWithServer
  :: T.Text           -- ^ Server URL
  -> Maybe T.Text     -- ^ Auth token
  -> SyncRequest      -- ^ Sync request
  -> IO (SyncResult SyncResponse)
syncWithServer serverUrl authToken syncReq = do
  manager <- createManager
  result <- try $ do
    req <- buildRequest serverUrl "/sync" authToken (encode syncReq)
    resp <- httpLbs req manager
    return resp
  case result of
    Left (e :: SomeException) ->
      return $ Left $ NetworkError $ T.pack $ show e
    Right resp -> do
      let status = statusCode $ responseStatus resp
          body = responseBody resp
      if status == 200
        then case eitherDecode body of
          Right syncResp -> return $ Right syncResp
          Left err       -> return $ Left $ ParseError $ T.pack err
        else if status == 401
          then return $ Left $ AuthError "Invalid or expired auth token"
          else return $ Left $ ServerError status $ T.pack $ show body

-- | Repeatedly call /sync until the server reports @has_more = false@,
-- accumulating every returned operation. Pending operations from the
-- caller are only sent on the *first* call; subsequent pagination
-- rounds send an empty pending list with the latest cursor.
--
-- Returns the merged list of operations received across all pages,
-- together with the final cursor to persist.
syncAllPages
  :: T.Text           -- ^ Server URL
  -> Maybe T.Text     -- ^ Auth token
  -> SyncRequest      -- ^ Initial sync request (pending ops go here)
  -> IO (SyncResult ([Operation], Maybe SyncCursor))
syncAllPages serverUrl authToken initialReq = go initialReq []
  where
    go req acc = do
      result <- syncWithServer serverUrl authToken req
      case result of
        Left err -> return $ Left err
        Right SyncResponse{..}
          | sresHasMore ->
              let nextReq = req
                    { srCursor     = sresCursor
                    , srOperations = []  -- only send pending ops on first call
                    }
              in go nextReq (acc ++ sresOperations)
          | otherwise ->
              return $ Right (acc ++ sresOperations, sresCursor)

-- | Register a new device with the server
registerDevice
  :: T.Text           -- ^ Server URL
  -> T.Text           -- ^ Device name
  -> T.Text           -- ^ Invite code
  -> IO (SyncResult RegisterResponse)
registerDevice serverUrl deviceName inviteCode = do
  manager <- createManager
  let regReq = RegisterRequest deviceName inviteCode
  result <- try $ do
    req <- buildRequest serverUrl "/register" Nothing (encode regReq)
    resp <- httpLbs req manager
    return resp
  case result of
    Left (e :: SomeException) ->
      return $ Left $ NetworkError $ T.pack $ show e
    Right resp -> do
      let status = statusCode $ responseStatus resp
          body = responseBody resp
      if status == 200
        then case eitherDecode body of
          Right regResp -> return $ Right regResp
          Left err      -> return $ Left $ ParseError $ T.pack err
        else if status == 403
          then return $ Left $ AuthError "Invalid or expired invite code"
          else return $ Left $ ServerError status $ T.pack $ show body

-- | Ask an already-configured sync server to mint a new invite
-- code on our behalf. Authenticated with the CLI's own bearer
-- token, so this only works after the CLI has been registered via
-- 'registerDevice' (either using an admin-minted bootstrap invite
-- code, or using a fresh invite minted by another already-
-- registered device via this same endpoint).
createInvite
  :: T.Text                 -- ^ Server URL
  -> T.Text                 -- ^ Auth token
  -> Maybe Int              -- ^ Expiry override in hours
  -> IO (SyncResult CreateInviteResponse)
createInvite serverUrl authToken expiresInHours = do
  manager <- createManager
  let body = CreateInviteRequest { cirExpiresInHours = expiresInHours }
  result <- try $ do
    req <- buildRequest serverUrl "/invite" (Just authToken) (encode body)
    resp <- httpLbs req manager
    return resp
  case result of
    Left (e :: SomeException) ->
      return $ Left $ NetworkError $ T.pack $ show e
    Right resp -> do
      let status = statusCode $ responseStatus resp
          body'  = responseBody resp
      if status == 200
        then case eitherDecode body' of
          Right inviteResp -> return $ Right inviteResp
          Left err         -> return $ Left $ ParseError $ T.pack err
        else if status == 401
          then return $ Left $ AuthError "Invalid or missing auth token"
          else return $ Left $ ServerError status $ T.pack $ show body'

-- | Check if the server is healthy
checkServerHealth
  :: T.Text           -- ^ Server URL
  -> IO (SyncResult HealthResponse)
checkServerHealth serverUrl = do
  manager <- createManager
  result <- try $ do
    let url = T.unpack $ serverUrl <> "/health"
    req <- parseRequest url
    resp <- httpLbs req { method = "GET" } manager
    return resp
  case result of
    Left (e :: SomeException) ->
      return $ Left $ NetworkError $ T.pack $ show e
    Right resp -> do
      let status = statusCode $ responseStatus resp
          body = responseBody resp
      if status == 200
        then case eitherDecode body of
          Right healthResp -> return $ Right healthResp
          Left err         -> return $ Left $ ParseError $ T.pack err
        else return $ Left $ ServerError status $ T.pack $ show body
