{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           RIO
import qualified RIO.Text as T

import           Data.Time.Clock         (NominalDiffTime)
import           Options.Applicative
import           Prelude                 (putStrLn)

import           Database
import           Server
import           Todo.QrRenderer         (buildDeeplink, renderQrInvite)
import           Types

-- | Command type
data Command
  = Serve !Int !FilePath
  | GenerateInviteCode !FilePath !Int !(Maybe T.Text)
  -- ^ db path, expiry hours, optional server URL. When the server
  -- URL is present the command also prints the full
  -- todo-sync://register deeplink and a scannable QR code.
  | ListInviteCodes !FilePath
  deriving (Eq, Show)

-- | Parse serve command options
serveParser :: Parser Command
serveParser = Serve
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 8080
     <> help "Port to listen on (default: 8080)"
      )
  <*> strOption
      ( long "database"
     <> short 'd'
     <> metavar "PATH"
     <> help "Path to SQLite database (required)"
      )

-- | Parse generate-invite-code command
generateInviteParser :: Parser Command
generateInviteParser = GenerateInviteCode
  <$> strOption
      ( long "database"
     <> short 'd'
     <> metavar "PATH"
     <> help "Path to SQLite database (required)"
      )
  <*> option auto
      ( long "expires-in"
     <> short 'e'
     <> metavar "HOURS"
     <> value 24
     <> help "Hours until invite code expires (default: 24)"
      )
  <*> optional (strOption
      ( long "server-url"
     <> short 's'
     <> metavar "URL"
     <> help "Public server URL. When set, the command also prints the \
             \todo-sync:// deeplink and a scannable QR code the Android \
             \app can read directly from the terminal."
      ))

-- | Parse list-invite-codes command
listInvitesParser :: Parser Command
listInvitesParser = ListInviteCodes
  <$> strOption
      ( long "database"
     <> short 'd'
     <> metavar "PATH"
     <> help "Path to SQLite database (required)"
      )

-- | Combined command parser
commandParser :: Parser Command
commandParser = subparser
  ( command "serve"
      (info serveParser (progDesc "Run the sync server"))
 <> command "generate-invite-code"
      (info generateInviteParser (progDesc "Generate a new invite code"))
 <> command "list-invite-codes"
      (info listInvitesParser (progDesc "List all invite codes"))
  )
  -- Default to serve if no subcommand given
  <|> serveParser

-- | Full options parser info
optsInfo :: ParserInfo Command
optsInfo = info (commandParser <**> helper)
  ( fullDesc
 <> progDesc "Todo sync server - manage sync server and invite codes"
 <> header "todo-sync-server - A sync server for the todo CLI"
  )

main :: IO ()
main = do
  cmd <- execParser optsInfo
  case cmd of
    Serve port dbPath -> do
      let config = ServerConfig
            { scPort   = port
            , scDbPath = dbPath
            }
      runServer config

    GenerateInviteCode dbPath expiryHours mServerUrl -> do
      -- Initialize database first
      initDatabase dbPath
      let expirySeconds = fromIntegral (expiryHours * 3600) :: NominalDiffTime
      code <- createInviteCode dbPath expirySeconds
      putStrLn $ "Invite code: " <> T.unpack code
      putStrLn $ "Expires in: " <> show expiryHours <> " hours"
      -- If the operator passed a public server URL, also print the
      -- todo-sync://register deeplink and a scannable QR code so
      -- the Android onboarding flow is a one-scan operation.
      case mServerUrl of
        Nothing -> return ()
        Just url -> do
          let deeplink = buildDeeplink url code
          putStrLn ""
          putStrLn "Deeplink URL:"
          putStrLn $ "  " <> T.unpack deeplink
          putStrLn ""
          case renderQrInvite deeplink of
            Right qr -> do
              putStrLn "Scan this QR code with the Android app:"
              putStrLn ""
              putStrLn $ T.unpack qr
            Left err ->
              putStrLn $ "Failed to render QR: " <> T.unpack err

    ListInviteCodes dbPath -> do
      codes <- listInviteCodes dbPath
      if null codes
        then putStrLn "No invite codes found."
        else do
          putStrLn "Invite codes:"
          putStrLn "---"
          forM_ codes $ \DbInviteCode{..} -> do
            let status = case dbInviteUsedAt of
                  Nothing -> "available"
                  Just _  -> "used by " <> maybe "unknown" T.unpack dbInviteUsedBy
            putStrLn $ T.unpack dbInviteCode <> " | expires: " <> show dbInviteExpiresAt <> " | " <> status
