module Todo.Options where

import           RIO

import           Control.Monad       (join)
import           Data.Maybe          (fromJust)
import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_todo          (version)

import           Todo.Types

type Env = [(String, String)]

data TodoCommand = AddTodo T.Text
                 | ListTodos [T.Text]
                 | CompleteTodo [Int]
                 | DeleteTodo [Int]
                 | AddPriority Int Priority
                 | DeletePriority Int
                 | PullRemotes [T.Text]
                 | Archive
                 -- Sync commands
                 | SyncInit T.Text T.Text  -- "todo sync init <server-url> --invite-code <code>"
                 | SyncStatus              -- "todo sync status"
                 | SyncNow               -- "todo sync"
                 | SyncEnable            -- "todo sync enable"
                 | SyncDisable           -- "todo sync disable"
                 | SyncDaemon            -- "todo sync daemon"
                 | SyncInvite (Maybe Int) -- "todo sync invite [--expires-in HOURS]"
                 deriving (Eq, Show)

data TodoOpts = TodoOpts {
                   configFilePath :: Maybe FilePath
                 , debug          :: Bool
                 , verbose        :: Bool
                 , cmd            :: TodoCommand
                 }

debugOpt = switch
        ( long "debug"
        <> help "Debug mode. Verbose output." )

versionOpt = infoOption (showVersion version) (
               long "version"
               <> help "Show version.")

verboseOpt = switch (
               long "verbose"
               <> short 'v'
               <> help "Verbose output")

configPathOpt = optional $ strOption
        ( long "config"
        <> short 'c'
        <> metavar "PATH"
        <> help "absolute path to the config file" )

todoCmds env = subparser (  cmdList env
                         <> cmdAdd env
                         <> cmdComplete env
                         <> cmdDelete env
                         <> cmdAddPriority env
                         <> cmdDeletePriority env
                         <> cmdPullRemotes env
                         <> cmdArchive env
                         <> cmdSync env
                         )

cmdList env = command "ls" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List todos"
          options = ListTodos <$> (many $ argument str (metavar "+project/@context"))

cmdPullRemotes env = command "pull" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Pull (and merge) issues from configured remote"
          options = PullRemotes <$> (many $ argument str (metavar "REMOTE"))

cmdAdd env = command "add" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Add todo"
          options = AddTodo <$> argument str (metavar "ITEM")

cmdComplete env = command "do" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Completes one or more todos"
          options = CompleteTodo <$> (some $ argument auto (metavar "LINENUM"))

cmdDelete env = command "rm" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Deletes one or more todos"
          options = DeleteTodo <$> (some $ argument auto (metavar "LINENUM"))

cmdAddPriority env = command "pri" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Adds/Changes priority for the specified todo"
          options = AddPriority <$> (argument auto (metavar "LINENUM")) <*> (argument auto (metavar "PRI"))

cmdDeletePriority env = command "depri" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Deletes priority for the specified todo"
          options = DeletePriority <$> (argument auto (metavar "LINENUM"))

cmdArchive env = command "archive" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Moves all done tasks from todo.txt to done.txt"
          options = pure Archive

cmdSync env = command "sync" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Sync todos with remote server"
          options = syncSubcommands <|> pure SyncNow

syncSubcommands :: Parser TodoCommand
syncSubcommands = subparser
  (  command "init" (info syncInitOpts (progDesc "Initialize sync with a server"))
  <> command "status" (info (pure SyncStatus) (progDesc "Show sync status"))
  <> command "enable" (info (pure SyncEnable) (progDesc "Enable automatic sync"))
  <> command "disable" (info (pure SyncDisable) (progDesc "Disable automatic sync"))
  <> command "daemon" (info (pure SyncDaemon) (progDesc "Run sync daemon (file watcher)"))
  <> command "invite" (info syncInviteOpts (progDesc "Mint an invite code + QR to onboard another device"))
  )
  where
    syncInitOpts = SyncInit
      <$> argument str (metavar "SERVER_URL")
      <*> strOption
          ( long "invite-code"
         <> short 'i'
         <> metavar "CODE"
         <> help "Invite code for registration"
          )
    syncInviteOpts = SyncInvite
      <$> optional (option auto
          ( long "expires-in"
         <> short 'e'
         <> metavar "HOURS"
         <> help "Hours until the new invite code expires (default: 24)"
          ))

todoOpts :: Env -> Parser TodoOpts
todoOpts env = TodoOpts <$> configPathOpt <*> debugOpt <*> verboseOpt <*> (todoCmds env)
