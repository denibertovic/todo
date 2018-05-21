module Todo.Options where

import           Control.Monad       (join)
import           Data.Maybe          (fromJust)
import           Data.Semigroup      ((<>))
import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_todo          (version)


import           Todo.Types

type Env = [(String, String)]

data TodoCommand = AddTodo String
                 | ListTodos [String]
                 | CompleteTodo [Int]
                 | DeleteTodo [Int]
                 | AddPriority Int Priority
                 | DeletePriority Int
                 | PullOrigins
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
                         <> cmdPullOrigins env
                         )

cmdList env = command "ls" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List todos"
          options = ListTodos <$> (many $ argument str (metavar "+project/@context"))

cmdPullOrigins env = command "pull" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Pull (and merge) issues from configured origins"
          options = pure PullOrigins

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

todoOpts :: Env -> Parser TodoOpts
todoOpts env = TodoOpts <$> configPathOpt <*> debugOpt <*> verboseOpt <*> (todoCmds env)
