module Todo.Options where

import           Control.Monad       (join)
import           Data.Maybe          (fromJust)
import           Data.Semigroup      ((<>))
import           Options.Applicative


import           Todo.Types

type Env = [(String, String)]

data TodoCommand = AddTodo
                 | ListTodo
                 | CompleteTodo
                 deriving (Eq, Show)

data TodoOpts = TodoOpts {
                   configFilePath :: Maybe FilePath
                 , debug          :: Bool
                 , cmd            :: TodoCommand
                 }

debugOpt = switch
        ( long "debug"
        <> help "Debug mode. Verbose output." )

configPathOpt = optional $ strOption
        ( long "config"
        <> short 'c'
        <> metavar "PATH"
        <> help "absolute path to the config file" )

todoCmds env = subparser (cmdList env)

cmdList env = command "ls" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List todos"
          options = pure ListTodo

todoOpts :: Env -> Parser TodoOpts
todoOpts env = TodoOpts <$> configPathOpt <*> debugOpt <*> (todoCmds env)

