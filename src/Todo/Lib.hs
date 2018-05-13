{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo.Lib where

import           Control.Monad      (when)
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Data.Yaml          (decodeFileEither)
import qualified Data.Yaml          as Y
import           System.Directory   (doesFileExist, getHomeDirectory)
import           System.Exit        (die)
import           System.FilePath    ((</>))
import           Text.Parsec        (ParseError, parse)
import           Text.Pretty.Simple (pPrint)

import           Todo.Options
import           Todo.Parser
import           Todo.Types

decodeConfig :: FilePath -> IO (Either Y.ParseException TodoConfig)
decodeConfig p = Y.decodeFileEither p

readConfig :: FilePath -> IO (TodoConfig)
readConfig p = do
  exists <- doesFileExist p
  when (not exists) (die "Config file does not exist.")
  c <- decodeConfig p
  case c of
    Left err -> die (show err)
    Right c  -> return c

entrypoint :: TodoOpts -> IO ()
entrypoint (TodoOpts configPath debug cmd) = do
  h <- getHomeDirectory
  let defaultConfig = h </> ".todo.yaml"
  c <- readConfig $ maybe defaultConfig id configPath
  let t = todoFile c
  case cmd of
    ListTodo -> list t

list :: FilePath -> IO ()
list p = do
  c <- TIO.readFile p
  let r  = parse todoParser p (T.unpack c) :: Either ParseError [Todo]
  case r of
    Right r' -> putStrLn $ show r'
    Left err -> die $ show err
