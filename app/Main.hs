{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid         ((<>))
import           Options.Applicative
import           System.Environment  (getEnvironment)

import           Todo.Lib
import           Todo.Options

main :: IO ()
main = do
  env <- getEnvironment
  execParser (opts env) >>= entrypoint
  where
    opts env = info (helper <*> versionOpt <*> todoOpts env)
      ( fullDesc
     <> progDesc "Todo.txt CLI tool"
     <> header "todo - A CLI tool for working with todo.txt" )

