{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Types where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)


newtype Context = Context T.Text deriving (Eq, Show)

newtype Project = Project T.Text deriving (Eq, Show)

data Metadata = MetadataProject Project | MetadataContext Context | MetadataTag Tag | MetadataString String deriving (Eq, Show)

data Priority = A | B | C | D | E | F deriving (Eq, Show, Read)

newtype TodoList = TodoList [Todo] deriving (Eq, Show)

data TodoItem  = TodoItem { tDescription :: T.Text
                          , tPriority    :: Maybe Priority
                          , tMetadata    :: [Metadata]
                          , tCreatedAt   :: Maybe UTCTime
                          , tDueAt       :: Maybe UTCTime
                          } deriving (Eq, Show)

type Tag = (String, String)

data Todo = Completed TodoItem | Incomplete TodoItem deriving (Eq, Show)

