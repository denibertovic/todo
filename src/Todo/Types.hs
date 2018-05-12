{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Types where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           Data.Time.Calendar  (Day (..))
import           Data.Time.Clock     (UTCTime)


newtype Context = Context T.Text deriving (Eq, Show)
newtype Project = Project T.Text deriving (Eq, Show)
newtype Link = Link String deriving (Eq, Show)

data Metadata = MetadataProject Project
              | MetadataContext Context
              | MetadataTag Tag
              | MetadataString String
              | MetadataLink Link deriving (Eq, Show)

data Priority = A | B | C | D | E | F deriving (Eq, Show, Read)

newtype TodoList = TodoList [Todo] deriving (Eq, Show)

data TodoItem  = TodoItem { tDescription :: T.Text
                          , tPriority    :: Maybe Priority
                          , tMetadata    :: [Metadata]
                          , tCreatedAt   :: Maybe UTCTime
                          , tDueAt       :: Maybe UTCTime
                          } deriving (Eq, Show)

data Tag = Tag String String
         | TagDueDate Day
         | TagOrigin Link deriving (Eq, Show)

data Todo = Completed TodoItem | Incomplete TodoItem deriving (Eq, Show)

