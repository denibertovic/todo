{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Types where

import           Data.Aeson          (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           Data.Time.Calendar  (Day (..))
import           Data.Time.Clock     (UTCTime)


data TodoConfig = TodoConfig { todoDir    :: FilePath
                             , todoFile   :: FilePath
                             , reportFile :: FilePath
                             } deriving (Eq, Show)

instance FromJSON TodoConfig where
  parseJSON (Object o) = do
    todoDir <- o .: "todo_dir"
    todoFile <- o .: "todo_file"
    reportFile <- o .: "todo_report"
    return $ TodoConfig {..}
  parseJSON _ = fail "Expected Object for Config value"


newtype Context = Context String deriving (Eq)
newtype Project = Project String deriving (Eq)
newtype Link = Link String deriving (Eq)

instance Show Link where
  show (Link l) = l

instance Show Context where
  show (Context c) = "@" <> c

instance Show Project where
  show (Project p) = "+" <> p

data Metadata = MetadataProject Project
              | MetadataContext Context
              | MetadataTag Tag
              | MetadataString String
              | MetadataLink Link deriving (Eq)

instance Show Metadata where
  show (MetadataProject p) = show p
  show (MetadataContext c) = show c
  show (MetadataTag t)     = show t
  show (MetadataString s)  = s
  show (MetadataLink l)    = show l

instance {-# OVERLAPPING #-} Show [Metadata] where
  show xs = concat $ map ((<>) space) $ map show xs

data Priority = A | B | C | D | E | F deriving (Eq, Read)

instance Show Priority where
  show A = "(" <> "A" <> ")"
  show B = "(" <> "B" <> ")"
  show C = "(" <> "C" <> ")"
  show D = "(" <> "D" <> ")"
  show E = "(" <> "E" <> ")"
  show F = "(" <> "F" <> ")"

newtype TodoList = TodoList [Todo] deriving (Eq, Show)

data TodoItem  = TodoItem { tDescription :: String
                          , tPriority    :: Maybe Priority
                          , tMetadata    :: [Metadata]
                          , tCreatedAt   :: Maybe Day
                          , tDoneAt      :: Maybe Day
                          } deriving (Eq)

instance Show TodoItem where
  show (TodoItem desc pri metadata createdAt doneAt) = (maybe "" show pri)
                                                     <> space
                                                     <> (maybe "" show createdAt)
                                                     <> space
                                                     <> (maybe "" show doneAt)
                                                     <> space
                                                     <> desc
                                                     <> (show metadata)

space = " " :: String

data Tag = Tag String String
         | TagDueDate Day
         | TagOrigin Link deriving (Eq)

data Todo = Completed TodoItem | Incomplete TodoItem deriving (Eq)


instance Show Todo where
  show (Completed i)  = "x" <> space <> (show i)
  show (Incomplete i) = show i

instance {-# OVERLAPPING #-} Show [Todo] where
  show xs = concat $ map ((<>) "\n") $ map show xs

instance Show Tag where
  show (Tag key value)  = key <> ":" <> value
  show (TagDueDate d)   = "due:" <> (show d)
  show (TagOrigin link) = "origin:" <> (show link)

