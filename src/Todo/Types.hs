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
import           Text.Parsec         (ParseError)


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

instance Ord Priority where
  compare a b = compare (show a) (show b)

newtype TodoList = TodoList [Todo] deriving (Eq, Show)

data TodoItem  = TodoItem { tPriority    :: Maybe Priority
                          , tDescription :: String
                          , tMetadata    :: [Metadata]
                          , tCreatedAt   :: Maybe Day
                          , tDoneAt      :: Maybe Day
                          } deriving (Eq)

instance Show TodoItem where
  show (TodoItem pri desc metadata createdAt doneAt) = (maybe "" (flip (<>) space . show) pri)
                                                     <> (maybe "" (flip (<>) space . show) createdAt)
                                                     <> (maybe "" (flip (<>) space . show) doneAt)
                                                     <> desc
                                                     <> (show metadata)

space = " " :: String

data Tag = Tag String String
         | TagDueDate Day
         | TagOrigin Link deriving (Eq)

data Todo = Completed TodoItem | Incomplete TodoItem deriving (Eq)

instance Ord TodoItem where
  compare (TodoItem Nothing _ _ _ _) (TodoItem Nothing _ _ _ _) = EQ
  compare (TodoItem (Just _) _ _ _ _) (TodoItem Nothing _ _ _ _) = LT
  compare (TodoItem Nothing  _ _ _ _) (TodoItem (Just _) _ _ _ _) = GT
  compare (TodoItem (Just a) _ _ _ _) (TodoItem (Just b) _ _ _ _) =
    compare a b

instance Ord Todo where
  compare (Incomplete _) (Completed _) = LT
  compare (Completed _ ) (Incomplete _) = GT
  compare (Incomplete t1) (Incomplete t2) =
    compare t1 t2
  compare (Completed t1) (Completed t2) = compare t1 t2

instance {-# OVERLAPPING #-} Ord (Int, Either ParseError Todo) where
  compare (_, Right _) (_, Left _)    = LT
  compare (_, Left _) (_, Right _)    = GT
  compare (_, Left _) (_, Left _)     = EQ
  compare (_, Right t1) (_, Right t2) = compare t1 t2

instance Ord ParseError where
  compare _ _ = EQ

instance Show Todo where
  show (Completed i)  = "x" <> space <> (show i)
  show (Incomplete i) = show i

instance {-# OVERLAPPING #-} Show [Todo] where
  show xs = concat $ map show xs

instance Show Tag where
  show (Tag key value)  = key <> ":" <> value
  show (TagDueDate d)   = "due:" <> (show d)
  show (TagOrigin link) = "origin:" <> (show link)

