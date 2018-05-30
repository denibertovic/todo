{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Types where

import           RIO

import           Data.Aeson          (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           Data.Time.Calendar  (Day (..))
import           Data.Time.Clock     (UTCTime)

import           Forge.Github.Types  (GithubConfig (..),
                                      GithubIssueDetails (..))
import           Forge.Gitlab.Types  (GitlabConfig (..), GitlabTodoDetails (..))

data App = App { appConfig :: !TodoConfig, appLogger :: !LogFunc  }

class HasConfig env where
  configL :: Lens' env TodoConfig

instance HasConfig App where
  configL = lens appConfig (\x y -> x { appConfig = y })

instance HasLogFunc App where
  logFuncL = lens appLogger (\x y -> x { appLogger = y })

-- | Type of remotes that we support. Add new types here
data RemoteType = Gitlab | Github deriving (Eq, Show)

data RemoteConfig = RemoteConfigGitlab GitlabConfig | RemoteConfigGithub GithubConfig deriving (Eq, Show)

instance FromJSON RemoteConfig where
  parseJSON (JSON.Object o) = do
    t <- o .: "type"
    c <- case t of
      Gitlab -> RemoteConfigGitlab <$> parseJSON (JSON.Object o)
      Github -> RemoteConfigGithub <$> parseJSON (JSON.Object o)
    return c
  parseJSON _ = fail "Expected Object for RemoteConfig value"

instance FromJSON RemoteType where
  parseJSON (JSON.String "github") = return Github
  parseJSON (JSON.String "gitlab") = return Gitlab
  parseJSON _ = fail "Failed to parse Remote type. Supported remotes are: github, gitlab."

data Remote = Remote { remoteName :: T.Text, remoteConfig :: RemoteConfig } deriving (Eq, Show)

data RemoteTodo = RemoteTodoGithub GithubIssueDetails | RemoteTodoGitlab GitlabTodoDetails deriving (Eq, Show)

instance FromJSON Remote where
    parseJSON (JSON.Object o) = do
      name <- o .: "name"
      c <- parseJSON (JSON.Object o)
      return $ Remote {remoteName=name, remoteConfig=c}
    parseJSON _ = fail "Expected Object for Remote value"

data TodoConfig = TodoConfig { todoDir        :: FilePath
                             , todoFile       :: FilePath
                             , todoDoneFile   :: FilePath
                             , todoReportFile :: FilePath
                             , todoRemotes    :: [Remote]
                             } deriving (Eq, Show)

instance FromJSON TodoConfig where
  parseJSON (Object o) = do
    todoDir <- o .: "todo_dir"
    todoFile <- o .: "todo_file"
    todoDoneFile <- o .: "done_file"
    todoReportFile <- o .: "report_file"
    todoRemotes <- (o .: "remotes") >>= parseJSON
    return $ TodoConfig {..}
  parseJSON _ = fail "Expected Object for Config value"

newtype Context = Context T.Text deriving (Eq)
newtype Project = Project T.Text deriving (Eq)
newtype Link = Link T.Text deriving (Eq)

instance Show Link where
  show (Link l) = T.unpack l

instance Show Context where
  show (Context c) = T.unpack $ "@" <> c

instance Show Project where
  show (Project p) = T.unpack $ "+" <> p

data Metadata = MetadataProject Project
              | MetadataContext Context
              | MetadataTag Tag
              | MetadataString T.Text
              | MetadataLink Link deriving (Eq)

instance Show Metadata where
  show (MetadataProject p) = show p
  show (MetadataContext c) = show c
  show (MetadataTag t)     = show t
  show (MetadataString s)  = T.unpack s
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

data TodoItem  = TodoItem { tPriority    :: Maybe Priority
                          , tDescription :: T.Text
                          , tMetadata    :: [Metadata]
                          , tCreatedAt   :: Maybe Day
                          , tDoneAt      :: Maybe Day
                          } deriving (Eq)

instance Show TodoItem where
  show (TodoItem pri desc metadata createdAt doneAt) = (maybe "" (flip (<>) space . show) pri)
                                                     <> (maybe "" (flip (<>) space . show) createdAt)
                                                     <> (maybe "" (flip (<>) space . show) doneAt)
                                                     <> T.unpack desc
                                                     <> (show metadata)

space = " " :: String

data Tag = Tag T.Text T.Text
         | TagDueDate Day
         | TagOrigin Link deriving (Eq)

data Todo a = Completed a | Incomplete a deriving (Eq)

instance Functor Todo where
  fmap f (Completed t)  =  Completed $ f t
  fmap f (Incomplete t) =  Incomplete $ f t

instance Ord TodoItem where
  compare (TodoItem Nothing _ _ _ _) (TodoItem Nothing _ _ _ _) = EQ
  compare (TodoItem (Just _) _ _ _ _) (TodoItem Nothing _ _ _ _) = LT
  compare (TodoItem Nothing  _ _ _ _) (TodoItem (Just _) _ _ _ _) = GT
  compare (TodoItem (Just a) _ _ _ _) (TodoItem (Just b) _ _ _ _) =
    compare a b

instance Ord (Todo TodoItem) where
  compare (Incomplete _) (Completed _) = LT
  compare (Completed _ ) (Incomplete _) = GT
  compare (Incomplete t1) (Incomplete t2) =
    compare t1 t2
  compare (Completed t1) (Completed t2) = compare t1 t2

instance {-# OVERLAPPING #-} Ord (Int, Todo TodoItem) where
  compare (_, t1) (_, t2) = compare t1 t2

instance Show (Todo TodoItem) where
  show (Completed i)  = "x" <> space <> (show i)
  show (Incomplete i) = show i

instance {-# OVERLAPPING #-} Show [Todo TodoItem] where
  show xs = T.unpack $ (T.intercalate "\n" $ (map (T.pack . show)  xs)) <> "\n"

instance Show Tag where
  show (Tag key value)  = show $ key <> ":" <> value
  show (TagDueDate d)   = "due:" <> (show d)
  show (TagOrigin link) = "origin:" <> (show link)

