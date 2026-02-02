{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Types where

import           RIO
import           Data.Char           (chr, ord)
import           Prelude             (toEnum, fromEnum)

import           Data.Aeson          (FromJSON, ToJSON, Value (..), parseJSON,
                                      toJSON, (.:), (.:?), (.=), object, withObject,
                                      withText)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Key      as Key
import qualified Data.Aeson.KeyMap   as KM
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Data.Time.Calendar  (Day (..))
import           Data.Time.Clock     (UTCTime)
import           Control.Monad       (mapM)

import           Forge.Types (Url)
import           Forge.Github.Types  (GithubConfig (..),
                                      GithubIssueDetails (..))
import           Forge.Gitlab.Types  (GitlabConfig (..), GitlabTodoDetails (..))

data App = App
  { appConfig     :: !TodoConfig
  , appLogger     :: !LogFunc
  , appConfigPath :: !FilePath
  }

class HasConfig env where
  configL :: Lens' env TodoConfig

class HasConfigPath env where
  configPathL :: Lens' env FilePath

instance HasConfig App where
  configL = lens appConfig (\x y -> x { appConfig = y })

instance HasConfigPath App where
  configPathL = lens appConfigPath (\x y -> x { appConfigPath = y })

instance HasLogFunc App where
  logFuncL = lens appLogger (\x y -> x { appLogger = y })

-- | Type of remotes that we support. Add new types here
data RemoteType = Gitlab | Github deriving (Eq, Show)

newtype RemoteGroup = RemoteGroup T.Text deriving (Eq, Show)
newtype RemoteRepo = RemoteRepo T.Text deriving (Eq, Show)

data AddContext = AddToEntireGroup RemoteGroup T.Text
                | AddToSpecificRepo RemoteGroup RemoteRepo T.Text
                deriving (Eq, Show)

data Ignore = IgnoreEntireGroup RemoteGroup
            | IgnoreSpecificRepo RemoteGroup RemoteRepo
            deriving (Eq, Show)

instance FromJSON AddContext where
  parseJSON (JSON.Object o) = do
    let ((k,v):_) = KM.toList o
    let k' = Key.toText k
    v' <- parseJSON v
    case (T.splitOn "/" k') of
      [g, p] -> case p of
                  "*" -> return $ AddToEntireGroup (RemoteGroup g) v'
                  "" -> fail addContextErrMsg
                  otherwise -> return $ AddToSpecificRepo (RemoteGroup g) (RemoteRepo p) v'
      otherwise -> fail addContextErrMsg
  parseJSON _ = fail addContextErrMsg

addContextErrMsg :: String
addContextErrMsg = "Please specify AddContext using the following format: 'org/repo: foo' or 'org/*: bar'."

instance FromJSON Ignore where
  parseJSON (JSON.String s) = case (T.splitOn "/" s) of
      [g, p] -> case p of
                  "*" -> return $ IgnoreEntireGroup (RemoteGroup g)
                  "" -> fail ignoreErrMsg
                  otherwise -> return $ IgnoreSpecificRepo (RemoteGroup g) (RemoteRepo p)
      otherwise -> fail ignoreErrMsg
  parseJSON _ = fail ignoreErrMsg

ignoreErrMsg :: String
ignoreErrMsg = "Please specify the remote ignore list using the following format: 'org/repo' or 'org/*'."

data RemoteConfig = RemoteConfigGitlab GitlabConfig [Ignore] [AddContext]
                  | RemoteConfigGithub GithubConfig [Ignore] [AddContext]
                  deriving (Eq, Show)

instance FromJSON RemoteConfig where
  parseJSON (JSON.Object o) = do
    t <- o .: "type"
    igns <- o .:? "ignore"
    cs <- o .:? "add_context"
    addCtxs <- case cs of
      Nothing -> return [] :: JSON.Parser [AddContext]
      Just xs -> JSON.withArray "Array of AddContext" (\arr -> mapM parseJSON (V.toList arr)) xs
    ignores <- case igns of
      Nothing -> return [] :: JSON.Parser [Ignore]
      Just xs -> JSON.withArray "Array of Ignores" (\arr -> mapM parseJSON (V.toList arr)) xs
    c <- case t of
      Gitlab -> do
        c' <- parseJSON (JSON.Object o)
        return $ RemoteConfigGitlab c' ignores addCtxs
      Github -> do
        c'' <- parseJSON (JSON.Object o)
        return $ RemoteConfigGithub c'' ignores addCtxs
    return c
  parseJSON _ = fail "Expected Object for RemoteConfig value"

instance FromJSON RemoteType where
  parseJSON (JSON.String "github") = return Github
  parseJSON (JSON.String "gitlab") = return Gitlab
  parseJSON _ = fail "Failed to parse Remote type. Supported remotes are: github, gitlab."

data Remote = Remote { remoteName :: T.Text, remoteConfig :: RemoteConfig } deriving (Eq, Show)

data RemoteTodo = RemoteTodoGithub GithubIssueDetails [Context]
                | RemoteTodoGitlab GitlabTodoDetails [Context]
                deriving (Eq, Show)

-- appendContext :: [Context] -> RemoteTodo -> RemoteTodo
-- appendContext xs (RemoteTodoGitlab t cs) = RemoteTodoGitlab t (cs <> xs)
-- appendContext xs (RemoteTodoGithub t cs) = RemoteTodoGithub t (cs <> xs)

class IsRemoteTodo a where
  remoteProject :: a -> T.Text
  remoteGroup :: a -> T.Text
  remoteUrl :: a -> Url
  remoteTitle :: a -> T.Text
  remoteContext :: a -> [Context]

instance IsRemoteTodo RemoteTodo where
  remoteProject (RemoteTodoGithub t _) = githubIssueProject t
  remoteProject (RemoteTodoGitlab t _) = gitlabTodoProject t

  remoteGroup (RemoteTodoGithub t _) = githubIssueGroup t
  remoteGroup (RemoteTodoGitlab t _) = gitlabTodoGroup t

  remoteUrl (RemoteTodoGithub t _) = githubIssueUrl t
  remoteUrl (RemoteTodoGitlab t _) = gitlabTodoUrl t

  remoteTitle (RemoteTodoGithub t _) = githubIssueTitle t
  remoteTitle (RemoteTodoGitlab t _) = gitlabTodoTitle t

  remoteContext (RemoteTodoGithub t c) = c
  remoteContext (RemoteTodoGitlab t c) = c

instance FromJSON Remote where
    parseJSON (JSON.Object o) = do
      name <- o .: "name"
      c <- parseJSON (JSON.Object o)
      return $ Remote {remoteName=name, remoteConfig=c}
    parseJSON _ = fail "Expected Object for Remote value"

-- | Sync configuration
data SyncConfig = SyncConfig
  { syncEnabled        :: !Bool
  , syncServerUrl      :: !T.Text
  , syncDeviceName     :: !T.Text
  , syncIntervalSecs   :: !Int
  , syncAuthToken      :: !(Maybe T.Text)
  } deriving (Eq, Show, Generic)

instance ToJSON SyncConfig where
  toJSON SyncConfig{..} = object
    [ "enabled"               .= syncEnabled
    , "server_url"            .= syncServerUrl
    , "device_name"           .= syncDeviceName
    , "sync_interval_seconds" .= syncIntervalSecs
    , "auth_token"            .= syncAuthToken
    ]

instance FromJSON SyncConfig where
  parseJSON = withObject "SyncConfig" $ \o -> do
    syncEnabled      <- o .: "enabled"
    syncServerUrl    <- o .: "server_url"
    syncDeviceName   <- o .: "device_name"
    syncIntervalSecs <- o .: "sync_interval_seconds"
    syncAuthToken    <- o .:? "auth_token"
    return SyncConfig{..}

-- | Default sync config (disabled)
defaultSyncConfig :: SyncConfig
defaultSyncConfig = SyncConfig
  { syncEnabled      = False
  , syncServerUrl    = ""
  , syncDeviceName   = "default"
  , syncIntervalSecs = 300
  , syncAuthToken    = Nothing
  }

data TodoConfig = TodoConfig { todoDir        :: FilePath
                             , todoFile       :: FilePath
                             , todoDoneFile   :: FilePath
                             , todoReportFile :: FilePath
                             , todoRemotes    :: [Remote]
                             , todoSync       :: SyncConfig
                             } deriving (Eq, Show)

instance FromJSON TodoConfig where
  parseJSON (Object o) = do
    todoDir <- o .: "todo_dir"
    todoFile <- o .: "todo_file"
    todoDoneFile <- o .: "done_file"
    todoReportFile <- o .: "report_file"
    todoRemotes <- (o .: "remotes") >>= parseJSON
    todoSync <- o .:? "sync" >>= \case
      Just s  -> return s
      Nothing -> return defaultSyncConfig
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

data Priority = A | B | C | D | E | F | G | H | I | J | K | L | M
              | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
              deriving (Eq, Read, Enum)

instance Show Priority where
  show p = "(" <> [chr (ord 'A' + fromEnum p)] <> ")"

instance Ord Priority where
  compare a b = compare (show a) (show b)

data TodoItem  = TodoItem { tPriority    :: Maybe Priority
                          , tDescription :: T.Text
                          , tMetadata    :: [Metadata]
                          , tCreatedAt   :: Maybe Day
                          , tCompletedAt :: Maybe Day
                          } deriving (Eq)

instance Show TodoItem where
  show (TodoItem pri desc metadata createdAt _completedAt) =
    (maybe "" (flip (<>) space . show) pri)
    <> (maybe "" (flip (<>) space . show) createdAt)
    <> T.unpack desc
    <> (show metadata)

space = " " :: String

data Tag = Tag T.Text T.Text
         | TagDueDate Day
         | TagNext
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
  show (Completed i)  = "x" <> space
                        <> (maybe "" (flip (<>) space . show) (tPriority i))
                        <> (maybe "" (flip (<>) space . show) (tCompletedAt i))
                        <> (maybe "" (flip (<>) space . show) (tCreatedAt i))
                        <> T.unpack (tDescription i)
                        <> (show (tMetadata i))
  show (Incomplete i) = show i

instance {-# OVERLAPPING #-} Show [Todo TodoItem] where
  show xs = T.unpack $ (T.intercalate "\n" $ (map (T.pack . show)  xs)) <> "\n"

instance Show Tag where
  show (Tag key value)  = T.unpack $ key <> ":" <> value
  show (TagDueDate d)   = "due:" <> (show d)
  show TagNext          = "due:next"
  show (TagOrigin link) = "origin:" <> (show link)

-- JSON instances for sync serialization

instance ToJSON Priority where
  toJSON p = JSON.String $ T.pack [chr (ord 'A' + fromEnum p)]

instance FromJSON Priority where
  parseJSON = withText "Priority" $ \t ->
    case T.unpack t of
      [c] | c >= 'A' && c <= 'Z' -> return $ toEnum (ord c - ord 'A')
      _ -> fail "Priority must be a single letter A-Z"

instance ToJSON Link where
  toJSON (Link l) = JSON.String l

instance FromJSON Link where
  parseJSON = withText "Link" $ \t -> return $ Link t

instance ToJSON Context where
  toJSON (Context c) = JSON.String c

instance FromJSON Context where
  parseJSON = withText "Context" $ \t -> return $ Context t

instance ToJSON Project where
  toJSON (Project p) = JSON.String p

instance FromJSON Project where
  parseJSON = withText "Project" $ \t -> return $ Project t

instance ToJSON Tag where
  toJSON (Tag key value) = object ["type" .= ("tag" :: T.Text), "key" .= key, "value" .= value]
  toJSON (TagDueDate d)  = object ["type" .= ("due_date" :: T.Text), "date" .= d]
  toJSON TagNext         = object ["type" .= ("due_next" :: T.Text)]
  toJSON (TagOrigin l)   = object ["type" .= ("origin" :: T.Text), "link" .= l]

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \o -> do
    tagType <- o .: "type" :: JSON.Parser T.Text
    case tagType of
      "tag"      -> Tag <$> o .: "key" <*> o .: "value"
      "due_date" -> TagDueDate <$> o .: "date"
      "due_next" -> return TagNext
      "origin"   -> TagOrigin <$> o .: "link"
      _          -> fail $ "Unknown tag type: " <> T.unpack tagType

instance ToJSON Metadata where
  toJSON (MetadataProject p) = object ["type" .= ("project" :: T.Text), "project" .= p]
  toJSON (MetadataContext c) = object ["type" .= ("context" :: T.Text), "context" .= c]
  toJSON (MetadataTag t)     = object ["type" .= ("tag" :: T.Text), "tag" .= t]
  toJSON (MetadataString s)  = object ["type" .= ("string" :: T.Text), "value" .= s]
  toJSON (MetadataLink l)    = object ["type" .= ("link" :: T.Text), "link" .= l]

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \o -> do
    metaType <- o .: "type" :: JSON.Parser T.Text
    case metaType of
      "project" -> MetadataProject <$> o .: "project"
      "context" -> MetadataContext <$> o .: "context"
      "tag"     -> MetadataTag <$> o .: "tag"
      "string"  -> MetadataString <$> o .: "value"
      "link"    -> MetadataLink <$> o .: "link"
      _         -> fail $ "Unknown metadata type: " <> T.unpack metaType

instance ToJSON TodoItem where
  toJSON TodoItem{..} = object
    [ "priority"     .= tPriority
    , "description"  .= tDescription
    , "metadata"     .= tMetadata
    , "created_at"   .= tCreatedAt
    , "completed_at" .= tCompletedAt
    ]

instance FromJSON TodoItem where
  parseJSON = withObject "TodoItem" $ \o -> do
    tPriority    <- o .:? "priority"
    tDescription <- o .: "description"
    tMetadata    <- o .: "metadata"
    tCreatedAt   <- o .:? "created_at"
    tCompletedAt <- o .:? "completed_at"
    return TodoItem{..}

instance ToJSON a => ToJSON (Todo a) where
  toJSON (Completed item)  = object ["status" .= ("completed" :: T.Text), "item" .= item]
  toJSON (Incomplete item) = object ["status" .= ("incomplete" :: T.Text), "item" .= item]

instance FromJSON a => FromJSON (Todo a) where
  parseJSON = withObject "Todo" $ \o -> do
    status <- o .: "status" :: JSON.Parser T.Text
    item <- o .: "item"
    case status of
      "completed"  -> return $ Completed item
      "incomplete" -> return $ Incomplete item
      _            -> fail $ "Unknown todo status: " <> T.unpack status

