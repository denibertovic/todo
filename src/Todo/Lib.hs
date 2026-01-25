{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo.Lib where

import           Prelude            (print, putStrLn, (!!))
import           RIO
import qualified RIO.Text as T

import           Control.Monad      (when)
import           Control.Monad      (forM)
import qualified Data.ByteString    as BS
import           Data.Either        (isRight)
import           Data.List          (nubBy, sort)
import           Data.Maybe         (catMaybes)
import qualified Data.List.Index    as LX
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Data.Time.Clock    (UTCTime (..), getCurrentTime)
import           Data.Yaml          (decodeFileEither)
import qualified Data.Yaml          as Y
import           Rainbow            hiding ((<>))
import           System.Directory   (copyFile, doesFileExist, getHomeDirectory)
import           System.Exit        (die)
import           System.FilePath    ((</>))
import           Text.Parsec        (ParseError, parse)
import           Text.Pretty.Simple (pPrint)

import qualified Forge.Github.Lib   as Github
import           Forge.Github.Types (GithubConfig (..), GithubIssueDetails (..))
import qualified Forge.Gitlab.Lib   as Gitlab
import           Forge.Gitlab.Types (GitlabConfig (..), GitlabTodoDetails (..))
import qualified Forge.Types        as ForgeTypes
import qualified Forge.Utils        as ForgeUtils

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
entrypoint (TodoOpts configPath debug verbose cmd) = do
  h <- getHomeDirectory
  let defaultConfig = h </> ".todo.yaml"
  c <- readConfig $ maybe defaultConfig id configPath
  let t = todoFile c
  let d = todoDir c </> "done.txt"
  lo <- logOptionsHandle stderr debug
  withLogFunc lo $ \l -> do
    let app = App { appConfig=c, appLogger=l }
    case cmd of
      ListTodos filter     -> runRIO app $ listTodos filter verbose
      AddTodo l            -> runRIO app $ addTodo l
      CompleteTodo lines   -> runRIO app $ completeTodo lines
      DeleteTodo lines     -> runRIO app $ deleteTodo lines
      AddPriority line pri -> runRIO app $ addPriority line pri
      DeletePriority line  -> runRIO app $ deletePriority line
      PullRemotes rs       -> runRIO app $ pullRemotes rs
      Archive              -> runRIO app $ archiveTodos

isProject :: String -> Bool
isProject (x:xs) = case x of
  '+' -> True
  _   -> False

isContext :: String -> Bool
isContext (x:xs) = case x of
  '@' -> True
  _   -> False

mkProject :: String -> Metadata
mkProject (x:xs) = case x of
  '+' -> MetadataProject $ Project $ T.pack xs
  _   -> MetadataProject $ Project $ T.pack (x:xs)

mkContext :: String -> Metadata
mkContext (x:xs) = case x of
  '@' -> MetadataContext $ Context $ T.pack xs
  _   -> MetadataContext $ Context $ T.pack (x:xs)

listTodos :: (HasLogFunc env, HasConfig env) => [T.Text] -> Bool -> RIO env ()
listTodos filters verbose = do
  env <- ask
  let p = todoFile $ env ^. configL
  let projects = [mkProject $ T.unpack x | x <- filters, isProject $ T.unpack x]
  let contexts = [mkContext $ T.unpack x | x <- filters, isContext $ T.unpack x]
  c <- liftIO $ TIO.readFile p
  ts <- liftIO $ parseOrDie p c
  let todos =  sort $ filter (hasProjAndCtx projects contexts) $ zip [1..] ts
  case verbose of
    False -> do
      liftIO $ mapM_ (printItem . hideVerboseItems) todos
      liftIO $ printSummary (length todos) (length ts)
    True  -> do
      liftIO $ mapM_ printItem todos
      liftIO $ printSummary (length todos) (length ts)

printSummary :: Int -> Int -> IO ()
printSummary shown all = do
      putStrLn $ T.unpack "--"
      putStrLn $ T.unpack "TODO: " <> (show shown) <> " of " <> (show all) <> " tasks shown"

printItem :: (Int, Todo TodoItem) -> IO ()
printItem (lineNum, item) = colorPrintChunks $ [chunk (T.pack $ show lineNum) & fore green, chunk " ", chunkize item, chunk "\n"]

hideVerboseItems :: (Int, Todo TodoItem) -> (Int, Todo TodoItem)
hideVerboseItems (i, Incomplete x) = (i, Incomplete $ x{tMetadata=map hideOrigin $ tMetadata x})
hideVerboseItems (i, Completed x) = (i, Completed $ x{tMetadata=map hideOrigin $ tMetadata x})

hideOrigin :: Metadata -> Metadata
hideOrigin (MetadataTag (TagOrigin (Link l))) = MetadataTag $ TagOrigin $ Link "hidden"
hideOrigin x = x

chunkize :: Todo TodoItem -> Chunk
chunkize (Completed i) = chunk (T.pack $ show i) & fore grey
chunkize (Incomplete i) = case (tPriority i) of
  Just A  -> chunk (T.pack $ show i) & fore red
  Just B  -> chunk (T.pack $ show i) & fore yellow
  Just C  -> chunk (T.pack $ show i) & fore green
  Just _  -> chunk (T.pack $ show i) & fore cyan
  Nothing -> chunk (T.pack $ show i)

colorPrintChunks = mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256

hasProjAndCtx :: [Metadata] -> [Metadata] -> (Int, Todo TodoItem) -> Bool
hasProjAndCtx [] [] (_, Incomplete x) = True
hasProjAndCtx [] cs (_, Incomplete x) = or (map (flip elem cs) $ tMetadata x)
hasProjAndCtx ps [] (_, Incomplete x) = or (map (flip elem ps) $ tMetadata x)
hasProjAndCtx (p:ps) (c:cs) (_, Incomplete x) = or (map (flip elem (p:ps)) $ tMetadata x) && or (map (flip elem (c:cs)) $ tMetadata x)
hasProjAndCtx [] [] (_, Completed x) = True
hasProjAndCtx [] cs (_, Completed x) = or (map (flip elem cs) $ tMetadata x)
hasProjAndCtx ps [] (_, Completed x) = or (map (flip elem ps) $ tMetadata x)
hasProjAndCtx (p:ps) (c:cs) (_, Completed x) = or (map (flip elem (p:ps)) $ tMetadata x) && or (map (flip elem (c:cs)) $ tMetadata x)

addTodo :: (HasLogFunc env, HasConfig env) => T.Text -> RIO env ()
addTodo item = do
  env <- ask
  let p = todoFile $ env ^. configL
  let r = validateLine item
  case r of
    Right r' -> do
      liftIO $ backupFile p
      liftIO $ TIO.appendFile p $ item <> "\n"
    Left err -> liftIO $ die $ show err

completeTodo :: (HasLogFunc env, HasConfig env) => [Int] -> RIO env ()
completeTodo nums = do
  env <- ask
  let t = todoFile $ env ^. configL
  let d = todoDoneFile $ env ^. configL
  now <- liftIO $ getCurrentTime
  let nowDay = utctDay now
  c <- liftIO $ TIO.readFile t
  let (filtered, rest) = filterTodoLines nums $ T.lines c
  -- Parse each task, set completion date, and convert to Completed
  completedTasks <- liftIO $ mapM (completeTask nowDay) filtered
  let filtered' = map (T.pack . show) completedTasks
  liftIO $ appendTodoLines d filtered'
  liftIO $ writeTodoLines t rest
  liftIO $ putStrLn $ "Completed items:"
  liftIO $ putStrLn $ T.unpack $ concatTodoLines filtered'
  where
    completeTask day line = do
      todo <- parseItemOrDie line
      case todo of
        Incomplete item -> return $ Completed $ item { tCompletedAt = Just day }
        Completed item  -> return $ Completed $ item { tCompletedAt = Just day }

deleteTodo :: (HasLogFunc env, HasConfig env) => [Int] -> RIO env ()
deleteTodo nums = do
  env <- ask
  let p = todoFile $ env ^. configL
  c <- liftIO $ TIO.readFile p
  let (filtered, rest) = filterTodoLines nums $ T.lines c
  liftIO $ writeTodoLines p rest
  liftIO $ putStrLn $ "Removed items:"
  liftIO $ putStrLn $ T.unpack $ concatTodoLines filtered

archiveTodos :: (HasLogFunc env, HasConfig env) => RIO env ()
archiveTodos= do
  env <- ask
  let t = todoFile $ env ^. configL
  let d = todoDoneFile $ env ^. configL
  c <- liftIO $ TIO.readFile t
  let (filtered, rest) = filterDoneTodos $ T.lines c
  liftIO $ appendTodoLines d filtered
  liftIO $ writeTodoLines t rest
  liftIO $ putStrLn $ "Archived items:"
  liftIO $ putStrLn $ T.unpack $ concatTodoLines filtered

addPriority :: (HasLogFunc env, HasConfig env) => Int -> Priority -> RIO env ()
addPriority lineNum pri = do
  env <- ask
  let p = todoFile $ env ^. configL
  c <- liftIO $ TIO.readFile p
  let all = T.lines c
  let item = getTodoLine lineNum all
  todo <- liftIO $ parseItemOrDie item
  let t' = fmap (\i -> i{tPriority=Just pri}) todo :: Todo TodoItem
  liftIO $ writeTodoLines p $ modifyTodoLine lineNum all (show t')
  liftIO $ putStrLn $ "Prioritized item:"
  liftIO $ putStrLn $ show t'

deletePriority :: (HasLogFunc env, HasConfig env) => Int -> RIO env ()
deletePriority lineNum = do
  env <- ask
  let p = todoFile $ env ^. configL
  c <- liftIO $ TIO.readFile p
  let all = T.lines c
  let item = getTodoLine lineNum all
  todo <- liftIO $ parseItemOrDie item
  let t' = fmap (\i -> i{tPriority=Nothing}) todo :: Todo TodoItem
  liftIO $ writeTodoLines p $ modifyTodoLine lineNum all (show t')
  liftIO $ putStrLn $ "Deprioritized item:"
  liftIO $ putStrLn $ show t'

pullRemotes :: (HasLogFunc env, HasConfig env)  => [T.Text] -> RIO env ()
pullRemotes which = do
    env <- ask
    let c = env ^. configL
    let f = todoFile c
    let remotes = filterRemotes which c
    liftIO $ putStrLn $ "Fetching from remotes: " <> (show $ T.intercalate ", " $ map remoteName remotes)
    remoteTodos <- liftIO $ fetchRemoteTodos [remoteConfig x | x <- remotes]
    liftIO $ putStrLn $ "Fetched: " <> (show $ length remoteTodos)
    c <- liftIO $ TIO.readFile f
    todos <- liftIO $ parseOrDie f c
    let rs = map mkTodoFromRemoteTodo remoteTodos
    let newTodos = nubBy compareByOrigin $ todos ++ rs
    liftIO $ putStrLn $ "New todos added: " <> (show $ length newTodos - length todos)
    liftIO $ backupFile f
    liftIO $ TIO.writeFile f (T.pack $ show newTodos)
  where filterRemotes [] c = todoRemotes c
        filterRemotes (x:xs) c = filter (\r -> (remoteName r) `elem` which) $ todoRemotes c

compareByOrigin :: Todo TodoItem -> Todo TodoItem -> Bool
compareByOrigin (Incomplete t1) (Incomplete t2) = (tDescription t1) == (tDescription t2) && (tMetadata t1) == (tMetadata t2)
compareByOrigin (Completed _) (Completed _) = False

mkTodoFromRemoteTodo :: IsRemoteTodo a => a -> Todo TodoItem
mkTodoFromRemoteTodo t =
  Incomplete $
  TodoItem
    { tPriority = Nothing
    , tDescription = remoteTitle t
    , tMetadata =
        (map MetadataContext $ remoteContext t) <>
        [ MetadataProject $ Project $ T.toLower $ remoteProject t
        , MetadataContext $ Context $ T.toLower $ remoteGroup t
        , MetadataTag $ TagOrigin $ url2link $ remoteUrl t
        ]
    , tCreatedAt = Nothing
    , tCompletedAt = Nothing
    }

url2link :: ForgeTypes.Url -> Link
url2link (ForgeTypes.Url s) = Link s

fetchRemoteTodos :: [RemoteConfig] -> IO [RemoteTodo]
fetchRemoteTodos cs = do
  xss <-
    forM cs $ \c -> do
      case c of
        RemoteConfigGitlab c' ignores ctx -> do
          res <- Gitlab.listTodos' c'
          case res of
            Left err -> die $ show err
            Right xs ->
              return $
              map (addContexts ctx) $
              filterOutIgnores ignores $ map (\x -> RemoteTodoGitlab x []) xs
        RemoteConfigGithub c' ignores ctx -> do
          res <- Github.listIssues' c'
          case res of
            Left err -> die $ show err
            Right xs ->
              return $
              map (addContexts ctx) $
              filterOutIgnores ignores $ map (\x -> RemoteTodoGithub x []) xs
  return $ concat xss
  where
    filterOutIgnores :: [Ignore] -> [RemoteTodo] -> [RemoteTodo]
    filterOutIgnores igns xs =
      filter
        (\x ->
           not $
           inIgnoreGroups (remoteGroup x) (getIgnoreGroups igns) ||
           inIgnoreProjects
             (remoteGroup x, remoteProject x)
             (getIgnoreProjects igns))
        xs
    getIgnoreGroups igns =
      [T.toLower g | i@(IgnoreEntireGroup (RemoteGroup g)) <- igns]
    getIgnoreProjects igns =
      [ (T.toLower g, T.toLower p)
      | i@(IgnoreSpecificRepo (RemoteGroup g) (RemoteRepo p)) <- igns
      ]
    inIgnoreGroups x igns = T.toLower x `elem` igns
    inIgnoreProjects (x, y) igns = (T.toLower x, T.toLower y) `elem` igns
    addContexts :: [AddContext] -> RemoteTodo -> RemoteTodo
    addContexts ctxs (RemoteTodoGitlab t cs) =
      RemoteTodoGitlab t $
      cs <> catMaybes (map (addCtx $ RemoteTodoGitlab t cs) ctxs)
    addContexts ctxs (RemoteTodoGithub t cs) =
      RemoteTodoGithub t $
      cs <> catMaybes (map (addCtx $ RemoteTodoGithub t cs) ctxs)
    addCtx :: RemoteTodo -> AddContext -> Maybe Context
    addCtx t (AddToEntireGroup (RemoteGroup g) c) =
      if ((T.toLower $ remoteGroup t) == (T.toLower g))
        then (Just $ Context c)
        else Nothing
    addCtx t (AddToSpecificRepo (RemoteGroup g) (RemoteRepo p) c) =
      if ((T.toLower $ remoteGroup t) == (T.toLower g)) &&
         ((T.toLower $ remoteProject t) == (T.toLower p))
        then (Just $ Context c)
        else Nothing

getTodoLine :: Int -> [T.Text] -> T.Text
getTodoLine 0 xs   = xs !! 0
getTodoLine num xs = xs !! (num - 1)

modifyTodoLine :: Int -> [T.Text] -> String -> [T.Text]
modifyTodoLine i xs l = LX.modifyAt (i - 1) (\_ -> T.pack l) xs

concatTodoLines :: [T.Text] -> T.Text
concatTodoLines xs = (T.intercalate "\n" xs) <> "\n"

writeTodoLines :: FilePath -> [T.Text] -> IO ()
writeTodoLines t xs = do
  backupFile t
  TIO.writeFile t (concatTodoLines xs)

appendTodoLines :: FilePath -> [T.Text] -> IO ()
appendTodoLines t xs = do
  backupFile t
  TIO.appendFile t (concatTodoLines xs)

filterTodoLines :: [Int] -> [T.Text] -> ([T.Text], [T.Text])
filterTodoLines is xs = (filtered, rest)
  where filtered = [x | (_, x) <- filter (\(l, _) -> l `elem` is) $ zip [1..] xs]
        rest = [x | (_, x) <- filter (\(l, _) -> not $ l `elem` is) $ zip [1..] xs]

filterDoneTodos :: [T.Text] -> ([T.Text], [T.Text])
filterDoneTodos xs = (filtered, rest)
  where filtered = [x | x <- filter (\x -> T.isPrefixOf "x " x) xs]
        rest = [x | x <- filter (\x -> not $ T.isPrefixOf "x " x) xs]

parseItemOrDie :: T.Text -> IO (Todo TodoItem)
parseItemOrDie i = do
  let todo = parse todoItem "" i
  case todo of
    Left err   -> die $ show err
    Right item -> return item

parseOrDie :: FilePath -> T.Text -> IO ([Todo TodoItem])
parseOrDie p xs = do
  let todos = parse todoParser p xs
  case todos of
    Left err    -> die $ show err
    Right items -> return items

backupFile :: FilePath -> IO ()
backupFile p = do
  exists <- doesFileExist p
  when exists (copyFile p (p <> ".bak"))
