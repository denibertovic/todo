{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo.Lib where

import           Control.Monad      (when)
import           Control.Monad      (forM)
import qualified Data.ByteString    as BS
import           Data.Either        (isRight)
import           Data.List          (nubBy, sort)
import qualified Data.List.Index    as LX
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Data.Time.Clock    (UTCTime (..), getCurrentTime)
import           Data.Yaml          (decodeFileEither)
import qualified Data.Yaml          as Y
import           Rainbow
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
  case cmd of
    ListTodos filter     -> listTodos t filter verbose
    AddTodo l            -> addTodo t l
    CompleteTodo lines   -> completeTodo t d lines
    DeleteTodo lines     -> deleteTodo t lines
    AddPriority line pri -> addPriority t line pri
    DeletePriority line  -> deletePriority t line
    PullOrigins          -> pullOrigins c

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
  '+' -> MetadataProject $ Project xs
  _   -> MetadataProject $ Project (x:xs)

mkContext :: String -> Metadata
mkContext (x:xs) = case x of
  '@' -> MetadataContext $ Context xs
  _   -> MetadataContext $ Context (x:xs)

listTodos :: FilePath -> [String] -> Bool -> IO ()
listTodos p filters verbose = do
  let projects = [mkProject x | x <- filters, isProject x]
  let contexts = [mkContext x | x <- filters, isContext x]
  c <- TIO.readFile p
  let todos =  sort $ filter (hasProjAndCtx projects contexts) $ zip [1..] $ map (parse todoItem p) $ map T.unpack $ T.lines c
  case verbose of
    False -> mapM_ (printItem . hideVerboseItems) todos
    True  -> mapM_ printItem todos

printItem :: (Int, Either ParseError Todo) -> IO ()
printItem (lineNum, item) = case item of
  Left err -> colorPrintChunks $ [chunk (show lineNum) & fore green, chunk " ", chunk $ show err, chunk "\n"]
  Right i  -> colorPrintChunks $ [chunk (show lineNum) & fore green, chunk " ", chunkize i, chunk "\n"]

hideVerboseItems :: (Int, Either ParseError Todo) -> (Int, Either ParseError Todo)
hideVerboseItems (i, Left x) = (i, Left x)
hideVerboseItems (i, Right (Incomplete x)) = (i, Right (Incomplete $ x{tMetadata=map hideOrigin $ tMetadata x}))
hideVerboseItems (i, Right (Completed x)) = (i, Right (Completed $ x{tMetadata=map hideOrigin $ tMetadata x}))

hideOrigin :: Metadata -> Metadata
hideOrigin (MetadataTag (TagOrigin (Link l))) = MetadataTag $ TagOrigin $ Link "hidden"
hideOrigin x = x

chunkize :: Todo -> Chunk String
chunkize (Completed i) = chunk (show i) & fore grey
chunkize (Incomplete i) = case (tPriority i) of
  Just A  -> chunk (show i) & fore red
  Just B  -> chunk (show i) & fore yellow
  Just C  -> chunk (show i) & fore green
  Just _  -> chunk (show i) & fore cyan
  Nothing -> chunk (show i)

colorPrintChunks = mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256

hasProjAndCtx :: [Metadata] -> [Metadata] -> (Int, Either ParseError Todo) -> Bool
hasProjAndCtx [] [] (_, (Right (Incomplete x))) = True
hasProjAndCtx [] cs (_, (Right (Incomplete x))) = or (map (flip elem cs) $ tMetadata x)
hasProjAndCtx ps [] (_, (Right (Incomplete x))) = or (map (flip elem ps) $ tMetadata x)
hasProjAndCtx (p:ps) (c:cs) (_, (Right (Incomplete x))) = or (map (flip elem (p:ps)) $ tMetadata x) && or (map (flip elem (c:cs)) $ tMetadata x)
hasProjAndCtx [] [] (_, (Right (Completed x))) = True
hasProjAndCtx [] cs (_, (Right (Completed x))) = or (map (flip elem cs) $ tMetadata x)
hasProjAndCtx ps [] (_, (Right (Completed x))) = or (map (flip elem ps) $ tMetadata x)
hasProjAndCtx (p:ps) (c:cs) (_, (Right (Completed x))) = or (map (flip elem (p:ps)) $ tMetadata x) && or (map (flip elem (c:cs)) $ tMetadata x)
hasProjAndCtx _ _ (_, (Left _)) = True

addTodo :: FilePath -> String -> IO ()
addTodo p item = do
  let r = validateLine item
  case r of
    Right r' -> do
      backupFile p
      TIO.appendFile p $ T.pack (item <> "\n")
    Left err -> die $ show err

completeTodo :: FilePath -> FilePath -> [Int] -> IO ()
completeTodo todoFile doneFile nums = do
  now <- getCurrentTime
  let nowDay = utctDay now
  c <- TIO.readFile todoFile
  -- TODO: Add completion date
  let (filtered, rest) = filterTodoLines nums $ T.lines c
  let filtered' = map (\x -> "x " <> x ) $ filtered
  appendTodoLines doneFile filtered'
  writeTodoLines todoFile rest
  putStrLn $ "Completed items:"
  putStrLn $ T.unpack $ concatTodoLines filtered'

deleteTodo :: FilePath -> [Int] -> IO ()
deleteTodo todoFile nums = do
  c <- TIO.readFile todoFile
  let (filtered, rest) = filterTodoLines nums $ T.lines c
  writeTodoLines todoFile rest
  putStrLn $ "Removed items:"
  putStrLn $ T.unpack $ concatTodoLines filtered

addPriority :: FilePath -> Int -> Priority -> IO ()
addPriority todoFile lineNum pri = do
  c <- TIO.readFile todoFile
  let all = T.lines c
  let item = getTodoLine lineNum all
  (Incomplete todo) <- parseItemOrDie item
  let t' = Incomplete $ todo{tPriority=Just pri}
  writeTodoLines todoFile $ modifyTodoLine lineNum all (show t')
  putStrLn $ "Prioritized item:"
  putStrLn $ show t'

deletePriority :: FilePath -> Int -> IO ()
deletePriority todoFile lineNum = do
  c <- TIO.readFile todoFile
  let all = T.lines c
  let item = getTodoLine lineNum all
  (Incomplete todo) <- parseItemOrDie item
  let t' = Incomplete $ todo{tPriority=Nothing}
  writeTodoLines todoFile $ modifyTodoLine lineNum all (show t')
  putStrLn $ "Deprioritized item:"
  putStrLn $ show t'

pullOrigins :: TodoConfig -> IO ()
pullOrigins c = do
  let f = todoFile c
  let os = origins c
  let githubOrigins = github os
  let gitlabOrigins = gitlab os
  ghs <- fetchGithubIssues githubOrigins
  gls <- fetchGitlabIssues gitlabOrigins
  c <- TIO.readFile f
  let todos = parse todoParser f $ T.unpack c
  case todos of
    Left err -> die $ show err
    Right ts -> do
      let ghtodos = map mkTodoFromGithubIssue ghs
      let gltodos = map mkTodoFromGitlabTodo gls
      let newTodos = nubBy compareByOrigin $ ts ++ ghtodos ++ gltodos
      backupFile f
      writeFile f (show newTodos)

compareByOrigin :: Todo -> Todo -> Bool
compareByOrigin (Incomplete t1) (Incomplete t2) = (tDescription t1) == (tDescription t2) && (tMetadata t1) == (tMetadata t2)
compareByOrigin (Completed _) (Completed _) = False

mkTodoFromGithubIssue :: GithubIssueDetails -> Todo
mkTodoFromGithubIssue g = Incomplete $ TodoItem { tPriority=Nothing
                                                , tDescription=githubIssueTitle g
                                                , tMetadata=[ MetadataProject $ Project $ githubIssueProject g
                                                            , MetadataContext $ Context $ githubIssueGroup g
                                                            , MetadataTag $ TagOrigin $ url2link $ githubIssueUrl g
                                                            ]
                                                , tCreatedAt=Nothing
                                                , tDoneAt=Nothing
                                                }
  where url2link (ForgeTypes.Url s) = Link s

mkTodoFromGitlabTodo :: GitlabTodoDetails -> Todo
mkTodoFromGitlabTodo g = Incomplete $ TodoItem { tPriority=Nothing
                                               , tDescription=gitlabTodoTitle g
                                               , tMetadata=[ MetadataProject $ Project $ gitlabTodoProject g
                                                           , MetadataContext $ Context $ gitlabTodoGroup g
                                                           , MetadataTag $ TagOrigin $ url2link $ gitlabTodoUrl g
                                                           ]
                                               , tCreatedAt=Nothing
                                               , tDoneAt=Nothing
                                               }
  where url2link (ForgeTypes.Url s) = Link s

fetchGithubIssues :: [FilePath] -> IO ([GithubIssueDetails])
fetchGithubIssues cs = do
  xss <- forM cs $ \c -> do
    c' <- ForgeUtils.readConfig c
    let token = githubAccessToken c'
    res <- Github.listIssues' token
    case res of
      Left err -> die $ show err
      Right xs -> return xs
  return $ concat xss

fetchGitlabIssues :: [FilePath] -> IO ([GitlabTodoDetails])
fetchGitlabIssues cs = do
  xss <- forM cs $ \c -> do
    c' <- ForgeUtils.readConfig c
    let token = gitlabAccessToken c'
    res <- Gitlab.listTodos' token
    case res of
      Left err -> die $ show err
      Right xs -> return xs
  return $ concat xss

getTodoLine :: Int -> [T.Text] -> String
getTodoLine 0 xs   = T.unpack $ xs !! 0
getTodoLine num xs = T.unpack $ xs !! (num - 1)

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

parseItemOrDie :: String -> IO (Todo)
parseItemOrDie i = do
  let todo = parse todoItem "" i
  case todo of
    Left err   -> die $ show err
    Right item -> return item

backupFile :: FilePath -> IO ()
backupFile p = copyFile p (p <> ".bak")
