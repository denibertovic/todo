{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo.Lib where

import           Control.Monad      (when)
import qualified Data.ByteString    as BS
import           Data.List          (sort)
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
  let d = todoDir c </> "done.txt"
  case cmd of
    ListTodos filter   -> listTodos t filter
    AddTodo l          -> addTodo t l
    CompleteTodo lines -> completeTodo t d lines
    DeleteTodo lines   -> deleteTodo t lines

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

listTodos :: FilePath -> [String] -> IO ()
listTodos p filters = do
  let projects = [mkProject x | x <- filters, isProject x]
  let contexts = [mkContext x | x <- filters, isContext x]
  c <- TIO.readFile p
  let todos =  sort $ filter (hasProjAndCtx projects contexts) $ zip [1..] $ map (parse todoItem p) $ map T.unpack $ T.lines c
  mapM_ printItem todos

printItem :: (Int, Either ParseError Todo) -> IO ()
printItem (lineNum, item) = case item of
  Left err -> colorPrintChunks $ [chunk (show lineNum) & fore green, chunk " ", chunk $ show err, chunk "\n"]
  Right i  -> colorPrintChunks $ [chunk (show lineNum) & fore green, chunk " ", chunkize i, chunk "\n"]

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
hasProjAndCtx _ _ (_, (Right (Completed x))) = False
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
  backupFile todoFile
  now <- getCurrentTime
  let nowDay = utctDay now
  c <- TIO.readFile todoFile
  -- TODO: Add completion date
  let filtered = T.intercalate "\n" ["x " <> x | (_, x) <- filter (\(l, _) -> l `elem` nums) $ zip [1..] $ T.lines c]
  TIO.appendFile doneFile (filtered <> "\n")
  let rest = T.intercalate "\n" [x | (_, x) <- filter (\(l, _) -> not $ l `elem` nums) $ zip [1..] $ T.lines c]
  TIO.writeFile todoFile (rest <> "\n")
  putStrLn $ "Completed items:"
  putStrLn $ T.unpack filtered

deleteTodo :: FilePath -> [Int] -> IO ()
deleteTodo todoFile nums = do
  backupFile todoFile
  c <- TIO.readFile todoFile
  let filtered = T.intercalate "\n" [x | (_, x) <- filter (\(l, _) -> l `elem` nums) $ zip [1..] $ T.lines c]
  let rest = T.intercalate "\n" [x | (_, x) <- filter (\(l, _) -> not $ l `elem` nums) $ zip [1..] $ T.lines c]
  TIO.writeFile todoFile (rest <> "\n")
  putStrLn $ "Removed items:"
  putStrLn $ T.unpack filtered

backupFile :: FilePath -> IO ()
backupFile p = copyFile p (p <> ".bak")
