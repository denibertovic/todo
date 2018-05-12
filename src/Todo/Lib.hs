{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo.Lib where

import           Control.Monad       (void)
import           Control.Monad       (join)
import           Data.Bool           (not)
import           Data.Char           (GeneralCategory (..), isSeparator)
import           Data.Char           (isAlphaNum)
import qualified Data.HashMap.Strict as HM
import           Data.List           (nub)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Void           (Void)
import           Text.Pretty.Simple  (pPrint)

import           Text.Parsec
import           Text.Parsec.Char    (char, digit, string)
import           Text.Parsec.String  (Parser)
import qualified Text.Parsec.Token   as P

import           Todo.Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sc :: Parser ()
sc = skipMany $ char ' '

priority :: Parser Priority
priority = do
  _ <- char '('
  p <- upper
  _ <- char ')'
  sc
  return $ read [p]

context :: Parser Metadata
context = do
  _ <- char '@'
  -- ctx <- manyTill alphaNum (space <|> endOfLine)
  ctx <- many1 (noneOf " \n\r\t")
  sc
  return $ MetadataContext $ Context $ T.pack ctx

project :: Parser Metadata
project = do
  _ <- char '+'
  -- p <- manyTill alphaNum (space <|> endOfLine)
  p <- many1 (noneOf " \n\r\t")
  sc
  return $ MetadataProject $ Project $ T.pack p

tag :: Parser Metadata
tag = do
  key <- tagKey
  _ <- char ':'
  value <- tagValue
  sc
  return $ MetadataTag (key, value)

tagKey :: Parser String
tagKey = many1 (noneOf ": \n\r\t")

tagValue :: Parser String
tagValue = manyTill (noneOf " \n\r\t") (space <|> endOfLine)

word :: Parser Metadata
word = do
  -- word <- manyTill (noneOf " \t\n\r") (space <|> endOfLine)
  word <- many1 $ noneOf " \t\n\r"
  sc
  return $ MetadataString word

metadata :: Parser Metadata
metadata = do
  -- tags need to be at the end of the line as per the spec so they'll get
  -- tried last
  m <- project <|> context <|> try word <|> tag
  return m

incompleteTask :: Parser Todo
incompleteTask = do
  pri <- optionMaybe priority
  -- startDate <- optionMaybe date
  m <- many1 $ metadata
  _ <- endOfLine
  let desc = [ T.pack s | x@(MetadataString s) <- m]
  let m' = filter (not . isMetadataString) m
  return $ Incomplete TodoItem { tDescription=T.intercalate " " desc
                               , tPriority=pri
                               , tCreatedAt=Nothing
                               , tDueAt=Nothing
                               , tMetadata=m'}

isMetadataString :: Metadata -> Bool
isMetadataString m = case m of
  MetadataString _ -> True
  otherwsie        -> False

-- |Complete Task
-- It is assumed that a completed task starts with x and a optional completion
-- date. It is also assumed that the rest of the string will be an incomplete
-- task.
-- completedTask :: Parser Todo
-- completedTask = do
--   _ <- char 'x'
--   _ <- sc
--   -- endDate <- optionalMaybe date
--   _ <- sc
--   (Incomplete item) <- incompleteTask
--   _ <- endOfLine
--   return $ Completed item

-- todo :: Parser Todo
-- todo = do
--   _ <- try (many (char '\n'))
--   t <- choice [ completedTask, incompleteTask ]
--   return t

todoParser :: Parser [Todo]
todoParser = between sc eof (many incompleteTask)
-- todoParser = between sc eof (many (incompleteTask <* (sc <|> eof)))

dinamo :: IO ()
dinamo = do
  let path = "/tmp/todo.txt"
  c <- TIO.readFile path
  let r  = parse todoParser "/tmp/todo.txt" (T.unpack c) :: Either ParseError [Todo]
  pPrint r
  case r of
    Right r' -> pPrint $ length r'
    Left err -> pPrint err
