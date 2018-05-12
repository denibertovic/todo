{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo.Lib where

import           Control.Monad       (join, void)
import           Data.Bool           (not)
import qualified Data.HashMap.Strict as HM
import           Data.List           (nub)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Time.Calendar  (Day (..), fromGregorian)
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

-- |Date: 2017-02-23
-- Supports 2 or 4 digit year, and 1 or 2 digit month and day.
date :: Parser Day
date = do
    year <- twoToFourDigits
    _ <- char '-'
    month <- oneToTwoDigits
    _ <- char '-'
    day <- oneToTwoDigits
    return $ fromGregorian (convertYear $ read year) (read month) (read day)
  where oneToTwoDigits = do
          x <- digit
          y <- option ' ' $ digit
          return (x:y:[])
        twoToFourDigits = do
          w <- digit
          x <- digit
          y <- option ' ' $ digit
          z <- option ' ' $ digit
          return (w:x:y:z:[])
        convertYear x = if x < 100
                        then x + 2000
                        else x

priority :: Parser Priority
priority = do
  _ <- char '('
  p <- upper
  _ <- char ')'
  sc
  return $ read [p]

context :: Parser Metadata
context = try $ do
  _ <- char '@'
  -- ctx <- manyTill alphaNum (space <|> endOfLine)
  ctx <- many1 (noneOf " \n\r\t")
  sc
  return $ MetadataContext $ Context $ T.pack ctx

project :: Parser Metadata
project = try $ do
  _ <- char '+'
  -- p <- manyTill alphaNum (space <|> endOfLine)
  p <- many1 (noneOf " \n\r\t")
  sc
  return $ MetadataProject $ Project $ T.pack p

tag :: Parser Metadata
tag = try $ do
  key <- tagKey
  _ <- char ':'
  value <- tagValue
  sc
  return $ MetadataTag $ Tag key value

tagKey :: Parser String
tagKey = many1 (noneOf ": \n\r\t")

tagValue :: Parser String
tagValue = many1 (noneOf " \n\r\t")

duedate :: Parser Metadata
duedate = try $ do
  _ <- string "due:"
  d <- date
  _ <- sc
  return $ MetadataTag $ TagDueDate d

colon :: Parser Char
colon = char ':'

link :: Parser Metadata
link = try $ do
  proto <- string "https:" <|> string "http:"
  rest <- many1 $ noneOf " \n\r\t"
  _ <- sc
  return $ MetadataLink $ Link $ proto <> rest


word :: Parser Metadata
word = do
  -- word <- manyTill (noneOf " \t\n\r") (space <|> endOfLine)
  word <- many1 $ noneOf " \t\n\r"
  sc
  return $ MetadataString word

metadata :: Parser Metadata
metadata =
  -- tags need to be at the end of the line as per the spec so they'll get
  -- tried last
  choice [ project, context, link, tag, word ]

incompleteTask :: Parser Todo
incompleteTask = do
    pri <- optionMaybe priority
    -- startDate <- optionMaybe date
    m <- many1 metadata
    _ <- endOfLine
    let m' = filter (isMetadataStringOrLink) m
    let desc = [ packIt x | x <- m']
    let m'' = filter (not . isMetadataStringOrLink) m
    return $ Incomplete TodoItem { tDescription=T.intercalate " " desc
                                 , tPriority=pri
                                 , tCreatedAt=Nothing
                                 , tDueAt=Nothing
                                 , tMetadata=m''}
  where packIt (MetadataString s)      = T.pack s
        packIt (MetadataLink (Link s)) = T.pack s

isMetadataString :: Metadata -> Bool
isMetadataString m = case m of
  MetadataString _ -> True
  otherwise        -> False

isMetadataStringOrLink :: Metadata -> Bool
isMetadataStringOrLink m = case m of
  MetadataString _ -> True
  MetadataLink _   -> True
  otherwise        -> False

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
