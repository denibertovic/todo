{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo.Parser where

import           Control.Monad       (join, void)
import           Data.Bool           (not)
import qualified Data.HashMap.Strict as HM
import           Data.List           (nub)
import           Data.List           (intercalate)
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
    _ <- sc
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
  _ <- sc
  return $ read [p]

context :: Parser Metadata
context = try $ do
  _ <- char '@'
  -- ctx <- manyTill alphaNum (space <|> endOfLine)
  ctx <- many1 (noneOf " \n\r\t")
  _ <- sc <|> eof
  return $ MetadataContext $ Context ctx

project :: Parser Metadata
project = try $ do
  _ <- char '+'
  -- p <- manyTill alphaNum (space <|> endOfLine)
  p <- many1 (noneOf " \n\r\t")
  _ <- sc <|> eof
  return $ MetadataProject $ Project p

kv :: Parser Metadata
kv = try $ do
  key <- tagKey
  _ <- char ':'
  value <- tagValue
  _ <- sc <|> eof
  return $ MetadataTag $ Tag key value

tagKey :: Parser String
tagKey = many1 (noneOf ": \n\r\t")

tagValue :: Parser String
tagValue = many1 (noneOf " \n\r\t")

duedate :: Parser Metadata
duedate = try $ do
  _ <- string "due:"
  d <- date
  _ <- sc <|> eof
  return $ MetadataTag $ TagDueDate d

origin :: Parser Metadata
origin = try $ do
  _ <- string "origin:"
  (MetadataLink l) <- link
  _ <- sc <|> eof
  return $ MetadataTag $ TagOrigin l

tag :: Parser Metadata
tag = choice [ duedate, origin, kv ]

colon :: Parser Char
colon = char ':'

link :: Parser Metadata
link = try $ do
  proto <- string "https:" <|> string "http:"
  rest <- many1 $ noneOf " \n\r\t"
  _ <- sc <|> eof
  return $ MetadataLink $ Link $ proto <> rest

word :: Parser Metadata
word = do
  -- word <- manyTill (noneOf " \t\n\r") (space <|> endOfLine)
  word <- many1 $ noneOf " \t\n\r"
  _ <- sc <|> eof
  return $ MetadataString word

metadata :: Parser Metadata
metadata =
  -- tags need to be at the end of the line as per the spec so they'll get
  -- tried last
  choice [ project, context, link, tag, word ]

pleaseNoMore :: Parser ()
pleaseNoMore = void (endOfLine) <|> eof


incompleteTask :: Parser Todo
incompleteTask = do
    pri <- optionMaybe priority
    _ <- sc
    startDate <- optionMaybe date
    _ <- sc
    endDate <- optionMaybe date
    _ <- sc
    m <- many1 metadata
    _ <- pleaseNoMore
    let m' = filter (isMetadataStringOrLink) m
    let desc = [ getIt x | x <- m']
    let m'' = filter (not . isMetadataStringOrLink) m
    return $ Incomplete TodoItem { tDescription=intercalate " " desc
                                 , tPriority=pri
                                 , tCreatedAt=startDate
                                 , tDoneAt=endDate
                                 , tMetadata=m''}
  where getIt (MetadataString s)      = s
        getIt (MetadataLink (Link s)) = s

isMetadataString :: Metadata -> Bool
isMetadataString m = case m of
  MetadataString _ -> True
  otherwise        -> False

isMetadataStringOrLink :: Metadata -> Bool
isMetadataStringOrLink m = case m of
  MetadataString _ -> True
  MetadataLink _   -> True
  otherwise        -> False

-- | Complete Task
-- It is assumed that a completed task starts with x and a optional completion
-- date. It is also assumed that the rest of the string will be an incomplete
-- task.
completedTask :: Parser Todo
completedTask = do
  _ <- char 'x'
  _ <- sc
  (Incomplete item) <- incompleteTask
  return $ Completed item

todoItem :: Parser Todo
todoItem = choice [ completedTask, incompleteTask ]

todoParser :: Parser [Todo]
todoParser = between sc eof (many todoItem)

validateLine :: String -> Either ParseError Todo
validateLine content = parse todoItem "" content

