{-# LANGUAGE OverloadedStrings #-}

module Test.Lib.LibSpec where

import           Todo.Lib
import           Todo.Types

import           Data.Monoid     ((<>))
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           System.FilePath ((</>))
import           System.IO.Temp  (withSystemTempDirectory)

import           Test.Hspec      (Spec, describe, it, shouldBe, shouldNotBe)
import           Text.Parsec     (between, eof)


-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Lib" $ do
    it "Tests adding a todo" $ do
      withTempTodo $ \todoF doneF -> do
        let t = "(A) test test +test"
        addTodo todoF t
        content <- TIO.readFile todoF
        (T.unpack $ last $ T.lines content) == t `shouldBe` True

    it "Tests completing a todo" $ do
      withTempTodo $ \todoF doneF-> do
        completeTodo todoF doneF [4]
        content <- TIO.readFile todoF
        (last $ T.lines content) == (testTodoTxtLines !! 2) `shouldBe` True

    it "Tests deleting a todo" $ do
      withTempTodo $ \todoF doneF-> do
        deleteTodo todoF [4]
        content <- TIO.readFile todoF
        (last $ T.lines content) == (testTodoTxtLines !! 2) `shouldBe` True

    it "Tests adding priority for a todo" $ do
      withTempTodo $ \todoF doneF-> do
        addPriority todoF 4 A
        content <- TIO.readFile todoF
        (last $ T.lines content) == ("(A) " <> testTodoTxtLines !! 3) `shouldBe` True

    it "Tests changing priority for a todo" $ do
      withTempTodo $ \todoF doneF-> do
        addPriority todoF 1 B
        content <- TIO.readFile todoF
        (head $ T.lines content) == "(B) First Foo @context" `shouldBe` True

    it "Tests removing priority from a todo" $ do
      withTempTodo $ \todoF doneF-> do
        deletePriority todoF 1
        content <- TIO.readFile todoF
        (head $ T.lines content) == "First Foo @context" `shouldBe` True

testConfig :: FilePath -> TodoConfig
testConfig d = TodoConfig { todoDir=d
                          , todoFile=d </> "todo.txt"
                          , reportFile=d </> "done.txt"
                          , origins=TodoConfigOrigin {github=[], gitlab=[]}
                          }


testTodoTxtLines :: [T.Text]
testTodoTxtLines = [ "(A) First Foo @context"
                   , "(B) Second Bar +project @context"
                   , "Third Baz baz:dinamo"
                   , "Test origin bla origin:https://github.com/foo/bar/issue/1"
                   ]

withTempTodo :: (FilePath -> FilePath -> IO a) -> IO a
withTempTodo f = do
    withSystemTempDirectory "todo--" $ \d -> do
      let cfg = testConfig d
      TIO.writeFile (todoFile cfg) (concatTodoLines testTodoTxtLines)
      TIO.writeFile (d </> "done.txt") ""
      f (todoFile cfg) (d </> "done.txt")
