{-# LANGUAGE OverloadedStrings #-}

module Test.Lib.LibSpec where

import           RIO

import           Data.List       (head, last, (!!))
import           Todo.Lib
import           Todo.Types

import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           System.FilePath ((</>))

import           Test.Hspec      (Spec, describe, it, shouldBe, shouldNotBe)
import           Text.Parsec     (between, eof)


-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Lib" $ do
    it "Tests adding a todo" $ do
      withTempTodo $ \app -> do
        let t = "(A) test test +test"
        runRIO app $ addTodo t
        content <- TIO.readFile (todoFile $ app ^. configL)
        (last $ T.lines content) == t `shouldBe` True

    it "Tests completing a todo" $ do
      withTempTodo $ \app -> do
        runRIO app $ completeTodo [4]
        content <- TIO.readFile (todoFile $ app ^. configL)
        (last $ T.lines content) == (testTodoTxtLines !! 2) `shouldBe` True

    it "Tests deleting a todo" $ do
      withTempTodo $ \app -> do
        runRIO app $ deleteTodo [4]
        content <- TIO.readFile (todoFile $ app ^. configL)
        (last $ T.lines content) == (testTodoTxtLines !! 2) `shouldBe` True

    it "Tests adding priority for a todo" $ do
      withTempTodo $ \app -> do
        runRIO app $ addPriority 4 A
        content <- TIO.readFile (todoFile $ app ^. configL)
        (last $ T.lines content) == ("(A) " <> testTodoTxtLines !! 3) `shouldBe` True

    it "Tests changing priority for a todo" $ do
      withTempTodo $ \app -> do
        runRIO app $ addPriority 1 B
        content <- TIO.readFile (todoFile $ app ^. configL)
        (head $ T.lines content) == "(B) First Foo @context" `shouldBe` True

    it "Tests removing priority from a todo" $ do
      withTempTodo $ \app -> do
        runRIO app $ deletePriority 1
        content <- TIO.readFile (todoFile $ app ^. configL)
        (head $ T.lines content) == "First Foo @context" `shouldBe` True

testConfig :: FilePath -> TodoConfig
testConfig d = TodoConfig { todoDir=d
                          , todoFile=d </> "todo.txt"
                          , todoDoneFile=d </> "done.txt"
                          , todoReportFile=d </> "report.txt"
                          , todoRemotes=[]
                          }

testTodoTxtLines :: [T.Text]
testTodoTxtLines = [ "(A) First Foo @context"
                   , "(B) Second Bar +project @context"
                   , "Third Baz baz:dinamo"
                   , "Test origin bla origin:https://github.com/foo/bar/issue/1"
                   ]

withTempTodo :: (App -> IO a) -> IO a
withTempTodo f = do
    withSystemTempDirectory "todo--" $ \d -> do
      let cfg = testConfig d
      TIO.writeFile (todoFile cfg) (concatTodoLines testTodoTxtLines)
      TIO.writeFile (todoDoneFile cfg) ""
      lo <- logOptionsHandle stderr True
      withLogFunc lo $ \l -> do
        let app = App { appConfig=cfg, appLogger=l }
        f app
