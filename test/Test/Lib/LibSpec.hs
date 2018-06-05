{-# LANGUAGE OverloadedStrings #-}

module Test.Lib.LibSpec where

import           Prelude         (print)
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
        runRIO app $ completeTodo [1]
        content <- TIO.readFile (todoFile $ app ^. configL)
        (last $ T.lines content) == (getTodo 5 testTodoTxtLines) `shouldBe` True

    it "Tests deleting a todo" $ do
      withTempTodo $ \app -> do
        runRIO app $ deleteTodo [1]
        content <- TIO.readFile (todoFile $ app ^. configL)
        (last $ T.lines content) == (getTodo 5 testTodoTxtLines) `shouldBe` True

    it "Tests adding priority for a todo" $ do
      withTempTodo $ \app -> do
        runRIO app $ addPriority 3 A
        content <- TIO.readFile (todoFile $ app ^. configL)
        (getTodo 3 $ T.lines content) == ("(A) " <> (getTodo 3 testTodoTxtLines)) `shouldBe` True

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

    it "Tests filtering done todos" $ do
      withTempTodo $ \app -> do
        content <- TIO.readFile (todoFile $ app ^. configL)
        let (f, r) = filterDoneTodos $ T.lines content
        f == [getTodo 5 testTodoTxtLines] `shouldBe` True
        r == (take 4 testTodoTxtLines) `shouldBe` True

    it "Tests archiving todos" $ do
      withTempTodo $ \app -> do
        runRIO app $ archiveTodos
        content <- TIO.readFile (todoFile $ app ^. configL)
        done <- TIO.readFile (todoDoneFile $ app ^. configL)
        (last $ T.lines done) == (getTodo 5 testTodoTxtLines) `shouldBe` True


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
                   , "x Some todo that's done"
                   ]

-- | Helper function so we don't confused 0 based and 1 based indexes
getTodo :: Int -> [T.Text] -> T.Text
getTodo i xs = xs !! (i - 1)

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
