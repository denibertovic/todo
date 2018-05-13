{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.ParserSpec where

import           Todo.Parser
import           Todo.Types

import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           Data.Time.Calendar (Day (..), fromGregorian)
import           Test.Hspec         (Spec, describe, it, shouldBe, shouldNotBe)
import           Text.Parsec        (between, eof)
import           Text.Parsec.Prim   (many, parse)

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Parsers" $ do
    describe "Priority" $ do
      it "Matches (A) as Priority A" $
        parse priority "" "(A)" `shouldBe` Right A

      it "Doesn't match A as Priority A" $
        parse priority "" "A" `shouldNotBe` Right A

    describe "Date" $ do
      it "Matches 2007-01-02" $ do
        parse date "" "2007-01-02" `shouldBe` Right (fromGregorian 2007 1 2)

      it "Matches 2007-1-2" $ do
        parse date "" "2007-1-2" `shouldBe` Right (fromGregorian 2007 1 2)

      it "Matches 07-1-2" $ do
        parse date "" "07-1-2" `shouldBe` Right (fromGregorian 2007 1 2)

      it "Doesn't match 7-1-2" $ do
        parse date "" "7-1-2" `shouldNotBe` Right (fromGregorian 2007 1 2)

    describe "Project" $ do

      it "Matches project +someProject1 as Project someProject1" $
        parse project "" "+someProject1 " `shouldBe` Right (MetadataProject $ Project "someProject1")

      it "Doesn't match project foo+someProject1 as Project someProject1" $
        parse project "" "foo+someProject1 " `shouldNotBe` Right (MetadataProject $ Project "someProject1")

    describe "Context" $ do

      it "Matches context @someContext1 as Context someContext1" $
        parse context "" "@someContext1 " `shouldBe` Right (MetadataContext $ Context "someContext1")

      it "Doesn't match context foo@someContext1 as Context someContext1" $
        parse context "" "foo+someContext1 " `shouldNotBe` Right (MetadataContext $ Context "someContext1")

    describe "Tag" $ do

      it "Matches tag key:value as (key, value)" $
        parse tag "" "key:value " `shouldBe` Right (MetadataTag $ Tag "key" "value")

    describe "Description/Word" $ do

      it "Matches word foo as foo" $
        parse word "" "foo " `shouldBe` Right (MetadataString "foo")

    describe "Todo Item" $ do

      it "Matches [Project, Context, Word, Word, Context, Project]" $
        parse (many metadata) "" sampleTodoRaw `shouldBe` Right sampleMetadata

      it "Matches TodoItem" $
        parse incompleteTask "" sampleTodoRaw `shouldBe` Right sampleTodo

      it "Matches TodoItem with Priority" $
        parse incompleteTask "" sampleTodoWithPriRaw `shouldBe` Right sampleTodoPriA

      it "Matches 2 TodoItems" $
        parse (between sc eof (many incompleteTask)) "" (sampleTodoWithPriRaw <> sampleTodoRaw) `shouldBe` Right [sampleTodoPriA, sampleTodo]

      it "Matches TodoItem with Tags at the end" $
        parse incompleteTask "" sampleTodoWithPriWithTagsRaw `shouldBe` Right sampleTodoPriAWithTags

      it "Matches Url as Text in Item" $
        parse incompleteTask "" sampleTodoWithUrlRaw `shouldBe` Right sampleTodoWithUrl



sampleTodoRaw = "+project @context foo bar @context2 +project2\n"
sampleTodoWithPriRaw = "(A) +project @context foo bar @context2 +project2\n"
sampleTodoWithPriWithTagsRaw = "(A) +project @context foo bar @context2 +project2 tagKeyFoo:bar\n"
sampleTodoWithUrlRaw = "This is a url: https://example.com/foo\n"


sampleMetadata = [ MetadataProject $ Project "project"
       , MetadataContext $ Context "context"
       , MetadataString "foo"
       , MetadataString "bar"
       , MetadataContext $ Context "context2"
       , MetadataProject $ Project "project2"
       ]

sampleTodo = Incomplete TodoItem { tDescription="foo bar"
                                 , tPriority=Nothing
                                 , tCreatedAt=Nothing
                                 , tDoneAt=Nothing
                                 , tMetadata=[ MetadataProject $ Project "project"
                                             , MetadataContext $ Context "context"
                                             , MetadataContext $ Context "context2"
                                             , MetadataProject $ Project "project2"
                                             ]
                                 }

sampleTodoPriA = Incomplete TodoItem { tDescription="foo bar"
                                 , tPriority=Just A
                                 , tCreatedAt=Nothing
                                 , tDoneAt=Nothing
                                 , tMetadata=[ MetadataProject $ Project "project"
                                             , MetadataContext $ Context "context"
                                             , MetadataContext $ Context "context2"
                                             , MetadataProject $ Project "project2"
                                             ]
                                 }

sampleTodoPriAWithTags = Incomplete TodoItem { tDescription="foo bar"
                                 , tPriority=Just A
                                 , tCreatedAt=Nothing
                                 , tDoneAt=Nothing
                                 , tMetadata=[ MetadataProject $ Project "project"
                                             , MetadataContext $ Context "context"
                                             , MetadataContext $ Context "context2"
                                             , MetadataProject $ Project "project2"
                                             , MetadataTag $  Tag "tagKeyFoo" "bar"
                                             ]
                                 }
sampleTodoWithUrl = Incomplete TodoItem { tDescription=T.unpack $ T.strip $ T.pack sampleTodoWithUrlRaw
                                 , tPriority=Nothing
                                 , tCreatedAt=Nothing
                                 , tDoneAt=Nothing
                                 , tMetadata=[]
                                 }
