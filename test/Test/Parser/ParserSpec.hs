{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.ParserSpec where

import           Todo.Lib
import           Todo.Types

import           Test.Hspec       (Spec, describe, it, shouldBe, shouldNotBe)
import           Text.Parsec      (between, eof)
import           Text.Parsec.Prim (many, parse)

-- | Required for auto-discovery
spec :: Spec
spec =
  describe "Parsers" $ do
    describe "Priority" $ do
      it "Matches (A) as Priority A" $ do
        parse priority "" "(A)" `shouldBe` Right A

      it "Doesn't match A as Priority A" $ do
        parse priority "" "A" `shouldNotBe` Right A

      it "Matches project +someProject1 as Project someProject1" $ do
        parse project "" "+someProject1 " `shouldBe` Right (MetadataProject $ Project "someProject1")

      it "Doesn't match project foo+someProject1 as Project someProject1" $ do
        parse project "" "foo+someProject1 " `shouldNotBe` Right (MetadataProject $ Project "someProject1")

      it "Matches context @someContext1 as Context someContext1" $ do
        parse context "" "@someContext1 " `shouldBe` Right (MetadataContext $ Context "someContext1")

      it "Doesn't match context foo@someContext1 as Context someContext1" $ do
        parse context "" "foo+someContext1 " `shouldNotBe` Right (MetadataContext $ Context "someContext1")

      it "Matches tag key:value as (key, value)" $ do
        parse tag "" "key:value " `shouldBe` Right (MetadataTag $ ("key", "value"))

      it "Matches word foo as foo" $ do
        parse word "" "foo " `shouldBe` Right (MetadataString "foo")

      it "Matches [Project, Context, Word, Word, Context, Project]" $ do
        parse (many metadata) "" sampleTodoRaw `shouldBe` Right sampleMetadata

      it "Matches TodoItem" $ do
        parse incompleteTask "" sampleTodoRaw `shouldBe` Right sampleTodo

      it "Matches TodoItem with Priority" $ do
        parse incompleteTask "" sampleTodoWithPriRaw `shouldBe` Right sampleTodoPriA

      it "Matches 2 TodoItems" $ do
        parse (between sc eof (many incompleteTask)) "" "(A) +project @context foo bar @context2 +project2\n+project @context foo bar @context2 +project2\n" `shouldBe` Right [sampleTodoPriA, sampleTodo]

      it "Matches TodoItem with Tags at the end" $ do
        parse incompleteTask "" sampleTodoWithPriWithTagsRaw `shouldBe` Right sampleTodoPriAWithTags


sampleTodoRaw = "+project @context foo bar @context2 +project2\n"
sampleTodoWithPriRaw = "(A) +project @context foo bar @context2 +project2\n"
sampleTodoWithPriWithTagsRaw = "(A) +project @context foo bar @context2 +project2 origin:fooBar\n"


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
                                 , tDueAt=Nothing
                                 , tMetadata=[ MetadataProject $ Project "project"
                                             , MetadataContext $ Context "context"
                                             , MetadataContext $ Context "context2"
                                             , MetadataProject $ Project "project2"
                                             ]
                                 }

sampleTodoPriA = Incomplete TodoItem { tDescription="foo bar"
                                 , tPriority=Just A
                                 , tCreatedAt=Nothing
                                 , tDueAt=Nothing
                                 , tMetadata=[ MetadataProject $ Project "project"
                                             , MetadataContext $ Context "context"
                                             , MetadataContext $ Context "context2"
                                             , MetadataProject $ Project "project2"
                                             ]
                                 }

sampleTodoPriAWithTags = Incomplete TodoItem { tDescription="foo bar"
                                 , tPriority=Just A
                                 , tCreatedAt=Nothing
                                 , tDueAt=Nothing
                                 , tMetadata=[ MetadataProject $ Project "project"
                                             , MetadataContext $ Context "context"
                                             , MetadataContext $ Context "context2"
                                             , MetadataProject $ Project "project2"
                                             , MetadataTag $  ("origin", "fooBar")
                                             ]
                                 }
