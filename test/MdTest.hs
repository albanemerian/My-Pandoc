{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- MarkdownTest
-}

module MdTest where

import Markdown
import Document
import Test.Hspec (Spec, describe, it, shouldBe)

testMd :: Spec
testMd = do
    describe "Markdown.parseString" $ do
        it "should parse hello in hello world" $ do
            runParser (parseString "hello") "hello world" `shouldBe` Right ("hello", " world")

        it "should parse hello in hello" $ do
            runParser (parseString "hello") "hello" `shouldBe` Right ("hello", "")

        it "should fail to parse hello in world" $ do
            runParser (parseString "hello") "world" `shouldBe` Left "Expected 'h'"
