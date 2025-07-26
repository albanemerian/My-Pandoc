{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- JsonTest
-}

module JsonTest where

import JSON
import Document
import Test.Hspec (Spec, describe, it, shouldBe)

testJson :: Spec
testJson = do
    describe "JSON.parseString" $ do
        it "should parse hello in hello world" $ do
            runParser (parseString "hello") "hello world" `shouldBe` Right ("hello", " world")

        it "should parse hello in hello" $ do
            runParser (parseString "hello") "hello" `shouldBe` Right ("hello", "")

        it "should fail to parse hello in world" $ do
            runParser (parseString "hello") "world" `shouldBe` Left "Expected 'h'"

    describe "JSON.parseString'" $ do
        it "should parse \"hello\" in \"hello\"" $ do
            runParser parseString' "\"hello\"" `shouldBe` Right ("hello", "")

        it "should parse \"hello\" in \"hello\" world" $ do
            runParser parseString' "\"hello\" world" `shouldBe` Right ("hello", " world")

        it "should fail to parse in \"hello" $ do
            runParser parseString' "\"hello" `shouldBe` Left "Expected '\"'"

    describe "JSON.parseJsonText" $ do
        it "should parse \"hello\" in \"hello\"" $ do
            runParser parseJsonText "\"hello\"" `shouldBe` Right (Text "hello", "")

        it "should parse \"hello\" in \"hello\" world" $ do
            runParser parseJsonText "\"hello\" world" `shouldBe` Right (Text "hello", " world")

        it "should fail to parse in \"hello" $ do
            runParser parseJsonText "\"hello" `shouldBe` Left "Expected '\"'"

    describe "JSON.parseJsonItalic" $ do
        it "should parse well formatted italic" $ do
            runParser parseJsonItalic "{\"italic\": \"hello\"}" `shouldBe` Right (Italic (Text "hello"), "")

    describe "JSON.parseJsonBold" $ do
        it "should parse well formatted bold" $ do
            runParser parseJsonBold "{\"bold\": \"hello\"}" `shouldBe` Right (Bold (Text "hello"), "")

    describe "JSON.parseJsonCode" $ do
        it "should parse well formatted code" $ do
            runParser parseJsonCode "{\"code\": \"hello\"}" `shouldBe` Right (Code "hello", "")

    describe "JSON.parseJsonLink" $ do
        it "should parse well formatted link" $ do
            runParser parseJsonLink "{\"link\": {\"url\": \"hello\", \"content\": \"world\"}}"
                `shouldBe` Right (Link "hello" (Text "world"), "")

    describe "JSON.parseJsonImage" $ do
        it "should parse well formatted image" $ do
            runParser parseJsonImage "{\"image\": {\"url\": \"hello\", \"alt\": \"world\"}}"
                `shouldBe` Right (Image "hello" (Text "world"), "")

    describe "JSON.parseJsonParagraph" $ do
        it "should parse well formatted paragraph" $ do
            runParser parseJsonParagraph "[ {\"italic\": \"hello\"}, {\"bold\": \"world\"}]"
                `shouldBe` Right (Paragraph [Italic (Text "hello"), Bold (Text "world")], "")

    describe "JSON.parseJsonSection" $ do
        it "should parse well formatted section" $ do
            runParser parseJsonSection "{\"section\": {\"title\": \"hello\", \"content\": [{\"italic\": \"world\"}]}}"
                `shouldBe` Right (Section (Just "hello") [Italic (Text "world")], "")

        it "should parse well formatted section without title" $ do
            runParser parseJsonSection "{\"section\": {\"content\": [{\"italic\": \"world\"}]}}"
                `shouldBe` Right (Section Nothing [Italic (Text "world")], "")

    describe "JSON.parseJsonCodeBlock" $ do
        it "should parse well formatted code block" $ do
            runParser parseJsonCodeBlock "{\"codeblock\": [\"hello\", \"world\"]}"
                `shouldBe` Right (CodeBlock [Text "hello", Text "world"], "")

    describe "JSON.parseJsonList" $ do
        it "should parse well formatted list" $ do
            runParser parseJsonList "{\"list\": [{\"italic\": \"hello\"}, {\"bold\": \"world\"}]}"
                `shouldBe` Right (List [Italic (Text "hello"), Bold (Text "world")], "")
