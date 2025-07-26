{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- Parse-Test
-}

module Parsetest (testParsing) where

import Test.Hspec
import Parser
import Document (MyPandocArgs(..), Format(..))
import GHC.RTS.Flags (TraceFlags(timestamp))

testParsing :: Spec
testParsing = do
    describe "parseOutputFormat" $ do
        it "parses 'xml' as XML format" $
            parseOutputFormat "xml" `shouldBe` Right XML

        it "parses 'json' as JSON format" $
            parseOutputFormat "json" `shouldBe` Right JSON

        it "parses 'markdown' as Markdown format" $
            parseOutputFormat "markdown" `shouldBe` Right Markdown

        it "returns an error for invalid format" $
            parseOutputFormat "invalid" `shouldBe` Left "Invalid output format: invalid"

    describe "validateFlags" $ do
        it "validates correct flags" $
            validateFlags ["-i", "input.txt", "-f", "json"] `shouldBe` Right ()

        it "returns an error for invalid flags" $
            validateFlags ["-x", "input.txt"] `shouldBe` Left "Invalid flags: -x"

    describe "parseArgs" $ do
        it "parses valid arguments with all flags" $
            parseArgs ["-i", "input.txt", "-f", "json", "-o", "output.txt", "-e", "xml"]
                `shouldBe` Right (MyPandocArgs "input.txt" "" JSON (Just "output.txt") (Just XML))

        it "parses valid arguments without optional flags" $
            parseArgs ["-i", "input.txt", "-f", "json"]
                `shouldBe` Right (MyPandocArgs "input.txt" "" JSON Nothing Nothing)

        it "returns an error for missing input file flag" $
            parseArgs ["-f", "json"] `shouldBe` Left "Missing flag: -i (input file)"

        it "returns an error for missing output format flag" $
            parseArgs ["-i", "input.txt"] `shouldBe` Left "Missing flag: -f (output forma)"

        it "returns an error for invalid output format" $
            parseArgs ["-i", "input.txt", "-f", "invalid"]
                `shouldBe` Left "Invalid output format: invalid"

        it "returns an error for invalid optional flag value" $
            parseArgs ["-i", "input.txt", "-f", "json", "-e", "-invalid"]
                `shouldBe` Left "Invalid flags: -invalid"




