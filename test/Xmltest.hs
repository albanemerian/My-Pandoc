{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- Xml-test
-}

module Xmltest where
import XML
import Document
import Test.Hspec (Spec, describe, it, shouldBe)

testXml :: Spec
testXml = do
    describe "XML.tokenize" $ do
        it "should tokenize a simple XML string" $ do
            let input = "<document><header title=\"Test\"><author>John Doe</author><date>2025-04-15</date></header><body><paragraph>Hello, world!</paragraph></body></document>"
            let expected = Right
                    [ OpenT "document" []
                    , OpenT "header" [("title", "Test")]
                    , OpenT "author" []
                    , TextN "John Doe"
                    , CloseT "author"
                    , OpenT "date" []
                    , TextN "2025-04-15"
                    , CloseT "date"
                    , CloseT "header"
                    , OpenT "body" []
                    , OpenT "paragraph" []
                    , TextN "Hello, world!"
                    , CloseT "paragraph"
                    , CloseT "body"
                    , CloseT "document"
                    ]
            tokenize input `shouldBe` expected

    describe "XML.parseDocument" $ do
        it "should parse a valid XML document" $ do
            let tokens =
                    [ OpenT "document" []
                    , OpenT "header" [("title", "Test")]
                    , OpenT "author" []
                    , TextN "John Doe"
                    , CloseT "author"
                    , OpenT "date" []
                    , TextN "2025-04-15"
                    , CloseT "date"
                    , CloseT "header"
                    , OpenT "body" []
                    , OpenT "paragraph" []
                    , TextN "Hello, world!"
                    , CloseT "paragraph"
                    , CloseT "body"
                    , CloseT "document"
                    ]
            let expected = Right $ Document (Header "Test" (Just "John Doe") (Just "2025-04-15")) [Paragraph [Text "Hello, world!"]]
            parseDocument tokens `shouldBe` expected

    describe "XML.parseContent" $ do
        it "should parse a section with a title and subcontents" $ do
            let tokens = [OpenT "section" [("title", "Section Title")], OpenT "paragraph" [], TextN "Content", CloseT "paragraph", CloseT "section"]
            let expected = Right (Section (Just "Section Title") [Paragraph [Text "Content"]], [])
            parseContent tokens `shouldBe` expected

        it "should parse a codeblock with multiple paragraphs" $ do
            let tokens = [OpenT "codeblock" [], OpenT "paragraph" [], TextN "Line 1", CloseT "paragraph", OpenT "paragraph" [], TextN "Line 2", CloseT "paragraph", CloseT "codeblock"]
            let expected = Right (CodeBlock [Paragraph [Text "Line 1"], Paragraph [Text "Line 2"]], [])
            parseContent tokens `shouldBe` expected

        it "should parse a list with multiple items" $ do
            let tokens = [OpenT "list" [], OpenT "paragraph" [], TextN "Item 1", CloseT "paragraph", OpenT "paragraph" [], TextN "Item 2", CloseT "paragraph", CloseT "list"]
            let expected = Right (List [Paragraph [Text "Item 1"], Paragraph [Text "Item 2"]], [])
            parseContent tokens `shouldBe` expected

        it "should return an error for unknown content" $ do
            let tokens = [OpenT "unknown" [], TextN "Content", CloseT "unknown"]
            parseContent tokens `shouldBe` Left "Unknown content"

    describe "XML.parseCodeBlockContents" $ do
        it "should parse multiple paragraphs in a codeblock" $ do
            let tokens = [OpenT "paragraph" [], TextN "Line 1", CloseT "paragraph", OpenT "paragraph" [], TextN "Line 2", CloseT "paragraph", CloseT "codeblock"]
            let expected = Right ([Paragraph [Text "Line 1"], Paragraph [Text "Line 2"]], [])
            parseCodeBlockContents tokens `shouldBe` expected

        it "should handle an empty codeblock" $ do
            let tokens = [CloseT "codeblock"]
            let expected = Right ([], [])
            parseCodeBlockContents tokens `shouldBe` expected

        it "should return an error for an unexpected end in a codeblock" $ do
            let tokens = []
            parseCodeBlockContents tokens `shouldBe` Left "Unknown content"

    describe "XML.parseListItems" $ do
        it "should parse a list with multiple items" $ do
            let tokens = [OpenT "paragraph" [], TextN "Item 1", CloseT "paragraph", OpenT "paragraph" [], TextN "Item 2", CloseT "paragraph", CloseT "list"]
            let expected = Right ([Paragraph [Text "Item 1"], Paragraph [Text "Item 2"]], [])
            parseListItems tokens `shouldBe` expected

        it "should handle an empty list" $ do
            let tokens = [CloseT "list"]
            let expected = Right ([], [])
            parseListItems tokens `shouldBe` expected

        it "should return an error for an invalid list format" $ do
            let tokens = [OpenT "paragraph" [], TextN "Item 1"]
            parseListItems tokens `shouldBe` Left "Invalid list format"
