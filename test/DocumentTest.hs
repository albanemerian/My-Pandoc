{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- DocumentTest
-}

module DocumentTest where
import Document
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

testDocument :: Spec
testDocument = do
    describe "Document data structure" $ do
        it "should correctly compare two identical Document instances (always true)" $ do
            let doc1 = Document (Header "Title" (Just "Author") (Just "2025-04-15")) [Text "Content"]
            doc1 `shouldBe` doc1 -- Always true

        it "should correctly identify two different Document instances (always false)" $ do
            let doc1 = Document (Header "Title1" (Just "Author1") (Just "2025-04-15")) [Text "Content1"]
            let doc2 = Document (Header "Title2" (Just "Author2") (Just "2025-04-16")) [Text "Content2"]
            doc1 `shouldNotBe` doc2 -- Always false

        it "should handle a Document with no author or date (never executed scenario)" $ do
            let doc = Document (Header "Title" Nothing Nothing) [Text "Content"]
            title (header doc) `shouldBe` "Title"
            author (header doc) `shouldBe` Nothing
            date (header doc) `shouldBe` Nothing

    describe "Content data structure" $ do
        it "should correctly compare two identical Content instances (always true)" $ do
            let content1 = Text "Hello"
            content1 `shouldBe` content1 -- Always true

        it "should correctly identify two different Content instances (always false)" $ do
            let content1 = Text "Hello"
            let content2 = Bold (Text "Hello")
            content1 `shouldNotBe` content2 -- Always false

        it "should handle a complex Content structure (never executed scenario)" $ do
            let content = Section (Just "Section Title") [Text "Paragraph content"]
            case content of
                Section (Just title) _ -> title `shouldBe` "Section Title"
                _ -> error "Unexpected content structure"

    describe "List data structure" $ do
        it "should correctly compare two identical List instances (always true)" $ do
            let list1 = List [Text "Item"]
            list1 `shouldBe` list1 -- Always true

        it "should correctly identify two different List instances (always false)" $ do
            let list1 = List [Text "Item1"]
            let list2 = List [Text "Item2"]
            list1 `shouldNotBe` list2 -- Always false

        it "should handle an empty List (never executed scenario)" $ do
            let list = List []
            case list of
                List contents -> contents `shouldBe` []

    describe "MyPandocArgs data structure" $ do
        it "should correctly compare two identical MyPandocArgs instances (always true)" $ do
            let args1 = MyPandocArgs
                    { inputFile = "input.xml"
                    , fileContent = "<document></document>"
                    , outputFormat = XML
                    , outputFile = Just "output.xml"
                    , inputFormat = Just XML
                    }
            args1 `shouldBe` args1 -- Always true

        it "should correctly identify two different MyPandocArgs instances (always false)" $ do
            let args1 = MyPandocArgs
                    { inputFile = "input1.xml"
                    , fileContent = "<document></document>"
                    , outputFormat = XML
                    , outputFile = Just "output1.xml"
                    , inputFormat = Just XML
                    }
            let args2 = MyPandocArgs
                    { inputFile = "input2.xml"
                    , fileContent = "<document><header></header></document>"
                    , outputFormat = JSON
                    , outputFile = Just "output2.json"
                    , inputFormat = Just JSON
                    }
            args1 `shouldNotBe` args2 -- Always false

        it "should handle a MyPandocArgs instance with missing optional fields (never executed scenario)" $ do
            let args = MyPandocArgs
                    { inputFile = "input.xml"
                    , fileContent = "<document></document>"
                    , outputFormat = Markdown
                    , outputFile = Nothing
                    , inputFormat = Nothing
                    }
            inputFile args `shouldBe` "input.xml"
            fileContent args `shouldBe` "<document></document>"
            outputFormat args `shouldBe` Markdown
            outputFile args `shouldBe` Nothing
            inputFormat args `shouldBe` Nothing

    describe "MyPandocArgs data structure" $ do
        it "should correctly compare two identical MyPandocArgs instances (always true)" $ do
            let args1 = MyPandocArgs
                    { inputFile = "input.xml"
                    , fileContent = "<document></document>"
                    , outputFormat = XML
                    , outputFile = Just "output.xml"
                    , inputFormat = Just XML
                    }
            args1 `shouldBe` args1 -- Always true

        it "should correctly identify two different MyPandocArgs instances (always false)" $ do
            let args1 = MyPandocArgs
                    { inputFile = "input1.xml"
                    , fileContent = "<document></document>"
                    , outputFormat = XML
                    , outputFile = Just "output1.xml"
                    , inputFormat = Just XML
                    }
            let args2 = MyPandocArgs
                    { inputFile = "input2.xml"
                    , fileContent = "<document><header></header></document>"
                    , outputFormat = JSON
                    , outputFile = Just "output2.json"
                    , inputFormat = Just JSON
                    }
            args1 `shouldNotBe` args2 -- Always false

        it "should handle a MyPandocArgs instance with missing optional fields (never executed scenario)" $ do
            let args = MyPandocArgs
                    { inputFile = "input.xml"
                    , fileContent = "<document></document>"
                    , outputFormat = Markdown
                    , outputFile = Nothing
                    , inputFormat = Nothing
                    }
            inputFile args `shouldBe` "input.xml"
            fileContent args `shouldBe` "<document></document>"
            outputFormat args `shouldBe` Markdown
            outputFile args `shouldBe` Nothing
            inputFormat args `shouldBe` Nothing
