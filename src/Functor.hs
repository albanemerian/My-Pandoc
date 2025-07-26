{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- Functor
-}

module Functor where
import Document (Parsing(..), Writing(..), MyPandocArgs(..), Document(..), Format(..))
import XML (parseXml)
import JSON (parseJson)
import Markdown (parseMarkdown)
import Formater (writingJson, writingXml, writingMarkdown)


-- | This module defines the functions used to parse and write documents
parserTable :: [(Format, Parsing)]
parserTable =
  [ (XML, parseXml)
  , (JSON, parseJson)
  , (Markdown, parseMarkdown)
  ]

-- | This function tries to parse a document using all available parsers
tryAllParsers :: MyPandocArgs -> Either String Document
tryAllParsers args = go parserTable
  where
    go [] = Left "Could not detect input format"
    go ((_, p):ps) = case p args of
        Right doc -> Right doc
        Left _    -> go ps
-- | This function tries the parsing using the specified format from the cli
tryParsers :: MyPandocArgs -> Either String Document
tryParsers args = maybe (tryAllParsers args) tryByFormat (inputFormat args)
  where
    tryByFormat fmt = case lookup fmt parserTable of
        Just p  -> p args
        Nothing -> Left "Unsupported input format"

-- | This function defines the available formats for
-- | writing data from Strucutre to format
writingTable :: [(Format, Writing)]
writingTable =
  [ (XML, writingXml)
  , (JSON, writingJson)
  , (Markdown, writingMarkdown)
  ]

-- | This function tries to write a document using all available writers
tryWriting :: Document -> MyPandocArgs -> Either String String
tryWriting doc args = tryByFormat (outputFormat args)
  where
    tryByFormat fmt = case lookup fmt writingTable of
        Just w  -> w doc
        Nothing -> Left "Unsupported output format"
