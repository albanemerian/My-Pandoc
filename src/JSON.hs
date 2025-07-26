{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- JSON
-}

module JSON where

import Control.Applicative (Alternative(..))

import Data.Functor (($>))

import Document (
    Parsing(..),
    Document(..),
    Header(..),
    Content(..),
    Format(..),
    MyPandocArgs(..),
    Parser(..),
    parseChar,
    parseWhitespace,
    parseMany
    )


parseString :: String -> Parser String
parseString "" = Parser $ \input -> Right ("", input)
parseString (c:cs) = do
    parseChar c
    rest <- parseString cs
    return (c : rest)

parseStringChar :: Parser Char
parseStringChar =
    (parseChar '\\' *> parseEscapedChar) <|> Parser { runParser = parse }
    where
        parse (x:xs) | x /= '"' && x /= '\\' = Right (x, xs)
        parse _ = Left "Unexpected end of string"
        parseEscapedChar = parseChar '"' $> '"' <|>
            parseChar '\\' $> '\\' <|> parseChar 'n' $> '\n' <|>
            parseChar 'r' $> '\r' <|> parseChar 't' $> '\t'

parseString' :: Parser String
parseString' = do
    parseChar '"'
    content <- parseMany parseStringChar
    parseChar '"'
    return content

parseStringFromKey :: String -> Parser String
parseStringFromKey key =
    parseWhitespace *> parseChar '"' *> parseString key *>
    parseChar '"' *> parseWhitespace *> parseChar ':' *>
    parseWhitespace *> parseString' <* parseWhitespace

parseContentFromKey :: String -> Parser Content
parseContentFromKey key =
    parseWhitespace *> parseChar '"' *> parseString key *>
    parseChar '"' *> parseWhitespace *> parseChar ':' *>
    parseWhitespace *> parseJsonContent <* parseWhitespace

-- | This function parses a Json text
parseJsonText :: Parser Content
parseJsonText = Text . filter (/= '\\') <$> parseString'

-- | This function parses a Json italic text
parseJsonItalic :: Parser Content
parseJsonItalic = do
    parseChar '{'
    content <- parseContentFromKey "italic"
    parseChar '}'
    return $ Italic content

-- | This function parses a Json bold text
parseJsonBold :: Parser Content
parseJsonBold = do
    parseChar '{'
    content <- parseContentFromKey "bold"
    parseChar '}'
    return $ Bold content

-- | This function parses a Json code
parseJsonCode :: Parser Content
parseJsonCode = do
    parseChar '{'
    content <- parseStringFromKey "code"
    parseChar '}'
    return $ Code content

-- | This function parses a Json link
parseJsonLink :: Parser Content
parseJsonLink = do
    parseChar '{' *> parseWhitespace *> parseChar '"' *>
        parseString "link" *> parseChar '"' *> parseWhitespace *>
        parseChar ':' *> parseWhitespace *> parseChar '{'
    url <- parseStringFromKey "url"
    parseChar ','
    content <- parseContentFromKey "content"
    parseWhitespace *> parseChar '}' *> parseWhitespace *> parseChar '}'
    return $ Link url content

-- | This function parses a Json image
parseJsonImage :: Parser Content
parseJsonImage = do
    parseChar '{' *> parseWhitespace *> parseChar '"' *>
        parseString "image" *> parseChar '"' *> parseWhitespace *>
        parseChar ':' *> parseWhitespace *> parseChar '{'
    url <- parseStringFromKey "url"
    parseChar ','
    alt <- parseContentFromKey "alt"
    parseWhitespace *> parseChar '}' *> parseWhitespace *> parseChar '}'
    return $ Image url alt

-- | This function parses a Json paragraph
parseJsonParagraph :: Parser Content
parseJsonParagraph = do
    parseChar '[' *> parseWhitespace
    contents <- parseJsonParagraphContent
    parseChar ']'
    return $ Paragraph contents

parseJsonParagraphContent :: Parser [Content]
parseJsonParagraphContent =
    parseMany (
        parseJsonContent <*
        parseWhitespace <*
        (parseChar ',' <|> pure ',') <*
        parseWhitespace
        )

-- | This function parses a Json list
parseJsonSection :: Parser Content
parseJsonSection = do
    parseChar '{' *> parseWhitespace *> parseChar '"' *>
        parseString "section" *> parseChar '"' *> parseWhitespace *>
        parseChar ':' *> parseWhitespace *> parseChar '{' *> parseWhitespace
    title <- (parseTitle <* parseWhitespace <* parseChar ',') <|> pure Nothing
    contents <- parseJsonSectionContent
    title' <- (parseChar ',' *> parseTitle) <|> pure Nothing
    parseWhitespace *> parseChar '}' *> parseWhitespace *> parseChar '}'
    return $ Section (title' <|> title) contents

parseJsonSectionContent :: Parser [Content]
parseJsonSectionContent =
    parseWhitespace *> parseChar '"' *> parseString "content" *> parseChar '"'
    *> parseChar ':' *> parseWhitespace *> parseChar '[' *> parseWhitespace
    *> parseJsonParagraphContent
    <* parseWhitespace <* parseChar ']' <* parseWhitespace

-- | This function parses a Json title
parseTitle :: Parser (Maybe String)
parseTitle = Just <$> parseStringFromKey "title"

-- | This function parses a Json code block
parseJsonCodeBlock :: Parser Content
parseJsonCodeBlock = CodeBlock <$> (
    parseChar '{' *> parseWhitespace *> parseChar '"' *>
    parseString "codeblock" *> parseChar '"' *> parseWhitespace *>
    parseChar ':' *> parseWhitespace *> parseJsonCodeBlockContent <*
    parseWhitespace <* parseChar '}'
    )

parseJsonCodeBlockContent :: Parser [Content]
parseJsonCodeBlockContent =
    parseChar '[' *> parseWhitespace *> parseJsonParagraphContent
    <* parseWhitespace <* parseChar ']'

-- | This function parses a Json list
parseJsonList :: Parser Content
parseJsonList = List <$> (
    parseChar '{' *> parseWhitespace *> parseChar '"' *> parseString "list" *>
    parseChar '"' *> parseWhitespace *> parseChar ':' *> parseWhitespace *>
    parseJsonListItems <* parseWhitespace <* parseChar '}'
    )

parseJsonListItems :: Parser [Content]
parseJsonListItems = do
    parseChar '['
    parseWhitespace
    items <- parseMany ( parseJsonContent <* parseWhitespace <*
        (parseChar ',' <|> pure ',') <* parseWhitespace)
    parseChar ']'
    return items

-- | This function parses a Json header
parseJsonHeader :: Parser Header
parseJsonHeader = do
    parseWhitespace *> parseChar '"' *> parseString "header" *>
        parseChar '"' *> parseWhitespace *> parseChar ':' *>
        parseWhitespace *> parseChar '{' *> parseWhitespace
    pairs <- parseHeaderPairs
    parseWhitespace *> parseChar '}'
    let (t, a, d) = (maybe "" id (getLast "title" pairs),
            getLast "author" pairs, getLast "date" pairs)
    return Header {title = t, author = a, date = d}

-- Helper to get the last value of a key in the header
getLast :: String -> [(String, String)] -> Maybe String
getLast k pairs =
    case [v | (key, v) <- pairs, key == k] of
        [] -> Nothing
        xs -> Just (last xs)

-- Helper to parse all key-value pairs in the header
parseHeaderPairs :: Parser [(String, String)]
parseHeaderPairs = parsePairList parseHeaderPair

-- Helper to parse a comma-separated list of pairs
parsePairList :: Parser a -> Parser [a]
parsePairList p = (:) <$> p <*> (
    (parseWhitespace *> parseChar ',' *> parseWhitespace *> parsePairList p)
    <|> pure []
    )

-- Helper to parse a single key-value pair (title, author, or date)
parseHeaderPair :: Parser (String, String)
parseHeaderPair = do
    parseWhitespace *> parseChar '"'
    key <- parseString "title" <|> parseString "author" <|> parseString "date"
    parseChar '"' *> parseWhitespace *> parseChar ':' *> parseWhitespace
    value <- parseString'
    return (key, value)

-- | This function parses a Json content list
parseJsonContent :: Parser Content
parseJsonContent = parseWhitespace *> parseJsonText <|>
    parseJsonItalic <|> parseJsonBold <|> parseJsonCode <|>
    parseJsonLink <|> parseJsonImage <|> parseJsonCodeBlock <|>
    parseJsonList <|> parseJsonSection <|> parseJsonParagraph

-- | This function parses a Json body
parseJsonBody :: Parser [Content]
parseJsonBody =
    parseChar '"' *> parseString "body" *> parseChar '"' *>
    parseChar ':' *> parseWhitespace *>
    parseChar '[' *> parseWhitespace *>
    parseJsonParagraphContent
    <* parseChar ']'

-- | This function parses a Json file
parseJsonFile :: Parser Document
parseJsonFile = do
    parseChar '{' *> parseWhitespace
    header <- (parseJsonHeader <* parseWhitespace <* parseChar ','
        <* parseWhitespace) <|> pure EmptyHeader
    body <- parseJsonBody
    header' <- (parseWhitespace *> parseChar ',' *> parseJsonHeader)
        <|> pure EmptyHeader
    parseWhitespace <* parseChar '}'
    return $ Document {header = if header' == EmptyHeader
        then header else header', body = body}

-- | This function is the entry point to the Json parser
parseJson :: Parsing
parseJson args =
    case runParser parseJsonFile (fileContent args) of
        Left err -> Left err
        Right (doc, _) -> Right doc
