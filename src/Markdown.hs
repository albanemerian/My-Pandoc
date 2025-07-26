{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- Markdown
-}

module Markdown where
import Debug.Trace (trace)
import Text.Printf
import Control.Applicative (Alternative(..), optional)
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
    parseMany,
    parseSome
    )


-- | Return a string composed of characters until the first occurrence of a key
parseUntilKey :: Char -> Parser String
parseUntilKey k = Parser $ \input ->
    let (line, rest) = span (/= k) input
    in Right (line, drop 1 rest)

-- | Parse the content until the last character of the key
parseString :: String -> Parser String
parseString "" = Parser $ \input -> Right ("", input)
parseString (c:cs) = do
    parseChar c
    rest <- parseString cs
    return (c : rest)

-- | Return a string composed of characters after a key
parseStringFromKey :: String -> Parser String
parseStringFromKey key =
    parseWhitespace *> parseString key *> parseWhitespace *>
    parseChar ':' *> parseWhitespace *> parseUntilKey '\n'

-------------------------------------------------------------------

-- | Parse from a key to get a maybe value
parseMdField :: String -> Parser (Maybe String)
parseMdField key =
    (parseWhitespace *> (Just <$> parseStringFromKey key <* parseWhitespace))
    <|> pure Nothing

-- | Markdown parsing header function
parseMdHeader :: Parser Header
parseMdHeader = do
    parseWhitespace *> parseString "---" *> parseWhitespace
    t <- parseStringFromKey "title"
    a <- parseMdField "author" <|> pure Nothing
    d <- parseMdField "date" <|> pure Nothing
    parseWhitespace *> parseString "---" *> parseWhitespace
    return Header {title = t, author = a, date = d}

-------------------------------------------------------------------

isBoundary :: String -> Bool
isBoundary ('*':'*':_) = True
isBoundary ('*':_) = True
isBoundary ('`':_) = True
isBoundary ('[':_) = True
isBoundary ('!':'[':_) = True
isBoundary ('\n':_) = True
isBoundary [] = True
isBoundary _ = False

-- | To parse: TEXT
parseMdText :: Parser Content
parseMdText = Parser $ \input ->
    let go acc str@(_:_) | isBoundary str = Right (Text acc, str)
        go acc (c:cs) = go (acc ++ [c]) cs
        go acc [] = Right (Text acc, [])
    in go "" input

-- | To parse: ITALIC
parseMdItalic :: Parser Content
parseMdItalic = do
    parseChar '*'
    content <- parseMdContent
    parseChar '*'
    return $ Italic content

-- | To parse: BOLD
parseMdBold :: Parser Content
parseMdBold = do
    parseString "**"
    content <- parseMdContent
    parseString "**"
    return $ Bold content

-- | To parse: CODE
parseMdCode :: Parser Content
parseMdCode = do
    parseChar '`'
    content <- parseUntilKey '`'
    return $ Code content

-- | To parse: LINK
parseMdLink :: Parser Content
parseMdLink = do
    parseChar '['
    text <- parseUntilKey ']'
    parseChar '('
    url <- parseUntilKey ')'
    return $ Link url (Text text)

-- | To parse: IMAGE
parseMdImage :: Parser Content
parseMdImage = do
    parseChar '!' *> parseChar '['
    text <- parseUntilKey ']'
    parseChar '('
    url <- parseUntilKey ')'
    return $ Image url (Text text)

-- | To parse: LIST
parseMdList :: Parser Content
parseMdList = do
    items <- parseSome $ do
        parseWhitespace
        parseChar '-'
        parseWhitespace
        item <- parseMdContent
        parseChar '\n' <|> pure '\n'
        return item
    return $ List items

-- | To parse: CODEBLOCK
parseMdCodeBlock :: Parser Content
parseMdCodeBlock = do
    parseString "```" *> parseChar '\n'
    code <- parseMany (Parser $ \input -> case input of
        [] -> Left "Unexpected end of input in code block"
        ('\n':'`':'`':'`':_) -> Left "End of code block"
        (c:cs) -> Right (c, cs))
    parseChar '\n'
    parseString "```"
    parseChar '\n'
    return $ CodeBlock [Text code]

-- | Apply parse functions
parseMdContent :: Parser Content
parseMdContent =
    parseMdBold <|>
    parseMdItalic <|>
    parseMdCode <|>
    parseMdLink <|>
    parseMdImage <|>
    parseMdText

parseManySafe :: Parser a -> Parser [a]
parseManySafe p = Parser $ \input ->
    let loop acc str =
            case runParser p str of
                Right (x, rest) | rest /= str -> loop (acc ++ [x]) rest
                _ -> Right (acc, str)
    in loop [] input

-- | To parse: PARAGRAPH
parseMdParagraph :: Parser Content
parseMdParagraph = Parser $ \input ->
    let (line, rest) = break (== '\n') input
        rest' = drop 1 rest
    in if null line
        then Left "Empty line"
        else case runParser
            (parseManySafe (parseMdContent)) line of
                Left _ -> Right (Text line, rest')
                Right ([], _) -> Right (Empty, rest')
                Right (contents, _) -> Right (Paragraph contents, rest')

parseMdBlock :: Parser Content
parseMdBlock =
    parseMdList
    <|> parseMdCodeBlock
    <|> parseMdParagraph

skipEmptyLines :: Parser ()
skipEmptyLines = do
    _ <- parseMany $ Parser $ \input ->
        case input of
            '\n':rest -> Right ((), rest)
            _         -> Left "not empty"
    pure ()

-- | Parse a Markdown heading, returning (level, title)
parseMdHeading :: Parser (Int, String)
parseMdHeading = do
    parseWhitespace
    hashes <- parseSome (parseChar '#')
    parseWhitespace
    title <- parseUntilKey '\n'
    return (length hashes, title)

wrapSections :: Int -> Content -> Content
wrapSections n inner =
    if n <= 0
        then inner
    else Section Nothing [wrapSections (n - 1) inner]

parseSectionContent :: Int -> Int -> Parser Content
parseSectionContent level parentLevel =
    skipEmptyLines *>
    Parser (\input ->
        case runParser parseMdHeading input of
            Right ((nextLevel, _), _) | nextLevel <= parentLevel ->
                Left "End of section"
            _ -> Right ((), input))
    *> (parseMdSection level <|> parseMdBlock)

-- | Parse a section and its nested subsections
parseMdSection :: Int -> Parser Content
parseMdSection parentLevel = do
    (level, title) <- parseMdHeading
    content <- parseMany (parseSectionContent level parentLevel)
    let section = Section (Just title) (filter (/= Empty) content)
    if level > parentLevel + 1
        then return $ wrapSections (level - parentLevel - 1) section
    else return section

-- | Markdown parser body function
parseMdBody :: Parser [Content]
parseMdBody =
    skipEmptyLines *>
    parseMany (skipEmptyLines *> (parseMdSection 0 <|> parseMdBlock))

-------------------------------------------------------------------

-- | Global function to parse Markdown
parseMdFile :: Parser Document
parseMdFile = do
    header <- parseMdHeader
    body <- parseMdBody
    return $ Document { header = header, body = body }

-- | Markdown Parser
parseMarkdown :: Parsing
parseMarkdown args =
    case runParser parseMdFile (fileContent args) of
        Left err -> Left err
        Right (doc, _) -> Right doc
