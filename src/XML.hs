{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- XML
-}

module XML where

import Document
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)

-- | This module defines the functions used to format a document in XML
data XmlToken
  = OpenT String [(String, String)]
  | CloseT String
  | TextN String
  deriving (Show, Eq)

-- | This functions takes the string from the files and defines its type
tokenize :: String -> Either String [XmlToken]
tokenize "" = Right []
tokenize ('<':'/':rest) =
  let (tag, rem') = span (/= '>') rest
   in fmap (CloseT (trim tag) :) (tokenize (drop 1 rem'))
tokenize ('<':rest) =
  let (rawTag, rem') = span (/= '>') rest
      (tagName, attrs) = parseTag (trim rawTag)
   in fmap (OpenT tagName attrs :) (tokenize (drop 1 rem'))
tokenize s =
  let (txt, rest) = span (/= '<') s
   in if all isSpace txt
         then tokenize rest
         else fmap (TextN txt :) (tokenize rest)

-- | This functions is used to parse the tag and its attributes (< , </, >)
parseTag :: String -> (String, [(String, String)])
parseTag input =
  let (name, rest) = break isSpace input
      attrs = parseAttributes (trim rest)
   in (name, attrs)

parseAttributes :: String -> [(String, String)]
parseAttributes "" = []
parseAttributes s =
  case break (== '=') s of
    (key, '=':rest) ->
      let (value, rest') = parseQuotedValue rest
       in (key, value) : parseAttributes (trim rest')
    _ -> []

parseQuotedValue :: String -> (String, String)
parseQuotedValue ('"':rest) =
  let (value, rest') = break (== '"') rest
   in (value, drop 1 rest') -- Drop the closing quote
parseQuotedValue s = break isSpace s

-- | This function removes trailing spaces
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parseDocument :: [XmlToken] -> Either String Document
parseDocument (OpenT "document" _ : rest) = do
  (header, rest1) <- parseHeader rest
  (bodyContents, rest2) <- parseBody rest1
  case rest2 of
    (CloseT "document" : _) ->
      Right $ Document { header = header, body = bodyContents }
    _ -> Left "Missing </document>"
parseDocument _ = Left "Expected <document>"

-- | This function parses the header of the document
parseHeader :: [XmlToken] -> Either String (Header, [XmlToken])
parseHeader (OpenT "header" attrs : rest) = do
  let title = lookup "title" attrs
  (author, date, afterHeader) <- parseHeaderFields rest
  case afterHeader of
    (CloseT "header" : xs) -> Right
      (Header {title=maybe "" id title, author=author, date=date}, xs)
    _ -> Left "Missing </header>"
parseHeader _ = Left "Expected <header>"

parseHeaderFields :: [XmlToken] -> Either String (Maybe String, Maybe String, [XmlToken])
parseHeaderFields tokens = go tokens Nothing Nothing
  where
    go (OpenT "author" [] : TextN txt : CloseT "author" : xs) _ d =
      go xs (Just txt) d
    go (OpenT "date" [] : TextN txt : CloseT "date" : xs) a _ =
      go xs a (Just txt)
    go xs a d = Right (a, d, xs)

parseBody :: [XmlToken] -> Either String ([Content], [XmlToken])
parseBody (OpenT "body" [] : rest) = do
  (contents, rest') <- parseContentsUntil "body" rest
  Right (contents, rest')
parseBody _ = Left "Expected <body>"

parseContentsUntil :: String -> [XmlToken] -> Either String ([Content], [XmlToken])
parseContentsUntil endTag tokens = go tokens []
  where
    go (CloseT tag : xs) acc | tag == endTag = Right (reverse acc, xs)
    go ts acc = do
      (content, rest) <- parseContent ts
      go rest (content : acc)

parseContent :: [XmlToken] -> Either String (Content, [XmlToken])
parseContent (OpenT "paragraph" [] : rest) = do
  (inlines, rest1) <- parseInlineUntil "paragraph" rest
  Right (Paragraph inlines, rest1)

parseContent (OpenT "section" attrs : rest) = do
  let title = lookup "title" attrs
  (subContents, rest1) <- parseContentsUntil "section" rest
  Right (Section title subContents, rest1)

parseContent (OpenT "codeblock" _ : rest) = do
  (contents, rest') <- parseCodeBlockContents rest
  Right (CodeBlock contents, rest')

parseContent (OpenT "list" [] : rest) = do
  (items, rest1) <- parseListItems rest
  Right (List items, rest1)

parseContent _ = Left "Unknown content"

parseCodeBlockContents :: [XmlToken] -> Either String ([Content], [XmlToken])
parseCodeBlockContents (CloseT "codeblock" : rest) =
  Right ([], rest)
parseCodeBlockContents tokens = do
  (content, rest1) <- parseContent tokens
  (nextContents, rest2) <- parseCodeBlockContents rest1
  Right (content : nextContents, rest2)

inlineToStr :: Content -> String
inlineToStr (Text s) = s
inlineToStr (Bold (Text s)) = s
inlineToStr (Italic (Text s)) = s
inlineToStr (Code s) = s
inlineToStr (Link _ (Text s)) = s
inlineToStr (Image _ (Text s)) = s
inlineToStr _ = ""

parseInlineUntil :: String -> [XmlToken] -> Either String ([Content], [XmlToken])
parseInlineUntil endTag = go []
  where
    go acc (CloseT tag : rest)
      | tag == endTag = Right (acc, rest)
    go acc (TextN txt : rest) = go (acc ++ [Text txt]) rest
    go acc toks = case matchInline toks of
        Just (c, rest) -> go (acc ++ [c]) rest
        Nothing        -> Left "Unrecognized inline content"

matchInline :: [XmlToken] -> Maybe (Content, [XmlToken])
matchInline (OpenT tag attrs : TextN t : CloseT close : r)
  | tag == close = case tag of
      "bold"   -> Just (Bold (Text t), r)
      "italic" -> Just (Italic (Text t), r)
      "code"   -> Just (Code t, r)
      "link"   -> fmap (\url -> (Link url (Text t), r)) (lookup "url" attrs)
      "image"  -> fmap (\url -> (Image url (Text t), r)) (lookup "url" attrs)
      _        -> Nothing
matchInline _ = Nothing

parseListItems :: [XmlToken] -> Either String ([Content], [XmlToken])
parseListItems (CloseT "list" : xs) = Right ([], xs)
parseListItems (OpenT "paragraph" [] : TextN txt : CloseT "paragraph" : xs) = do
  (rest, after) <- parseListItems xs
  Right (Paragraph [Text txt] : rest, after)
parseListItems _ = Left "Invalid list format"

-- | This function is the entry point to the Xml parser
parseXml :: MyPandocArgs -> Either String Document
parseXml args =
  case tokenize (fileContent args) of
    Right tokens -> parseDocument tokens
    Left err -> Left err
