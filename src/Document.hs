{-
-- EPITECH PROJECT, 2025
-- bsPandoc
-- File description:
-- Document
-}

module Document where

import Control.Applicative (Alternative(empty, (<|>)))
import Data.Functor (($>))

import Text.Printf ( printf )

-- | This module defines the data structures used to represent a document
data Document = Document
  { header  :: Header
  , body    :: [Content]
  } deriving (Show, Eq)

data Header =
  EmptyHeader
  | Header
    { title   :: String
    , author  :: Maybe String
    , date    :: Maybe String
    }
  deriving (Show, Eq)

data Content
  = Empty
  | Text String
  | Italic Content
  | Bold Content
  | Code String
  | Link String Content
  | Image String Content
  | Paragraph [Content]
  | Section (Maybe String) [Content]
  | CodeBlock [Content]
  | List [Content]
  deriving (Show, Eq)

type Parsing = MyPandocArgs -> Either String Document

type Writing = Document -> Either String String

data Format = XML | JSON | Markdown deriving (Show, Eq)


-- | This module defines the data structure used to represent the command line arguments
data MyPandocArgs = MyPandocArgs
    { inputFile :: FilePath
    , fileContent :: String
    , outputFormat :: Format
    , outputFile :: Maybe String
    , inputFormat :: Maybe Format
    } deriving (Show, Eq)


-- | This module defines the parser for any type, with its instances
newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser $ \str ->
        case runParser parser str of
            Left err -> Left err
            Right (result, rest) -> Right (fct result, rest)

instance Applicative Parser where
    pure x = Parser $ \str -> Right (x, str)
    Parser f <*> Parser g = Parser $ \str ->
        case f str of
            Left err -> Left err
            Right (fct, rest) ->
                case g rest of
                    Left err -> Left err
                    Right (result, remaining) -> Right (fct result, remaining)

instance Alternative Parser where
    empty = Parser $ \_ -> Left "No parser found"
    Parser f <|> Parser g = Parser $ \str ->
        case f str of
            Left _ -> g str
            Right (result, rest) -> Right (result, rest)

instance Monad Parser where
    return = pure
    Parser f >>= g = Parser $ \str ->
        case f str of
            Left err -> Left err
            Right (result, rest) -> runParser (g result) rest


parseChar :: Char -> Parser Char
parseChar c = Parser { runParser = parse }
  where
    parse (x:xs) | x == c   = Right (c, xs)
    parse _                 = Left $ printf "Expected '%c'" c

parseAnyChar :: String -> Parser Char
parseAnyChar chars = Parser { runParser = parse }
  where
    parse (x:xs) | x `elem` chars = Right (x, xs)
    parse [] = Left "Unexpected end of input"
    parse _  = Left $ printf "Expected one of '%s'" chars

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = p1 <|> p2

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = (,) <$> p1 <*> p2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = f <$> p1 <*> p2

parseMany :: Parser a -> Parser [a]
parseMany p = parseSome p <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome p = parseAndWith (:) p (parseMany p)

parseWhitespace :: Parser String
parseWhitespace = parseMany (parseAnyChar [' ', '\t', '\n', '\r'])
