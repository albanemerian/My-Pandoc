{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- Formater
-}

module Formater where
import Text.Printf
import Document (
  Parsing(..),
  Writing(..),
  MyPandocArgs(..),
  Document(..), 
  Format(..),
  Content(..),
  Header(..)
  )


-- | This module defines the functions used to format a document

writingXml :: Writing
writingXml doc = Right $ documentToXml 0 doc

documentToXml :: Int -> Document -> String
documentToXml indent (Document (Header title author date) contents) =
  "<document>\n" ++
  "<header title=\"" ++ escapeXml title ++ "\">\n" ++
  maybe "" (\a -> "<author>" ++ escapeXml a ++ "</author>\n") author ++
  maybe "" (\d -> "<date>" ++ escapeXml d ++ "</date>\n") date ++
  "</header>\n" ++
  "<body>\n" ++
  concatMap (contentToXmlMixed indent) (groupContents contents) ++
  "</body>\n" ++
  "</document>\n"

contentToXml :: Int -> Content -> String
contentToXml indent (Text str) = escapeXml str
contentToXml indent Empty = ""
contentToXml indent (Italic content) =
  "<italic>" ++ contentToXml indent content ++ "</italic>"
contentToXml indent (Bold content) =
  "<bold>" ++ contentToXml indent content ++ "</bold>"
contentToXml indent (Code str) =
  "<code>" ++ escapeXml str ++ "</code>"
contentToXml indent (Link url content) =
  "<link url=\"" ++ escapeXml url ++ "\">" ++
  contentToXml indent content ++ "</link>"
contentToXml indent (Image url alt) =
  "<image url=\"" ++ escapeXml url ++ "\">" ++
  contentToXml indent alt ++ "</image>"
contentToXml indent (Paragraph contents) =
  concatMap (contentToXmlMixed indent) (groupContents contents)
contentToXml indent (Section title contents) =
  "<section" ++ maybe "" (\t -> " title=\"" ++ escapeXml t ++ "\"")
  title ++ ">\n" ++ concatMap (contentToXml indent) contents ++ "</section>\n"
contentToXml indent (CodeBlock contents) =
  "<codeblock>\n" ++
  concatMap (contentToXml indent) contents ++
  "</codeblock>\n"
contentToXml indent (List items) =
  "<list>\n" ++
  concatMap (contentToXml indent) items ++
  "</list>\n"

-- | Helper to render "paragraph content" (inline elements)
contentToXmlParagraphChild :: Int -> Content -> String
contentToXmlParagraphChild indent (Text str) = escapeXml str
contentToXmlParagraphChild indent (Italic c) = "<italic>" ++
  contentToXml indent c ++ "</italic>"
contentToXmlParagraphChild indent (Bold c) = "<bold>" ++
  contentToXml indent c ++ "</bold>"
contentToXmlParagraphChild indent (Code str) = "<code>" ++
  escapeXml str ++ "</code>"
contentToXmlParagraphChild indent (Link url c) = "<link url=\"" ++
  escapeXml url ++ "\">" ++ contentToXml indent c ++ "</link>"
contentToXmlParagraphChild indent (Image url c) = "<image url=\"" ++
  escapeXml url ++ "\">" ++ contentToXml indent c ++ "</image>"
contentToXmlParagraphChild _ (Paragraph c) = concatMap
  (contentToXmlParagraphChild 0) c
contentToXmlParagraphChild _ (Section title cs) =
  "<section" ++ maybe "" (\t -> " title=\"" ++ escapeXml t ++ "\"")
  title ++ ">\n" ++ concatMap (contentToXml 0) cs ++ "</section>\n"
contentToXmlParagraphChild _ (List cs) =
  "<list>\n" ++ concatMap (contentToXml 0) cs ++ "</list>\n"
contentToXmlParagraphChild _ (CodeBlock cs) =
  "<codeblock>\n" ++ concatMap (contentToXml 0) cs ++ "</codeblock>\n"
contentToXmlParagraphChild _ Empty = ""

-- | Split content between inline and block elements
data ContentGroup = InlineGroup [Content] | BlockGroup Content

groupContents :: [Content] -> [ContentGroup]
groupContents [] = []
groupContents (c:cs)
  | isInline c = case groupContents cs of
      (InlineGroup cs' : rest) -> InlineGroup (c:cs') : rest
      rest -> InlineGroup [c] : rest
  | otherwise = BlockGroup c : groupContents cs

isInline :: Content -> Bool
isInline (Text _) = True
isInline (Italic _) = True
isInline (Bold _) = True
isInline (Code _) = True
isInline (Link _ _) = True
isInline (Image _ _) = True
isInline Empty = True
isInline _ = False

contentToXmlMixed :: Int -> ContentGroup -> String
contentToXmlMixed indent (InlineGroup cs) =
  "<paragraph>" ++ concatMap
  (contentToXmlParagraphChild indent) cs ++ "</paragraph>\n"
contentToXmlMixed indent (BlockGroup c) =
  contentToXml indent c

-- | This function handles special characters that don't translate to Xml
escapeXml :: String -> String
escapeXml = concatMap escapeChar
  where
    escapeChar '<' = "<"
    escapeChar '>' = ">"
    escapeChar '&' = "&"
    escapeChar '"' = "\""
    escapeChar c = [c]

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` ['\n', '\t'])



--     escapeChar '<' = "&lt;"
--     escapeChar '>' = "&gt;"
--     escapeChar '&' = "&amp;"
--     escapeChar '"' = "&quot;"











-- | This function handles the writing of a JSON document
writeJsonContents :: Document -> String
writeJsonContents (Document (Header title author date) contents) =
  "{\n\"header\": {\n" ++
  "\"title\": \"" ++ title ++ "\"" ++
  maybe "" (\a -> ",\n\"author\": \"" ++ a ++ "\"") author ++
  maybe "" (\d -> ",\n\"date\": \"" ++ d ++ "\"") date ++
  "\n},\n\"body\": "++ contentToJson (Paragraph contents) ++ "\n" ++
  "}"

-- | This function handles the writing of JSON contents
contentToJson :: Content -> String

contentToJson (Text str) = "\"" ++ str ++ "\""

contentToJson (Italic content) =
  "{\n\"italic\": " ++ contentToJson content ++ "\n}"

contentToJson (Bold content) =
  "{\n\"bold\": " ++ contentToJson content ++ "\n}"

contentToJson (Code str) =
  "{\n\"code\": \"" ++ str ++ "\"\n}"

contentToJson (Link url content) =
  "{\n\"link\": {\n\"url\": \"" ++ url ++ "\",\n\"content\": " ++
  contentToJson content ++ "\n}\n}"

contentToJson (Image url alt) =
  "{\n\"image\": {\n\"url\": \"" ++ url ++ "\",\n\"alt\": " ++
  contentToJson alt ++ "\n}\n}"

contentToJson (Paragraph contents) =
  "[\n" ++ (
    case contents of
      [] -> ""
      [c] -> contentToJson c
      _ -> concatMap (\c -> contentToJson c ++ ",\n") (init contents) ++
        contentToJson (last contents)
    ) ++ "\n]"

contentToJson (Section title contents) =
  "{\n\"section\": {\n" ++
  maybe "" (\t -> "\"title\": \"" ++ t ++ "\",\n") title ++
  "\"content\": " ++ contentToJson (Paragraph contents) ++ "\n}\n}"

contentToJson (CodeBlock contents) =
  "{\n\"codeblock\": " ++ contentToJson (Paragraph contents) ++ "\n}"

contentToJson (List items) =
  "{\n\"list\": [\n" ++ (
    case items of
      [] -> ""
      [c] -> contentToJson c
      _ -> concatMap (\c -> contentToJson c ++ ",\n") (init items) ++
        contentToJson (last items)
    ) ++ "\n]\n}"

contentToJson Empty = ""

-- | This function is the entry point for writing a JSON document
writingJson:: Writing
writingJson doc = Right $ writeJsonContents doc



-- | This function handles the writing of a Markdown document
writingMarkdown:: Writing
writingMarkdown doc = Right $ documentToMd doc

documentToMd :: Document -> String
documentToMd (Document (Header title author date) contents) =
  "---\n" ++ "title: " ++ title ++ maybe "" ("\nauthor: " ++) author ++
  maybe "" ("\ndate: " ++) date ++ "\n---\n\n" ++
  concatMap (\c -> contentToMd c 1 True) contents

displaySection :: String -> Int -> String
displaySection "" _ = ""
displaySection str n =
  let prefix = replicate n '#'
  in prefix ++ " " ++ str ++ "\n\n"

contentToMd :: Content -> Int -> Bool -> String
contentToMd Empty _ _ = ""

contentToMd (Text str) _ _ = str

contentToMd (Italic content) n b =
  "*" ++ contentToMd content n b ++ "*"

contentToMd (Bold content) n b =
  "**" ++ contentToMd content n b ++ "**"

contentToMd (Code str) _ _ =
  "`" ++ str ++ "`"

contentToMd (Link url content) n _ =
  "[" ++ contentToMd content n False ++ "](" ++ url ++ ")"

contentToMd (Image url alt) n _ =
  "![" ++ contentToMd alt n False ++ "](" ++ url ++ ")"

contentToMd (Paragraph contents) n b =
  concatMap (\c -> contentToMd c n b) contents ++ if b then "\n\n" else ""

contentToMd (Section title contents) n b =
  displaySection (maybe "" id title) n ++
  concatMap (\c -> contentToMd c (n+1) b) contents

contentToMd (CodeBlock contents) n b =
  "```\n" ++
  concatMap (\c -> contentToMd c n b) contents ++
  "\n```\n"

contentToMd (List items) n b =
  concatMap (\i -> "- " ++ contentToMd i n False ++ "\n") items ++ "\n"
