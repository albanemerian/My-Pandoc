{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- Tester
-}


-- Pour compiller : ghc Tester.hs -o docparser -package text -package bytestring -package aeson -package xml-conduit -package blaze-html -package cmark

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson as Aeson
import Text.XML (parseLBS, def)
import Text.XML.Cursor (fromDocument, child, content, ($//))
import CMark (commonmarkToHtml, optSafe)
import qualified Data.Text as T
import Text.Blaze.Html.Renderer.Text (renderHtml)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [format, file] -> do
      content <- readFile file
      case format of
        "json"     -> parseJSON (BL.pack content)
        "xml"      -> parseXML (BL.pack content)
        "markdown" -> parseMarkdown (T.pack content)
        _          -> putStrLn "Unsupported format. Use: json, xml, or markdown."
    _ -> putStrLn "Usage: docparser <format> <file>"

parseJSON :: BL.ByteString -> IO ()
parseJSON bs =
  case Aeson.decode bs :: Maybe Aeson.Value of
    Just val -> putStrLn ("-- Parsed JSON to custom format:\n" ++ show val)
    Nothing  -> putStrLn "Invalid JSON."


parseXML :: BL.ByteString -> IO ()
parseXML bs =
  let
    -- Convert to Text for manipulation
    rawText :: T.Text
    rawText = T.pack (BL.unpack bs)

    -- Sanitize: escape unescaped '&' characters
    fixedText :: T.Text
    fixedText =
      T.replace (T.pack "&TEMP;") (T.pack "&amp;") $
      T.replace (T.pack "&") (T.pack "&amp;") $
      T.replace (T.pack "&amp;") (T.pack "&TEMP;") rawText

    fixedBs :: BL.ByteString
    fixedBs = BL.pack (T.unpack fixedText)
  in
  case parseLBS def fixedBs of
    Left err -> putStrLn $ "Error parsing XML: " ++ show err
    Right doc -> do
      let cursor = fromDocument doc
          cleanOutput = map (T.strip . T.replace (T.pack "\t") (T.pack "    ")) (concatMap content (cursor $// child))
      putStrLn "-- Parsed XML to custom format:"
      mapM_ T.putStrLn cleanOutput


parseMarkdown :: T.Text -> IO ()
parseMarkdown txt = do
  let html = commonmarkToHtml [optSafe] txt
  putStrLn "-- Parsed Markdown to custom format:"
  print html
