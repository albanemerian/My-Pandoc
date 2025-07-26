{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- Parser
-}

module Parser where

import Document (Format(..), MyPandocArgs(..))
import Data.List (find, isPrefixOf)
import Data.Char (toLower)


-- | This function parses the output format from a string
parseOutputFormat :: String -> Either String Format
parseOutputFormat str = case map toLower str of
    "xml"      -> Right XML
    "json"     -> Right JSON
    "markdown" -> Right Markdown
    _          -> Left $ "Invalid output format: " ++ str


parseInFormat :: String -> Either String Format
parseInFormat = parseOutputFormat


-- | This function checks if the flag is present in the arguments
getFlagValue :: String -> [String] -> String -> Either String String
getFlagValue flag args errMsg =
    case findFlagValue flag args of
        Just value -> Right value
        Nothing    -> Left errMsg

-- | This function checks if the flag is present in the arguments
findFlagValue :: String -> [String] -> Maybe String
findFlagValue flag args = case dropWhile (/= flag) args of
    (_:value:_) -> Just value
    _           -> Nothing

-- | Parse command line arguments and return a MyPandocArgs structure
parseArgs :: [String] -> Either String MyPandocArgs
parseArgs args = do
    validateFlags args
    inFile <- getFlagValue "-i" args "Missing flag: -i (input file)"
    outputFormatStr <- getFlagValue "-f" args "Missing flag: -f (output forma)"
    outputFormat <- parseOutputFormat outputFormatStr
    outFile <- validateOptionalFlag "-o" args
    inFormatSt <- validateOptionalFlag "-e" args
    inFormat <- maybe (Right Nothing) (fmap Just . parseInFormat) inFormatSt
    return $  MyPandocArgs{ inputFile = inFile, fileContent = "",
    outputFormat = outputFormat,outputFile = outFile, inputFormat = inFormat}


-- | Validate the command line flags
validateFlags :: [String] -> Either String ()
validateFlags args =
    let validFlags = ["-i", "-f", "-e", "-o"]
        flags = filter (isPrefixOf "-") args
    in case filter (`notElem` validFlags) flags of
        [] -> Right ()
        invalid -> Left $ "Invalid flags: " ++ unwords invalid

-- | Validate the optional flag and return its value if present
validateOptionalFlag :: String -> [String] -> Either String (Maybe String)
validateOptionalFlag flag args =
    case dropWhile (/= flag) args of
        (_:value:rest) | not (null value) && not (isPrefixOf "-" value) ->
            Right (Just value)
        (_:[]) -> Left $ "Flag " ++ flag ++ " requires a value"
        (_:value:_) -> Left $ "Flag " ++ flag ++
            " requires a valid value, but got: " ++ value
        _ -> Right Nothing
