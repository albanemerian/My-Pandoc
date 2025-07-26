{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- Main
-}

module Main where
import Parser (parseArgs)
import Functor(tryParsers, tryWriting)
import Document (MyPandocArgs(..), Document(..))
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (catch, IOException)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> handleParseError err
        Right parsedArgs -> processInputFile parsedArgs

handleParseError :: String -> IO ()
handleParseError err = putStrLn ("Error: " ++ err) >>
    exitWith (ExitFailure 84)


processInputFile :: MyPandocArgs -> IO ()
processInputFile parsedArgs = do
    inputContent <- catch (readFile (inputFile parsedArgs)) handleFileError
    let updatedArgs = parsedArgs { fileContent = inputContent }
    handleParsing updatedArgs

handleFileError :: IOException -> IO a
handleFileError _ = exitWithError "Error: Input file"

handleParsing :: MyPandocArgs -> IO ()
handleParsing updatedArgs = case tryParsers updatedArgs of
    Left parseErr -> exitWithError ("Parsing failed: " ++ parseErr)
    Right doc -> handleWriting doc updatedArgs

handleWriting :: Document -> MyPandocArgs -> IO ()
handleWriting doc uptArg = case tryWriting doc uptArg of
    Left writeErr -> exitWithError ("Writing failed: " ++ writeErr)
    Right out -> maybe (putStrLn out) (`writeFile` out) (outputFile uptArg)

exitWithError :: String -> IO a
exitWithError errMsg = putStrLn errMsg >> exitWith (ExitFailure 84)
