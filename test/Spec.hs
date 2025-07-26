{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-NAN-4-1-mypandoc-albane.merian
-- File description:
-- Spec
-}

module Main (main) where

import Parsetest (testParsing)
import DocumentTest (testDocument)
import Xmltest (testXml)
import JsonTest (testJson)
import MdTest (testMd)
import Test.Hspec


main :: IO ()
main = hspec $ do
    testParsing
    testDocument
    testXml
    testJson
    testMd
