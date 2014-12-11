{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

import Places

-- I got most of this parsec stuff from Real World Haskell
-- Then I warped it to my own purposes in probably not the best fashion.

placeFile = many1 line
dictionaryFile = many1 lineDictionary

line = do
  char '{'
  rawNum  <- many digit
  string ". "
  rawName <- placeString
  char '\n'
  skipMany space
  rawDesc <- placeString
  char '\n'
  skipMany space
  char '['
  rawExits <- listexits
  string "]}\n"
  return $ Place (read rawNum) rawName rawDesc rawExits

lineDictionary = do
  mainDirection <- dirString
  char ':'
  skipMany space
  directions <- listSynonyms mainDirection
  return $ makeDictionary mainDirection directions 

-- The list of synonyms is either given by the user with spaces between, or it's-- just the same as the main direction.
listSynonyms main = option [ main ] $ sepBy dirString (string ", ")

-- Turns the main direction plus synonyms into a list of tuples of 
-- (user input, directions)
makeDictionary :: String -> [String] -> [(String, String)]
makeDictionary word = 
     map (\x -> (x, word)) 

placeString = many (noneOf "\n{}\\")
dirString = many (noneOf "\n.\\:")

listexits = option [NoExit] $ sepBy getExits (string ", ")

getExits = do
   skipMany space
   ddir <- many (noneOf "[],:()")
   char ':'
   skipMany space
   dnode <- many digit
   let dexit = Exit ddir (read dnode)
   return dexit

parseDSL :: String -> Either ParseError [Place]
parseDSL = parse placeFile "(unknown)"

parseDictionary :: String -> Either ParseError [[(String, String)]]
parseDictionary = parse dictionaryFile "(unknown)"

dslFileName = "places.exp"
