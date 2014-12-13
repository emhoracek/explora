{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec
import Data.Map (Map, fromList)
import Places
import Dictionary

-- I got most of this parsec stuff from Real World Haskell
-- Then I warped it to my own purposes in probably not the best fashion.
-- It looks really ugly. How can I make it better?

placeFile :: Parser [Place]
placeFile = placeLine `sepBy` newline

placeLine :: Parser Place
placeLine = do
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
  rawExits <- listOfExits
  string "]}"
  return $ Place (read rawNum) rawName rawDesc rawExits

dictionaryFile :: Parser [[(String, Direction)]]
dictionaryFile =  dictionaryLine `sepBy` newline 

dictionaryLine :: Parser [(String, Direction)] 
dictionaryLine = do
  direction <- directionString
  string ": "
  synonyms <- listOfSynonyms direction
  return $ makeDefinitions direction synonyms

-- The list of synonyms is either given by the user with spaces between, or it's-- just the same as the main direction.
listOfSynonyms :: Direction -> Parser [String]
listOfSynonyms main = option [ main ] $ sepBy directionString (string ", ")

-- Turns the main direction plus synonyms into a list of tuples of 
-- (user input, directions)
makeDefinitions :: Direction -> [String] -> [(String, Direction)]
makeDefinitions direction = map (\word -> (word, direction)) 

placeString = many (noneOf "\n{}\\")
directionString = many (noneOf "\n.\\:")

listOfExits :: Parser [Exit]
listOfExits = option [NoExit] $ sepBy getExits (string ", ")

getExits :: Parser Exit
getExits = do
   skipMany space
   ddir <- many (noneOf "[],:()")
   char ':'
   skipMany space
   dnode <- many digit
   return $ Exit ddir (read dnode) 

parsePlaces :: String -> Either ParseError [Place]
parsePlaces = parse placeFile "(unknown)"

parseDictionary :: String -> Either ParseError [[(String, Direction)]]
parseDictionary = parse dictionaryFile "(unknown)"
