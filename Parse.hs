{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse where

import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec as P
import Data.Map (Map, fromList)
import Places
import Dictionary

{--

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
 --}

listOfPlaces :: Parser [Place]
listOfPlaces = many parsePlace

parsePlace :: Parser Place
parsePlace = do
    skipMany space
    num <- many digit
    char '.'
    skipMany space
    name <- validPlaceString 
    char '\n'
    skipMany space
    desc <- validPlaceString
    char '\n'
    exits <- listOfExits
    skipMany (char '\n')
    return $ Place (read num) name desc exits

validPlaceString = many (noneOf "\n")

-- what about multiword synonyms? are those okay?
validExitString = many (noneOf " ,:()\n")

listOfExits :: Parser [Exit]
listOfExits = option [] $ parseExit `sepBy` (string ", ")

parseSynonyms :: Parser [String]
parseSynonyms = do
    char '('
    synonyms <- validExitString `sepBy` (string ", ")
    char ')'
    return synonyms

parseExit :: Parser Exit
parseExit = do
   skipMany space
   string "->"
   skipMany space
   ddir <- validExitString
   skipMany space
   synonyms <- option [] $ parseSynonyms
   skipMany space
   char ':'
   skipMany space
   dnode <- many digit
   return $ Exit ddir synonyms (read dnode)

parseExits :: String -> Either ParseError [Exit]
parseExits = parse listOfExits "Exit error: "

parsePlaces :: String -> Either ParseError [Place]
parsePlaces = parse listOfPlaces "Map error: "

{--
parseDictionary :: String -> Either ParseError [[(String, Direction)]]
parseDictionary = parse dictionaryFile "Dictionary error: "
--}
