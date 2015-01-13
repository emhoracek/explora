{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse where

import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec as P
import Data.Map (Map, fromList)
import Places
import Dictionary

listOfDefinitions :: Exit -> [(Direction, [UserInput])]
listOfDefinitions (Exit dir syn _) = [(dir, syn)]

listOfPlaces :: Parser [Place]
listOfPlaces = many parsePlace

parsePlace :: Parser Place
parsePlace = do
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
listOfExits = option [] $ try $ parseExit `sepBy` string ", "

parseSynonyms :: Parser [String]
parseSynonyms = do
    char '('
    synonyms <- validExitString `sepBy` string ", " 
    char ')'
    return synonyms

parseExit :: Parser Exit
parseExit = do
   optional $ string "->"
   skipMany space
   ddir <- validExitString
   skipMany space
   synonyms <- option [] parseSynonyms
   skipMany space
   char ':'
   skipMany space
   dnode <- many digit
   return $ Exit ddir synonyms (read dnode)

parseExits :: String -> Either ParseError [Exit]
parseExits = parse listOfExits "Exit error: "

parsePlaces :: String -> Either ParseError [Place]
parsePlaces = parse listOfPlaces "Map error: "

parseDictionary :: String -> Either ParseError Dictionary
parseDictionary file = case parsePlaces file of 
    Left x -> Left x
    Right x -> Right $ toDictionary x
