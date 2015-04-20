{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse where

import Data.Map (fromList)

import Text.ParserCombinators.Parsec
import Places
import Dictionary
import Items

-- | Turns an Exit into a list of definitions.
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
    items <- listOfItems
    action <- listOfActions
    char '\n'
    skipMany space
    exits <- listOfExits
    skipMany (char '\n')
    return $ Place (read num) name desc items action exits

validPlaceString :: Parser String
validPlaceString = many (noneOf "\n*")

listOfActions :: Parser String
listOfActions = option "" $ try parseAction 

parseAction :: Parser String
parseAction = do
    char '\n'
    skipMany space
    char '*'
    skipMany space
    string <- validPlaceString
    return string

parseItem :: Parser [Item]
parseItem = do
    char '\n'
    skipMany space
    char '#'
    skipMany space
    string <- validExitString
    skipMany space
    char ':'
    skipMany space
    desc <- validPlaceString
    return [Item { itemName = string,
                   itemInfo = fromList [("description", desc)],
                   itemActions = [[]] }]

listOfItems :: Parser [Item]
listOfItems = option [] $ try parseItem

-- TODO: This doesn't work with multiword synonyms? are those okay?
validExitString :: Parser String
validExitString = many (noneOf " ,:()\n")

parseSynonyms :: Parser [String]
parseSynonyms = do
    char '('
    synonyms <- validExitString `sepBy` string ", " 
    char ')'
    return synonyms

listOfExits :: Parser [Exit]
listOfExits = option [] $ try $ parseExit `sepBy` string ", "

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

parseItemsTest :: String -> Either ParseError [Item]
parseItemsTest = parse listOfItems "Item error:"

parseDictionary :: String -> Either ParseError Dictionary
parseDictionary file = case parsePlaces file of 
    Left x -> Left x
    Right x -> Right $ toDictionary x
