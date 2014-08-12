{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

import Places

-- I got most of this parsec stuff from Real World Haskell
-- Then I warped it to my own purposes in probably not the best fashion.

placeFile = many1 line

line = do
  char '{'
  rawNum  <- many digit
  char '.'
  char ' '
  rawName <- placeString
  char '\n'
  skipMany space
  rawDesc <- placeString
  char '\n'
  skipMany space
  char '['
  rawExits <- listexits
  char ']'
  char '}'
  char '\n'
  return (Place (read rawNum) rawName rawDesc rawExits)

placeString = many (noneOf "\n{}\\")
listexits = option [NoExit] $ sepBy getExits (char ',')

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

dslFileName :: FilePath
dslFileName = "places.exp"

