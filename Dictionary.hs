module Dictionary where

import Data.Char (toLower)
import Data.Map (Map, fromList, findWithDefault)
import Text.ParserCombinators.Parsec 

type Dictionary = Data.Map.Map UserInput Direction
type UserInput = String
type Direction = String

findDirection :: UserInput -> Dictionary -> Direction
findDirection dir = findWithDefault "no match" (map toLower dir)

errorDictionary :: String -> Dictionary 
errorDictionary s =  fromList [("n", "North"),("s", "South"),("e", "East"), ("w", "West"),
                      ("north", "North"),("south", "South"),("east", "East"), ("west", "West"),
                      ("d", "Down"), ("down", "Down"), ("u", "Up"), ("up", "Up"),
                      ("xyzzy", "xyzzy"), ("jump", "jump"), ("error", s)]
