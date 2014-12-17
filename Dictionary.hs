module Dictionary where

import Data.Char (toLower)
import Text.ParserCombinators.Parsec
import Data.Map 
import qualified Data.Map as M

type Dictionary = M.Map UserInput Direction
type UserInput = String
type Direction = String

toDictionary :: [ (Direction, [UserInput]) ] -> Dictionary
toDictionary list = M.fromList $ concatMap toDefinition list

toDefinition :: (Direction, [UserInput]) -> [(UserInput, Direction)]
toDefinition (x, []) = [ (Prelude.map toLower x, x) ]
toDefinition (x, y:ys) = (y, x) : toDefinition (x, ys)

inputToDirection :: UserInput -> Dictionary -> Maybe Direction
inputToDirection = M.lookup

defaultDictionary :: Dictionary 
defaultDictionary = M.fromList  [("n", "North"),("s", "South"),("e", "East"), ("w", "West"),
                      ("north", "North"),("south", "South"),("east", "East"), ("west", "West"),
                      ("d", "Down"), ("down", "Down"), ("u", "Up"), ("up", "Up"),
                      ("xyzzy", "xyzzy"), ("jump", "jump")]
