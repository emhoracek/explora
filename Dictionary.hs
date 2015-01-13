module Dictionary where

import Data.Char (toLower)
import Text.ParserCombinators.Parsec
import Data.Map 
import qualified Data.Map as M
import Places (Exit(..),Direction(..),Place(..))

type Dictionary = M.Map UserInput Direction
type UserInput = String

toDictionary :: [ Place ] -> Dictionary
toDictionary list = M.fromList $ concatMap exit2Definitions $ concatMap exits list

-- this changes the tuple of a canonical direction and a bunch of synonyms
-- to a key, value pairing of a user input and direction
exit2Definitions :: Exit -> [(UserInput, Direction)]
exit2Definitions (Exit dir [] node) = [ (Prelude.map toLower dir, dir) ]
exit2Definitions (Exit dir (syn:syns) node) = (syn, dir) : exit2Definitions (Exit dir syns node)

inputToDirection :: UserInput -> Dictionary -> Maybe Direction
inputToDirection = M.lookup

defaultDictionary :: Dictionary 
defaultDictionary = M.fromList  [("n", "North"),("s", "South"),("e", "East"), ("w", "West"),
                      ("north", "North"),("south", "South"),("east", "East"), ("west", "West"),
                      ("d", "Down"), ("down", "Down"), ("u", "Up"), ("up", "Up"),
                      ("xyzzy", "xyzzy"), ("jump", "jump")]
