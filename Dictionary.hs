module Dictionary where

import Data.Char (toLower)
import Data.Map (Map, fromList, lookup)
import Places (Exit(..),Direction,Place(..))

type Dictionary = Data.Map.Map UserInput Direction
type UserInput = String

toDictionary :: [ Place ] -> Dictionary
toDictionary list = Data.Map.fromList $ 
                    concatMap exitToDefinitions $ concatMap exits list

-- this changes the tuple of a canonical direction and a bunch of synonyms
-- to a key, value pairing of a user input and direction
exitToDefinitions :: Exit -> [(UserInput, Direction)]
exitToDefinitions (Exit dir [] _) = 
    [ (Prelude.map toLower dir, dir) ]
exitToDefinitions (Exit dir (syn:syns) n) = 
    (syn, dir) : exit2Definitions (Exit dir syns n)

inputToDirection :: UserInput -> Dictionary -> Maybe Direction
inputToDirection = Data.Map.lookup

