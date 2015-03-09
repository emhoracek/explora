module Dictionary where

import Data.Char (toLower)
import Data.Map (Map, fromList, lookup)
import Places (Exit(..),Direction,Place(..))

-- | A Dictionary is map from strings to directions.
type Dictionary = Data.Map.Map UserInput Direction
-- This might not really be necessary.
type UserInput = String

-- | Turns a list of places into a Dictionary mapping synonyms to their 
-- exit directions.
toDictionary :: [ Place ] -> Dictionary
toDictionary list = Data.Map.fromList $ 
                    concatMap exitToDefinitions $ concatMap exits list

-- | Changes the tuple of a canonical direction and a bunch of synonyms
-- to a key, value pairing of a user input and direction
exitToDefinitions :: Exit -> [(UserInput, Direction)]
exitToDefinitions (Exit dir [] _) = 
    [ (Prelude.map toLower dir, dir) ]
exitToDefinitions (Exit dir (syn:syns) n) = 
    (syn, dir) : exitToDefinitions (Exit dir syns n)

-- | Maybe finds the direction that a user means.
inputToDirection :: UserInput -> Dictionary -> Maybe Direction
inputToDirection = Data.Map.lookup

