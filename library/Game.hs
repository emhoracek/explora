module Game where 

import DIYGraph
import Parse
import Dictionary
import Graph
import Places
import Player

import Text.ParserCombinators.Parsec.Error

data Game = Game { player :: Player,
                   mapGraph :: Graph Place String,
                   dictionary :: Dictionary } deriving (Eq, Show)

initGame :: String -> Either String Game
initGame file =    
    let p = parsePlaces file
        d = parseDictionary file in
    parseGame p d

-- is this the best way to "add" these Eithers?
-- If both are Right, make the game. If the place file was file, but the
-- dictionary wasn't, show the error for the dictionary. Otherwise, show 
-- the error for the dictionary.
parseGame :: Either ParseError [Place] -> Either ParseError Dictionary ->
                Either String Game
parseGame (Right p) (Right d) = Right $ makeGame p d
parseGame (Right _) (Left  d) = Left $ show d
parseGame (Left  p)  _        = Left $ show p

makeGame :: [Place] -> Dictionary -> Game
makeGame p d = let graph = createGraph p in
               Game (makePlayer graph) graph d
