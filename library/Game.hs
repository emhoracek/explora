module Game where 

import DIYGraph
import Parse
import Dictionary
import Graph
import Places
import Player

import Text.ParserCombinators.Parsec.Error

-- TODO -- change mapGraph to places, game to gameState
data Game = Game { player :: Player,
                   mapGraph :: Graph Place String,
                   dictionary :: Dictionary } deriving (Eq, Show)

-- | Turns a game file into either an error message from the parser or a game.
initGame :: String -> Either String Game
initGame file =    
    let p = parsePlaces file
        d = parseDictionary file in
    parseGame p d

-- TODO: is this the best way to "add" these Eithers?
-- | Attemps to parse the game file into a Game. If both the list of Places 
-- and the Dictionary parse, then make the game. If the Places were fine, but 
-- the dictionary wasn't, show the error for the dictionary. Otherwise, show 
-- the error for the dictionary. (Unfortunately, I'm not sure this works 
-- because I couldn't figure out how to test.)
parseGame :: Either ParseError [Place] -> Either ParseError Dictionary ->
                Either String Game
parseGame (Right p) (Right d) = Right $ makeGame p d
parseGame (Right _) (Left  d) = Left $ show d
parseGame (Left  p)  _        = Left $ show p

-- | Makes a game by creating a graph from the places and combining the 
-- Dictionary and graph into a Game.
makeGame :: [Place] -> Dictionary -> Game
makeGame p d = let graph = createGraph p in
               Game (makePlayer graph) graph d
