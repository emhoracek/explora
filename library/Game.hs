module Game where 

import DIYGraph
import Parse
import Dictionary
import Graph
import Places
import Player

data Game = Game { player :: Player,
                   mapGraph :: Graph Place String,
                   dictionary :: Dictionary } deriving (Eq, Show)
makeGame :: [Place] -> Dictionary -> Game
makeGame p d = let graph = createGraph p in
               Game (makePlayer graph) graph d

