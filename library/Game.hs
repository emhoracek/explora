module Game where 

import DIYGraph
import Parse
import Dictionary
import Graph
import Places

data Game = Game { player :: Player,
                   mapGraph :: Graph Place String,
                   dictionary :: Dictionary } deriving (Eq, Show)

data Player = Player { currentPlace:: NodeID,
                       playerInventory:: [String],
                       score :: Integer,
                       alive :: Bool } deriving (Eq, Show)

-- Player starts with no inventory at first place in graph.
makePlayer :: Graph Place Direction -> Player
makePlayer graph = Player { currentPlace = head $ nodes graph,
                            playerInventory = [],
                            score = 0,
                            alive = True }

makeGame :: [Place] -> Dictionary -> Game
makeGame p d = let graph = createGraph p in
               Game (makePlayer graph) graph d
