module Game where 

import DIYGraph
import Parse
import Dictionary
import Graph
import Places


data World = World { player :: Player,
                     mapGraph :: Graph Place String } deriving (Eq, Show)

data Game = Game { world :: World,
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
               Game (World (makePlayer graph) graph) d
