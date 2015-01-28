module Game where 

import DIYGraph
import Parse
import Dictionary
import Graph
import Places

data Game = Game { player :: Player,
                   mapGraph :: Graph Place String,
                   dictionary :: Dictionary } deriving (Eq, Show)

data PlayerState = Alive | Dead | Won | Lost deriving (Eq, Show)

data Player = Player { currentPlace:: NodeID,
                       playerInventory:: [String],
                       score :: Integer,
                       playerState :: PlayerState } deriving (Eq, Show)

-- Player starts with no inventory at first place in graph.
makePlayer :: Graph Place Direction -> Player
makePlayer graph = Player { currentPlace = head $ nodes graph,
                            playerInventory = [],
                            score = 0,
                            playerState = Alive }

makeGame :: [Place] -> Dictionary -> Game
makeGame p d = let graph = createGraph p in
               Game (makePlayer graph) graph d

killPlayer :: Game -> Game
killPlayer (Game (Player n i s a) g d) = Game (Player n i s Dead) g d

changeScore :: (Integer -> Integer) -> Game -> Game
changeScore f (Game (Player n i s a) g d) = Game (Player n i (f s) a ) g d

isAlive :: Game -> Bool
isAlive (Game (Player n i s a) g d) = a == Alive

isWon :: Game -> Bool
isWon (Game (Player n i s a) g d) = a == Won

movePlayer :: Game -> Int -> Game
movePlayer (Game (Player n i s a) g d) node = Game (Player node i s a) g d
