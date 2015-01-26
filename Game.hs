module Game where 

import DIYGraph
import Parse
import Dictionary
import Graph
import Places


data World = World { currentPlace :: NodeID,
                     mapGraph :: Graph Place String } deriving (Eq, Show)

data Game = Game { world :: World,
                   dictionary :: Dictionary } deriving Show


