module Graph where

import Control.Applicative
import Data.List (find)
import Places
import DIYGraph


nodeFromPlace :: Place -> Node Place
nodeFromPlace place = (num place, place)

edgesFromPlace :: Place -> [Edge Direction]
edgesFromPlace place = map (\x -> (num place, node x, direction x)) (exits place)

createGraph :: [ Place ] -> Graph Place Direction
createGraph places = fromLists (map nodeFromPlace places)
                               (concatMap edgesFromPlace places)

maybeFindNode :: Direction -> [Edge Direction] -> Maybe NodeID
maybeFindNode direction edges = 
  let sameDirection x (_, _, y) = x == y 
      newNode (_, x, _) = x in
  newNode <$> find (sameDirection direction) edges

-- This takes a node and direction and the graph, and tries to follow 
-- the direction to another node in the graph.
findNodeByDirection :: NodeID -> Direction -> Graph Place Direction -> Maybe NodeID
findNodeByDirection node direction graph = maybeFindNode direction $ out graph node

