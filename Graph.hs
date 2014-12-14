module Graph where

import Data.Graph.Inductive
import Data.Char (toLower)
import Data.Map (Map, fromList, findWithDefault)
import Data.Maybe (fromMaybe, listToMaybe)
import Text.ParserCombinators.Parsec
import Data.Set (Set, toList)
import qualified Data.Set as Set
import Parse
import Places
import Dictionary (Direction)

nodeFromPlace :: Place -> LNode Place
nodeFromPlace place = (num place, place)

edgesFromPlace :: Place -> [LEdge Direction]
edgesFromPlace place = map (\x -> (num place, node x, direction x)) (Set.toList $ exits place)

createGraph :: [ Place ] -> Gr Place Direction
createGraph places = mkGraph (map nodeFromPlace places)
                             (concatMap edgesFromPlace places)

edgesFromNode :: Node -> Gr Place Direction -> [ LEdge Direction ] 
edgesFromNode node graph = out graph node

maybeMatchEdge :: Direction -> [LEdge Direction] -> Maybe (LEdge Direction)
maybeMatchEdge direction edges = 
  let sameDirection x (_, _, y) = x == y in
  listToMaybe $ filter (sameDirection direction) edges 

findNodeByDirection :: Node -> Direction -> Gr Place Direction -> Node
findNodeByDirection oldNode direction graph = 
  let newNode (_, x, _) = x in
  maybe oldNode newNode (maybeMatchEdge direction $ edgesFromNode oldNode graph)

{--
-- "out" gives a list of edges that go out from a node. This folds a function

-- over the the list that looks at each edge and sees if it is going in the
-- then that exit's node is the new node. Otherwise, it's the node stays the same.
tryExits :: Direction -> Node -> Gr Place String -> Node
tryExits direction node graph = foldr (tryEdge exit) node $ out graph node

tryEdge :: String -> LEdge String -> Node -> Node
tryEdge exit (_, newNode, label) oldNode | label == exit = newNode
                                         | otherwise     = oldNode

createGraph :: Place -> Gr Place Direction
createGraph = undefined

initGraph file = do
    f <- readFile file 
    let p = parsePlaces f
    let makeGraph l = placeGraph (placesToNodes l) (concat $ listAllExits l)
    let pl = either (errorPlaces . show) id p
    let grph = makeGraph pl
    return grph

--}
