module Graph where

import Data.Graph.Inductive
import Data.Char (toLower)
import Data.Map (Map, fromList, findWithDefault)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

import Parse
import Places

placesToNodes :: [Place] -> [(Int, Place)]
placesToNodes = map (\x -> (num x, x))

-- We want to end up with a list of all the exits for all the places, in edge format
listAllExits :: [Place] -> [[(Int, Int, String)]]
listAllExits = map listExits

listExits :: Place -> [(Int, Int, String)]
listExits x = map ((\ n x -> (n, node x, dir x)) (num x)) (exits x)

-- This turns the lists of nodes and list of edges into an inductive graph.
placeGraph :: [(Int, Place)] -> [(Int, Int, String)] -> Gr Place String
placeGraph = mkGraph

-- "out" gives a list of edges that go out from a node. This folds a function

-- over the the list that looks at each edge and sees if it is going in the
-- same direction as the player wanted. If it's going in the right direction,
-- then that exit's node is the new node. Otherwise, it's the node stays the same.
tryExits :: String -> Node -> Gr Place String -> Node
tryExits exit node graph = foldr (tryEdge exit) node $ out graph node

tryEdge :: String -> LEdge String -> Node -> Node
tryEdge exit (_, newNode, label) oldNode | label == exit = newNode
                                         | otherwise     = oldNode

initGraph file = do
    f <- readFile file 
    let p = parsePlaces f
    let makeGraph l = placeGraph (placesToNodes l) (concat $ listAllExits l)
    let pl = either defaultPlaces id p
    let grph = makeGraph pl
    return grph
