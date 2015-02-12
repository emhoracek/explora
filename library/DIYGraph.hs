module DIYGraph where

import Control.Arrow (first)

-- copying fgl

type NodeID = Int
type Node a = (NodeID, a)
type Edge b = (NodeID, NodeID, b)
type Links b = [(b, NodeID)]

-- a context is the links to the node, the node, links from the node
type Context a b = (Links b, Node a, Links b)

-- a graph is either empty or a context and a graph
data Graph a b = EmptyGraph | (:&:) (Context a b) (Graph a b) deriving Eq


instance (Show a, Show b) => Show (Graph a b) where
    show (context :&: graph) = show context ++ " & " ++
                               show graph
    show EmptyGraph = "EmptyGraph"
instance Functor (Graph blah) where
    fmap _ EmptyGraph = EmptyGraph
    fmap f ((linksA, node, linksB) :&: graph) = 
        (mapLinks linksA, node, mapLinks linksB) :&: fmap f graph 
        where mapLinks = map (first f) 
-- I only added this to learn about functors and I think it kinda sucks???

singleton :: Node a -> Graph a ()
singleton node = ([], node, []) :&: EmptyGraph

insertNode :: Node a -> Graph a b -> Graph a b
insertNode node graph = ([], node, []) :&: graph

insertNodes :: [Node a] -> Graph a b -> Graph a b
insertNodes [] graph = graph
insertNodes [node] graph = insertNode node graph
insertNodes (x:xs) graph = insertNode x (insertNodes xs graph)

findNode :: NodeID -> Graph a b -> Bool
findNode _ EmptyGraph = False
findNode x ((_, (n, _), _) :&: graph) 
    | x == n    = True
    | otherwise = findNode x graph 

removeNode :: (Eq a) => Node a -> Graph a b -> Graph a b
removeNode _ EmptyGraph = EmptyGraph
removeNode x graph@(context@(linksIn, node, linksOut) :&: g) 
    | x == node  = removeNode x g
    | otherwise  = (removeLinks linksIn, node, removeLinks linksOut) 
                        :&: removeNode x g
    where noMatchID link = fst x /= snd link
          removeLinks = filter noMatchID

-- This takes a node and a graph and returns a new graph with
-- that node in the place of whatever node had the same ID
changeNode :: (Eq a) => Node a -> Graph a b -> Graph a b
changeNode _ EmptyGraph = EmptyGraph
changeNode x graph@(context@(linksIn, node, linksOut) :&: g) 
    | fst x == fst node = (linksIn, x, linksOut) :&: g
    | otherwise = context :&: changeNode x g 


insertEdge :: Edge b -> Graph a b -> Graph a b
insertEdge _ EmptyGraph = EmptyGraph
insertEdge edge (context :&: g) =
    addEdgeToContext edge context :&: insertEdge edge g

insertEdges :: [Edge b] -> Graph a b -> Graph a b
insertEdges edges graph = foldl (flip insertEdge) graph edges 

addEdgeToContext :: Edge b -> Context a b -> Context a b 
addEdgeToContext (n1, n2, label) (linksA, node, linksB)
    | n1 == fst node && n2 == fst node = 
        ((label, n1) : linksA, node, (label, n2) : linksB)
    | n1 == fst node = (linksA, node, (label, n2) : linksB)
    | n2 == fst node = ((label, n1) : linksA, node, linksB)
    | otherwise = (linksA, node, linksB)

fromLists :: [Node a] -> [Edge b] -> Graph a b
fromLists nodes edges = insertEdges edges $ insertNodes nodes EmptyGraph

-- This isn't good
nodeToContext :: NodeID -> Graph a b -> Context a b
nodeToContext _ EmptyGraph = error "Node not in graph!!!"
nodeToContext node (context@(_, n,_) :&: graph)
    | node == fst n = context
    | otherwise     = nodeToContext node graph

outLinks :: Context a b-> Links b
outLinks (_, _, x) = x

linksToEdges :: NodeID -> Graph a b -> [Edge b]
linksToEdges n graph = map (\(label, node) -> (n, node, label)) 
                        (outLinks $ nodeToContext n graph)

out :: Graph a b -> NodeID -> [Edge b]
out graph n = linksToEdges n graph

nodes :: Graph a b -> [NodeID]
nodes EmptyGraph = []
nodes ((_, (id, _),_) :&: graph) = id : nodes graph

label :: Graph a b -> NodeID -> a
label graph n = label context
    where context = nodeToContext n graph
          label (_, (_, x),_) = x 
