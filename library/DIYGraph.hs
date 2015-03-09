-- | Really simple directed inductive graph that does only what the game 
-- engine needs (representing a map). Based on Erwig's inductive graph.
module DIYGraph where

-- | A unique ID for each node 
type NodeID = Int
-- | A node is a node ID and a "label" 
type Node a = (NodeID, a)
-- | A labeled edge
-- Here the "label" is called "b" to show it can be a different type from 
-- whatever the "labels" for the nodes.
type Edge b = (NodeID, NodeID, b)
-- | Links are a list of labels and node IDs
-- A node can have a links going to the node and going out from the node. 
-- This list represents th
type Links b = [(b, NodeID)]

-- | A context is the links to the node, the node, links from the node
type Context a b = (Links b, Node a, Links b)

-- | A graph is either empty or a context and a graph. (:&:) is kinda like 
-- (:) for lists. 
data Graph a b = EmptyGraph | (:&:) (Context a b) (Graph a b) deriving Eq
-- The output of "show" is pretty ugly and could be improved.
instance (Show a, Show b) => Show (Graph a b) where
    show (context :&: graph) = show context ++ " & " ++
                               show graph
    show EmptyGraph = "EmptyGraph"
-- I only added this to learn about functors and it kinda sucks???
instance Functor (Graph blah) where
    fmap _ EmptyGraph = EmptyGraph
    fmap f ((linksA, node, linksB) :&: graph) = 
        (mapLinks linksA, node, mapLinks linksB) :&: fmap f graph 
        where mapLinks = map (\(a, b) -> (f a, b)) 

-- | A graph with only one node.
singleton :: Node a -> Graph a ()
singleton node = ([], node, []) :&: EmptyGraph

-- | Inserts a single node into a graph.
insertNode :: Node a -> Graph a b -> Graph a b
insertNode node graph = ([], node, []) :&: graph

-- | Inserts a list of nodes into a graph.
insertNodes :: [Node a] -> Graph a b -> Graph a b
insertNodes [] graph = graph
insertNodes [node] graph = insertNode node graph
insertNodes (x:xs) graph = insertNode x (insertNodes xs graph)

-- | Returns True if the node is in the graph.
findNode :: NodeID -> Graph a b -> Bool
findNode _ EmptyGraph = False
findNode x ((_, (n, _), _) :&: graph) 
    | x == n    = True
    | otherwise = findNode x graph 

-- | Removes a node from the graph, if it is in the graph, or returns the same
-- graph if it's not in the graph.
removeNode :: (Eq a) => Node a -> Graph a b -> Graph a b
removeNode _ EmptyGraph = EmptyGraph
removeNode x graph@(context@(linksIn, node, linksOut) :&: g) 
    | x == node  = removeNode x g
    | otherwise  = (removeLinks linksIn, node, removeLinks linksOut) 
                        :&: removeNode x g
    where noMatchID link = fst x /= snd link
          removeLinks = filter noMatchID

-- | Given a node and graph, will find the node with a matching ID and replace
-- its label with the label of the given node, returning a new graph. If no 
-- matching node is in the graph, returns an identical graph.
changeNode :: (Eq a) => (NodeID, a)  -- ^ ID of the node that needs to change
                                     -- and what to change it to.
                        -> Graph a b -- ^ the current graph
                        -> Graph a b -- ^ the new graph
changeNode _ EmptyGraph = EmptyGraph
changeNode x graph@(context@(linksIn, node, linksOut) :&: g) 
    | fst x == fst node = (linksIn, x, linksOut) :&: g
    | otherwise = context :&: changeNode x g 

-- | Adds a new labeled edge to a node's context, but only if the edge is
-- connected to that node.
addEdgeToContext :: Edge b -> Context a b -> Context a b 
addEdgeToContext (n1, n2, label) (linksTo, node, linksFrom)
    -- If an edge goes from the node back to itself, then it belongs in both 
    -- the "to" and "from" lists
    | n1 == fst node && n2 == fst node = 
        ((label, n1) : linksTo, node, (label, n2) : linksFrom)
    -- The edge goes from this node to another 
    | n1 == fst node = (linksTo, node, (label, n2) : linksFrom)
    -- The edge goes from another node to this one
    | n2 == fst node = ((label, n1) : linksTo, node, linksFrom)
    -- This edge is not connected to this node, so context stays the same
    | otherwise = (linksTo, node, linksFrom)

-- | Attempts to add an edge to every context in the graph.
insertEdge :: Edge b -> Graph a b -> Graph a b
insertEdge _ EmptyGraph = EmptyGraph
insertEdge edge (context :&: g) =
    addEdgeToContext edge context :&: insertEdge edge g

-- | Adds a list of edges to the graph.
insertEdges :: [Edge b] -> Graph a b -> Graph a b
insertEdges edges graph = foldl (flip insertEdge) graph edges 

-- | Turns a list of nodes and a list of edges into a graph.
fromLists :: [Node a] -> [Edge b] -> Graph a b
fromLists nodes edges = insertEdges edges $ insertNodes nodes EmptyGraph

-- | Tries to find the context of a node. It should really be a Maybe.
nodeToContext :: NodeID -> Graph a b -> Context a b
nodeToContext _ EmptyGraph = error "Node not in graph!!!"
nodeToContext node (context@(_, n,_) :&: graph)
    | node == fst n = context
    | otherwise     = nodeToContext node graph

-- | Shorthand to get links from a context.
outLinks :: Context a b-> Links b
outLinks (_, _, x) = x

-- What was my motivation for this??
linksToEdges :: NodeID -> Graph a b -> [Edge b]
linksToEdges n graph = map (\(label, node) -> (n, node, label)) 
                        (outLinks $ nodeToContext n graph)

-- Seems to be same as the above but in what feels like the wrong order?
out :: Graph a b -> NodeID -> [Edge b]
out graph n = linksToEdges n graph

-- | Lists the IDs of every node in the graph.
nodes :: Graph a b -> [NodeID]
nodes EmptyGraph = []
nodes ((_, (id, _),_) :&: graph) = id : nodes graph

-- | Gets the label of a node in a graph by its ID.
-- Why graph then node?
label :: Graph a b -> NodeID -> a
label graph n = label context
    where context = nodeToContext n graph
          label (_, (_, x),_) = x 
