module DIYGraphSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import DIYGraph

{--
instance (Arbitrary a, Arbitrary b) => Arbitrary (Graph a b) where
    arbitrary = do
        context <- arbitrary
        graph   <- arbitrary
    return $ oneOf [EmptyGraph, (context :&: graph)]
--}

sampleGraph :: Graph String String 
sampleGraph = ([], (1, "hello"), []) :&: EmptyGraph

sampleGraph2 :: Graph String ()
sampleGraph2 = ([], (1, "hello"), []) :&: EmptyGraph

sampleGraphInt :: Graph String Int
sampleGraphInt = ([(2, 1)], (1, "hello"), [(3, 1)]) :&: EmptyGraph 

longerGraph :: Graph String String
longerGraph = ([("mother", 2)], (1, "Marilyn"), [("mother of",2)]) :&:
              (([("daughter", 1)], (2, "Libby"), [("daughter of",1)]) :&: EmptyGraph)

spec =  do
    describe "fmap" $ do
        it "satisfies law 1" $
            fmap id sampleGraph `shouldBe` sampleGraph
        it "satisfies law 2" $
            fmap ((+2) . (* 4)) sampleGraphInt `shouldBe`
                (fmap (+2) . fmap (*4)) sampleGraphInt
   
    describe "singleton" $ 
        it "is a single node with no edges" $
            singleton (1, "hello") `shouldBe` sampleGraph2
    
    describe "insertNode" $ 
        it "into an empty graph, same as singleton" $ property $
            \x -> insertNode (x :: (Int, String)) EmptyGraph == singleton x

    describe "insertNodes" $ do
        context "empty list" $
            it "returns the same graph" $ 
               insertNodes [] sampleGraph `shouldBe` sampleGraph
        context "single node" $
            it "is the same as insertNode" $ property $
                \x -> insertNodes [x :: (Int, String)] sampleGraph
                        == insertNode x sampleGraph
        context "multiple nodes" $
            it "should add the node to the graph" $
                insertNodes [(3, "hello again"),
                             (2, "goodbye")] sampleGraph `shouldBe`
                    ([], (3, "hello again"), []) :&:
                    (([], (2, "goodbye"), []) :&: sampleGraph)

    describe "findNode" $ do
        context "node in graph" $
            it "says true" $
                findNode 1 sampleGraph `shouldBe` True
        context "not in graph" $
            it "says false" $
                findNode 32 sampleGraph `shouldBe` False

    describe "removeNode" $ do
        context "node in graph" $
            it "removes node and context from graph" $
                removeNode (1, "hello") sampleGraph `shouldBe`
                    EmptyGraph
        context "larger graph" $
            it "removes node and context from graph" $
                removeNode (2, "Libby") longerGraph `shouldBe`
                    (([], (1, "Marilyn"), []) :&: EmptyGraph)
        context "node not in graph" $
            it "returns the same graph" $
                removeNode (12, "yo") sampleGraph `shouldBe`
                    sampleGraph
        context "empty graph" $
            it "returns an empty graph" $
                removeNode (12, "yo") EmptyGraph `shouldBe` (EmptyGraph :: Graph String Int) 

    describe "insertEdge" $ do
        context "empty graph" $
            it "returns an empty graph" $
                insertEdge (1,2,3) EmptyGraph `shouldBe` (EmptyGraph :: Graph Int Int)
        context "no matching node" $
            it "returns the same graph" $
                insertEdge (12,43,34) sampleGraphInt `shouldBe` 
                    sampleGraphInt
        context "nodes match" $ 
            it "add to the context of matching node" $
                insertEdge (1,1,"turn around") sampleGraph `shouldBe`
                    ([("turn around", 1)], (1, "hello"), [("turn around", 1)])
                        :&: EmptyGraph

    describe "fromLists" $
        it "turns lists of nodes and lists of edges into a graph" $
            fromLists [(1, "hello"), (2, "goodbye")] 
                      [(1, 2, "exit")] `shouldBe`
            (([], (1, "hello"), [("exit", 2)]) :&:
             (([("exit", 1)], (2, "goodbye"), []) :&: EmptyGraph))

    describe "linksToEdge" $
        it "turns a list of links to a list of edges" $
            linksToEdges 1 longerGraph `shouldBe`
                [(1, 2, "mother of")]

    describe "nodeToContect" $ do
        context "existing node" $
            it "returns the context" $
                nodeToContext 2 longerGraph `shouldBe`
                  ([("daughter", 1)], (2, "Libby"), [("daughter of",1)])
        context "nonexistent node" $
            it "????" $
                pendingWith "This is going to be a problem" 
