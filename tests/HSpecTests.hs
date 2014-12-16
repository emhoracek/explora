module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Graph.Inductive.Graph
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Places
import Graph
import Dictionary

samplePlaces :: [ Place ]
samplePlaces = [ Place 1 "A place" "description" [Exit "South" ["s"] 2] ,
                 Place 2 "A place" "description" [Exit "North" ["n"] 1] ]

sampleGraph = createGraph samplePlaces

main :: IO()
main = hspec $ do

    describe "Graph" $ do

        describe "nodeFromPlace" $
            it "creates a node representing a place" $ 
                nodeFromPlace defaultPlace `shouldBe` (1, defaultPlace)

        describe "edgesFromPlace" $ 
            it "creates a list of edges from the list of exits from a place" $ 
                edgesFromPlace defaultPlace `shouldBe` [(1, 1, "")]

        describe "createGraph" $ 
            it "creates a graph from a list of places and directions" $ 
                pendingWith "Dunno how to test this."
    
        describe "maybeFindNode" $ do
            it "finds a node by following direction from a list of edges" $
                maybeFindNode "South" (out sampleGraph 1)
                    `shouldBe` Just 2  
            it "return Nothing if no matching edge" $ 
                maybeFindNode "Albequerque" (out sampleGraph 1)
                    `shouldBe` Nothing

        describe "findNodeByDirection" $ do
            it "finds the place matching the exit direction" $ 
                findNodeByDirection 1  "South" sampleGraph `shouldBe` 
                    Right 2
            it "returns the old place if no matching edges" $ 
                findNodeByDirection 1 "Clockwise" sampleGraph `shouldBe` 
                    Left "You can't go that way."

    describe "Dictionary" $ do

        describe "toDefinition" $ do
            it "converts an exit to a definition" $
                toDefinition ("Down", ["d", "descend"]) `shouldBe`
                    [("d","Down"),("descend","Down"),("down","Down")]
        
        describe "toDictionary" $ do
            it "converts a list (directions, list of synonyms) to a dictionary" $ 
                toDictionary [("South", ["s"])] `shouldBe` 
                    Map.fromList [("s", "South"), ("south", "South")]
            it "should be empty given an empty list" $ 
                toDictionary [] `shouldBe` Map.empty 
