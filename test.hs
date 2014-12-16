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
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Graph" $

    describe "nodeFromPlace" $
      it "creates a node representing a place" $ 
        nodeFromPlace defaultPlace
          `shouldBe` (1, defaultPlace)

    describe "edgesFromPlace" $ do
      it "creates a list of edges from the list of exits from a place" $ do
        edgesFromPlace defaultPlace `shouldBe` [(1, 1, "")]

    -- not sure how to test this function.
    describe "createGraph" $ do
      it "creates a graph from a list of places and directions" $ do
        pendingWith "Dunno how to test this."
    
    describe "maybeFindNode" $ do
      it "finds a node by following direction from a list of edges" $ do
        maybeFindNode "South" (out sampleGraph 1)
          `shouldBe` Just 2  
      it "return Nothing if no matching edge" $ do
        maybeFindNode "Albequerque" (out sampleGraph 1)
          `shouldBe` Nothing

    describe "findNodeByDirection" $ do
      it "finds the place matching the exit direction" $ do
        findNodeByDirection 1  "South" sampleGraph `shouldBe` 
            Right 2
      it "returns the old place if no matching edges" $ do
        findNodeByDirection 1 "Clockwise" sampleGraph `shouldBe` 
            Left "You can't go that way."

    describe "Dictionary" $ do
        
        describe "toDictionary" $ do
            it "converts a list (directions, list of synonyms) to a dictionary" $ do
                toDictionary [("South", ["s"])] `shouldBe` 
                    Map.fromList [("s", "South")] 
