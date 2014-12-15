module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Graph.Inductive.Graph
import Data.Set (Set, fromList, empty)
import qualified Data.Set as Set
import Places
import Graph
import Dictionary

samplePlaces :: [ Place ]
samplePlaces = [ Place 1 "A place" "description" (Set.fromList [Exit "South" Set.empty 2]) ,
                 Place 2 "A place" "description" (Set.fromList [Exit "North" Set.empty 1]) ]

sampleGraph = createGraph samplePlaces

main :: IO()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Graph" $ do

    describe "nodeFromPlace" $ do
      it "creates a node representing a place" $ do 
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
        maybeFindNode "South" (edgesFromNode 1 sampleGraph)
          `shouldBe` Just 2  
      it "return Nothing if no matching edge" $ do
        maybeFindNode "Albequerque" (edgesFromNode 1 sampleGraph)
          `shouldBe` Nothing

    describe "findNodeByDirection" $ do
      it "finds the place matching the exit direction" $ do
        findNodeByDirection 1  "South" sampleGraph `shouldBe` 2
      it "returns the old place if no matching edges" $ do
        findNodeByDirection 1 "Clockwise" sampleGraph `shouldBe` 1
