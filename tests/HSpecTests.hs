module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Graph.Inductive.Graph

import Places
import Graph
import Dictionary

samplePlaces :: [ Place ]
samplePlaces = [ Place 1 "A place" "description" [ Exit "South" 2 ],
                 Place 2 "A place" "description" [ Exit "North" 1 ] ]

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

    describe "edgesFromNode" $ do
      it "lists the edges connected to the node" $ do 
        edgesFromNode (nodeFromPlace $ head samplePlaces) sampleGraph
          `shouldBe` [ (1, 2, "South") ]

    describe "findNodeByDirection" $ do
      it "finds the exit matching the direction" $ do
        findNodeByDirection (nodeFromPlace $ head samplePlaces)  "South" sampleGraph `shouldBe`
          (nodeFromPlace $ last samplePlaces)
