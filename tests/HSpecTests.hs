module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Graph.Inductive.Graph

import Places
import Graph
import Dictionary

main :: IO()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Places" $ do

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
