{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Graph.Inductive.Graph
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Places
import Graph
import Dictionary
import Parse

import Text.ParserCombinators.Parsec.Error(ParseError(..), Message, newErrorMessage, errorMessages, messageEq)

import Text.Parsec.Pos(SourcePos, initialPos)

-- ParseError isn't an instance of Eq
instance Eq ParseError where
   a == b = errorMessages a == errorMessages b


sampleExits :: [Exit]
sampleExits = [ Exit "South" ["s"] 2 ]

samplePlaces :: [ Place ]
samplePlaces = [ Place 1 "A place" "description" [Exit "South" ["s"] 2] ,
                 Place 2 "A place" "description" [Exit "North" ["n"] 1] ]

sampleGraph = createGraph samplePlaces

sampleMap :: String
sampleMap = "1. A place\n description\n -> South (s): 2 \n " ++
            "2. A place\n description\n -> North (n): 1"

sampleMapExits_good :: String
sampleMapExits_good = "-> South (s): 2"

sampleMapExits_bad :: String
sampleMapExits_bad = "-> South @#4 f(s): 2"

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
                    Just 2
            it "gives you nothing if you can't go that way" $ 
                findNodeByDirection 1 "Clockwise" sampleGraph `shouldBe` 
                    Nothing

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
            it "converts a list (directions, list of synonyms) to a dictionary" $ 
                toDictionary [("South", ["s"]), ("South", [])] `shouldBe` 
                    Map.fromList [("s", "South"), ("south", "South")]
                
        describe "inputToDirection" $ do
            it "finds Just a direction corresponding to what the user types" $
                inputToDirection "s" (toDictionary [("South", ["s"])])
                    `shouldBe` Just "South"
            it "gives you Nothing if no match" $
                inputToDirection "apple" (toDictionary [("South", ["s"])])
                    `shouldBe` Nothing

    describe "Parse" $ do

        describe "parseExits" $ do
            it "changes a string to a list of exits" $
                parseExits sampleMapExits_good `shouldBe`
                    Right sampleExits
            it "tells you the error if bad input" $ 
                pendingWith "Dunno how to test this?"

        describe "parsePlaces" $ do
            it "changes a string to a list of places" $
                parsePlaces sampleMap `shouldBe`
                    Right samplePlaces
            it "tells you the error if bad input" $
                pendingWith "Dunno how to test this?"
