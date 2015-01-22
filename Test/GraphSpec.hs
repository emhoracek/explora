module Test.GraphSpec (spec) where

import Test.Hspec
import Graph
import Test.Samples
import Places ( defaultPlace ) 
import DIYGraph

main :: IO()
main = hspec spec

spec =  do
    
    describe "nodeFromPlace" $
        it "creates a node representing a place" $ 
            nodeFromPlace defaultPlace `shouldBe` (1, defaultPlace)

    describe "edgesFromPlace" $ 
        it "creates a list of edges from the list of exits from a place" $ 
            edgesFromPlace defaultPlace `shouldBe` [(1, 1, "")]

    describe "createGraph" $ 
        it "creates a graph from a list of places and directions" $ 
            createGraph samplePlaces `shouldBe` sampleGraph  
   
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
