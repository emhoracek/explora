module Test.ParseSpec (spec) where

import Test.Hspec
import Parse
import Test.Samples

main :: IO()
main = hspec spec

spec :: Spec
spec =  do

    describe "Parse" $ do

        describe "parseExits" $ do
            it "changes a file/string to a list of exits" $
                parseExits sampleMapExitsGood `shouldBe`
                    Right sampleExits
            it "tells you the error if bad input" $ 
                pendingWith "Dunno how to test this?"

        describe "parsePlaces" $ do
            it "changes a file/string to a list of places" $
                parsePlaces sampleMap `shouldBe`
                    Right samplePlaces
            it "tells you the error if bad input" $
                pendingWith "Dunno how to test this?"

        describe "parseDictionary" $ do
            it "changes a file/string to a list of definitions" $
                parseDictionary sampleMapExitsGood `shouldBe`
                    Right sampleMapExitsDefinition
            it "tells you the error if bad input" $
                pendingWith "Dunno how to test."

