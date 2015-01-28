module MainSpec (spec) where

import Test.Hspec
import Samples
import Main
import Game
import Actions
import Input
import Response 

main = hspec spec

spec :: Spec
spec = do 
    describe "showDesc" $
       it "gets the desciption of a place on the graph" $
            showDesc (World 1 sampleGraph) `shouldBe`
                 "A place\ndescription"

    describe "validateAction" $ do
        it "says okay if good direction from node" $
            validateAction (Right ("go", "South")) sampleGame 
                `shouldBe` Okay (World 2 sampleGraph)
        it "says impossible if can't go that way" $
            validateAction (Right ("go", "North")) sampleGame 
                `shouldBe` Impossible "You can't go North."

    describe "validateInput" $ do
        context "no input" $
            it "gives player instructions" $
                validateInput "" sampleDefinitions `shouldBe`
                    Left NoInput 
        context "input that's not in the dictionary" $
            it "tells the user it doesn't understand" $
                validateInput "alberquerque" sampleDefinitions `shouldBe`
                    Left (BadInput "alberquerque")
        context "input in the dictionary but that doesn't work" $
            it "allows it for now" $
                validateInput "north" sampleDefinitions `shouldBe`
                    Right ("go", "North")
        context "good input" $ do
            it "says okay for a direction" $
                validateInput "south" sampleDefinitions `shouldBe`
                    Right ("go", "South")
            it "says okay for \"go\" and a direction" $
                validateInput "go south" sampleDefinitions `shouldBe`
                    Right ("go", "South")

    describe "stepWorld" $ do
        context "valid direction" $
            it "steps the world forward to new place" $
                pendingWith "no Eq instance for Graph"
