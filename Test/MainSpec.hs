module Test.MainSpec (spec) where

import Test.Hspec
import Test.Samples
import Main

main = hspec spec

spec :: Spec
spec = do 
    describe "showDesc" $
       it "gets the desciption of a place on the graph" $
            showDesc (World 1 sampleGraph) `shouldBe`
                 "A place\ndescription"

    describe "validateDirection" $ do
        it "says okay if good direction from node" $
            validateDirection "South" sampleGame 
                `shouldBe` Okay 2
        it "says impossible if can't go that way" $
            validateDirection "North" sampleGame 
                `shouldBe` Impossible "North"

    describe "validateInput" $ do
        context "no input" $
            it "tells the player what to do" $
                validateInput "" sampleGame `shouldBe`
                    NoInput 
        context "input that's not in the dictionary" $
            it "tells the user it doesn't understand" $
                validateInput "alberquerque" sampleGame `shouldBe`
                    BadInput "alberquerque"
        context "input in the dictionary but that doesn't work" $
            it "tells the user they can't do that" $
                validateInput "north" sampleGame `shouldBe`
                    Impossible "North"
        context "good input" $
            it "says okay for a direction" $
                validateInput "south" sampleGame `shouldBe`
                    Okay (go 2)
            it "says okay for \"go\" and a direction" $
                validateInput "go south" sampleGame `shouldBe`
                    Okay (go 2)

    describe "stepWorld" $ do
        context "valid direction" $
            it "steps the world forward to new place" $
                pendingWith "no Eq instance for Graph"
