module Test.MainSpec (spec) where

import Test.Hspec
import Test.Samples
import Main

main = hspec spec

spec :: Spec
spec = do 
    describe "showDesc" $
       it "gets the desciption of a place on the graph" $
            showDesc 1 sampleGraph `shouldBe`
                 "description"

    describe "validateDirection" $ do
        it "says okay if good direction from node" $
            validateDirection "South" sampleGame 
                `shouldBe` Okay
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
            it "says okay" $
                validateInput "south" sampleGame `shouldBe`
                    Okay
--}            

{--
    describe "respond" $ do
        it "decides how to respond to the users input" $
            response "n" (Just "North") `shouldBe`
                 "You try going North."
        it "tell the user if input is invalid" $
            response "pie" Nothing `shouldBe` 
                "I don't know what \"pie\" means."
        it "tells the player if you can't go that way" $
            response "n" (Just "North") Nothing `shouldBe`
                "You can't go that way."
        it "offers instructions to the user if nothing was typed" $
            response "" Nothing `shouldBe`
                "Enter a direction, any direction."
--}
