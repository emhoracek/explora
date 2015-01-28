module InputSpec (spec) where

import Test.Hspec 
import Input
import Response
import Samples

spec = do
    describe "isADirection" $
        it "says whether the input is a direction" $
            isADirection "south" sampleDefinitions `shouldBe` True
    describe "toDirection" $
        it "changes the string to input" $
            toDirection "south" sampleDefinitions `shouldBe` 
                Right ("go", "South")
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
            it "is okay to have extra spaces" $
                validateInput " go    south  " sampleDefinitions `shouldBe`
                    Right ("go", "South")
