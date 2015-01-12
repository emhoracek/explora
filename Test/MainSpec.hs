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
