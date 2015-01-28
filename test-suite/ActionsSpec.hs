module ActionsSpec where

import Test.Hspec
import Actions
import Samples
import Response


spec = do
    describe "tryAction" $ do
        context "go somewhere" $
            it "returns a world with a new place" $
                pending
        context "impossible action" $
            it "responds saying it's impossible" $
                tryAction ("take", "moon") sampleWorld `shouldBe`
                    Impossible "You can't do that."

    describe "go" $ do
        context "good direction" $
            it "returns okay with a good world" $
                pending
        context "impossible direction" $
            it "says that's impossible" $
                go "to Albequerque" sampleWorld `shouldBe`
                    Impossible "You can't go to Albequerque." 
