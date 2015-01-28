module ActionsSpec where

import Test.Hspec
import Actions
import Samples
import Response
import Game

spec = do
    describe "validateAction" $ do
        it "says okay if good direction from node" $
            validateAction (Right ("go", "South")) sampleGame
                `shouldBe` Okay (Game (Player 2 [] 0 Alive) sampleGraph sampleDefinitions)
        it "says impossible if can't go that way" $
            validateAction (Right ("go", "North")) sampleGame 
                `shouldBe` Impossible "You can't go North."

    describe "tryAction" $ do
        context "go somewhere" $
            it "returns a world with a new place" $
                pending
        context "kill player" $
            it "kills the player" $
                tryAction ("kill", "player") sampleGame `shouldBe`
                    Okay (Game (Player 1 [] 0 Dead) sampleGraph sampleDefinitions)
        context "impossible action" $
            it "responds saying it's impossible" $
                tryAction ("take", "moon") sampleGame `shouldBe`
                    Impossible "You can't do that."

    describe "go" $ do
        context "good direction" $
            it "returns okay with a good world" $
                pending
        context "impossible direction" $
            it "says that's impossible" $
                go "to Albequerque" sampleGame `shouldBe`
                    Impossible "You can't go to Albequerque."

