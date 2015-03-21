module ActionsSpec where

import Test.Hspec
import Actions
import Items
import Player
import Samples
import Response
import Game

import SampleItems
spec = do
    describe "validateAction" $ do
        it "says okay if good direction from node" $
            validateAction (Right ("go", "South")) sampleGame
                `shouldBe` Okay (Game (movePlayer 2 samplePlayer) sampleGraph sampleDefinitions) ""
        it "says impossible if can't go that way" $
            validateAction (Right ("go", "North")) sampleGame 
                `shouldBe` Impossible "You can't go North."
        it "gives the response if it's a bad action" $
            validateAction (Left NoInput) sampleGame `shouldBe`
                NoInput

    describe "tryAction" $ do
        context "go somewhere" $
            it "returns a world with a new place" $
                tryAction ("go", "South") sampleGame `shouldBe`
                    go "South" sampleGame
        context "kill player" $
            it "kills the player" $
                tryAction ("kill", "player") sampleGame `shouldBe`
                    Okay (sampleGame { player = killPlayer (player sampleGame)}) "You have died." 
        context "look" $
            it "same world and description" $
                tryAction ("look", "") sampleGame `shouldBe`
                    Okay sampleGame (look sampleGame)
        context "examine" $
            it "shows a description of the item" $
                tryAction ("examine", "box of hair dye") itemsGame `shouldBe`
                    Okay itemsGame "You can change the color of your hair with this." 
        context "impossible action" $
            it "responds saying it's impossible" $
                tryAction ("take", "moon") sampleGame `shouldBe`
                    Impossible "You can't see any moon."

    describe "go" $ do
        context "good direction" $
            it "returns okay with a good world" $
                pending
        context "impossible direction" $
            it "says that's impossible" $
                go "to Albequerque" sampleGame `shouldBe`
                    Impossible "You can't go to Albequerque."

    describe "look" $
        it "give a description of the place" $
            look sampleGame `shouldBe`
                "A place\ndescription\n"

    describe "examine" $ do
        context "self" $
            it "gives a description of the player" $ 
                examine "self" itemsGame `shouldBe`
                    Okay itemsGame "As lovely as ever."
        context "box of hair dye" $
            it "give a description of the hair dye" $
                examine "box of hair dye" itemsGame `shouldBe`
                    Okay itemsGame "You can change the color of your hair with this."
        context "something that's not there" $
            it "says you can't see that" $
                examine "elephant in the room" itemsGame `shouldBe`
                    Impossible "You don't see any \"elephant in the room\" here." 

    context "takeItem" $
        it "moves an item from the place inventory to the player's" $
            takeItem "box of hair dye" itemsGame `shouldBe`
                Okay itemsGameChanged "Taken."

    context "dropItem" $
        it "moves an item from the player's inventory to the current place" $
            dropItem "box of hair dye" itemsGameChanged `shouldBe`
                Okay itemsGame "Dropped."

