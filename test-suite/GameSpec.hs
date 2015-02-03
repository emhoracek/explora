module GameSpec (spec) where

import Test.Hspec
import Game
import Samples

spec = do
    describe "initGame" $ do
        context "a messed up place" $
            it "returns an error" $
                initGame "1. applesauce (dinosaur)" `shouldBe`
                    Left "\"Map error: \" (line 1, column 25):\nunexpected end of input\nexpecting \"\\n\""
        context "a messed up exit" $
            it "returns an error" $
                pendingWith "Sooooooo broken"
        context "a good game file" $
            it "initializes the game" $
                initGame sampleFile `shouldBe`
                    Right sampleGame
