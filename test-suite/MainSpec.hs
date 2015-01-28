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
            showDesc (World samplePlayer sampleGraph) `shouldBe`
                 "A place\ndescription"

    describe "validateAction" $ do
        it "says okay if good direction from node" $
            validateAction (Right ("go", "South")) sampleGame 
                `shouldBe` Okay (World (Player 2 [] 0 True) sampleGraph)
        it "says impossible if can't go that way" $
            validateAction (Right ("go", "North")) sampleGame 
                `shouldBe` Impossible "You can't go North."

    describe "stepWorld" $ do
        context "valid direction" $
            it "steps the world forward to new place" $
                pendingWith "no Eq instance for Graph"
