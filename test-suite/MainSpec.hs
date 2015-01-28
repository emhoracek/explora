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
            showDesc sampleGame `shouldBe`
                 "A place\ndescription"

    describe "stepWorld" $ do
        context "valid direction" $
            it "steps the world forward to new place" $
                pending 
