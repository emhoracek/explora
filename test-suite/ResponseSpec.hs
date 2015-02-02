module ResponseSpec (spec) where

import Test.Hspec
import Response
import Samples

spec = 
    describe "show" $ do
        it "shows NoInput" $
            show NoInput `shouldBe` "Enter a direction, any direction."
        it "shows BadInput" $
            show (BadInput "hello") `shouldBe` "I don't know what \"hello\" means."
        it "shows Impossible actions" $
            show (Impossible "") `shouldBe` "That's impossible. "
        it "shows response to okay actions" $
            show (Okay sampleGame "Okay.") `shouldBe` "Okay."
