module PlayerSpec (spec) where

import Test.Hspec
import Player
import Samples
import Data.Map (fromList)

spec = do 
    describe "makePlayer" $
        it "makes a player at the first place in the graph" $
            makePlayer sampleGraph `shouldBe` samplePlayer
    describe "changeInfo" $
        it "changes some information about the player" $
            changeInfo "description" (const "none") samplePlayer 
                `shouldBe`
            samplePlayer { 
                playerInfo = fromList 
                    [("description", "none")] }
    describe "getInfo" $ do
        context "existing key" $  
            it "gives the corresponsing info" $
                getInfo "description" samplePlayer `shouldBe`
                    Just "As lovely as ever."
        context "non-existent key" $
            it "returns Nothing" $ 
                getInfo "apple" samplePlayer `shouldBe` Nothing
    describe "changeStat" $ 
        it "changes a stat" $
            changeStat "score" (+ 2) samplePlayer `shouldBe`
                samplePlayer { playerStats = fromList [("score", 2)] }
    
    describe "getStat" $ do
        context "existing key" $
            it "gives the corresponsing stat" $
                getStat "score" samplePlayer `shouldBe`
                    Just 0
        context "nonexistent key" $
            it "returns Nothing" $
                getStat "apple" samplePlayer `shouldBe`
                    Nothing
    describe "changeAttribute" $
        it "changes a true/false attribute of a player" $
            changeAttribute "Alive" (const False) samplePlayer `shouldBe` 
                samplePlayer { 
                    playerAttributes = 
                        fromList [("Alive", False), ("Won", False)] }
    describe "getAttribute" $ do
        context "exisiting key" $
             it "gets the corresponding boolean value" $
                getAttribute "Alive" samplePlayer `shouldBe`
                    Just True
        context "nonexistent key" $
            it "returns Nothing" $
                getAttribute "apple" samplePlayer `shouldBe`
                    Nothing 
