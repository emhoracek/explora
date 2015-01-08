module Test.DictionarySpec (spec) where

import Test.Hspec
import Dictionary
import Places
import Data.Map (Map, fromList)
import qualified Data.Map as Map

main :: IO()
main = hspec spec

spec :: Spec
spec =  do
   describe "Dictionary" $ do

        describe "toDefinition" $ 
            it "converts an exit to a definition" $
                exit2Definitions (Exit "Down" ["d", "descend"] 2) `shouldBe`
                    [("d","Down"),("descend","Down"),("down","Down")]
        
        describe "toDictionary" $ do
            it "converts a list (directions, list of synonyms) to a dictionary" $ 
                toDictionary [Exit "South" ["s"] 1] `shouldBe` 
                    Map.fromList [("s", "South"), ("south", "South")]
            it "should be empty given an empty list" $ 
                toDictionary [] `shouldBe` Map.empty
                
        describe "inputToDirection" $ do
            it "finds Just a direction corresponding to what the user types" $
                inputToDirection "s" (toDictionary [Exit "South" ["s"] 1])
                    `shouldBe` Just "South"
            it "gives you Nothing if no match" $
                inputToDirection "apple" (toDictionary [Exit "South" ["s"] 1])
                    `shouldBe` Nothing

