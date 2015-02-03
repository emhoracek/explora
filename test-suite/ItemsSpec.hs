module ItemsSpec (spec) where

import Test.Hspec
import Items

import Data.Map (fromList)

spec = do
    describe "changeItem" $
        it "changes something about the item" $
            changeItem "open" (const "closed")
                Item { itemInfo = fromList [("open", "open")] } 
            `shouldBe` 
                Item { itemInfo = fromList [("open", "closed")] } 
