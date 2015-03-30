module InitSpec where

import Test.Hspec
import Init

spec = do

    describe "listGames" $ do
        it "lists the games in the current directory" $
            listGames "/home/libby/dev/exploration-game/games" >>= ( `shouldBe`
                ["demo-w-items.exp","tinygame.exp","demo.exp","example.exp","idealgame.exp"])
