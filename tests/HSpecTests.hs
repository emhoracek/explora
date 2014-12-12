module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Map (Map)
import qualified Data.Map as Map
import Dictionary


main :: IO()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Parse" $ do

    describe "this " $ do
      it "converts the file to a list" $ do 
        pending
