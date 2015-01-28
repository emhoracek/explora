module Spec where

import Test.Hspec

import qualified Test.DictionarySpec
import qualified Test.GraphSpec
import qualified Test.ParseSpec
import qualified Test.MainSpec

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "Graph"      Test.GraphSpec.spec
    describe "Dictionary" Test.DictionarySpec.spec
    describe "Parse"      Test.ParseSpec.spec
    describe "Main"       Test.MainSpec.spec
