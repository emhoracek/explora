module Spec where

import Test.Hspec

import qualified DictionarySpec
import qualified GraphSpec
import qualified ParseSpec
import qualified MainSpec

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "Graph"      GraphSpec.spec
    describe "Dictionary" DictionarySpec.spec
    describe "Parse"      ParseSpec.spec
    describe "Main"       MainSpec.spec
