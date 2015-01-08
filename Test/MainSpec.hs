module MainSpec (spec) where

import Test.Hspec
import Test.Samples
import Main

main = hspec spec

spec :: Spec
spec = do 
  describe "showDesc" $
    it "gets the desciption of a place on the graph" $
        showDesc 1 sampleGraph `shouldBe`
             "description"
