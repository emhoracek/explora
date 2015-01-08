import Test.Hspec

import qualified Test.DictionarySpec
import qualified Test.GraphSpec
import qualified Test.ParseSpec

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    Test.GraphSpec.spec
    Test.DictionarySpec.spec
    Test.ParseSpec.spec
