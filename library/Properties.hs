module Properties where

import Data.Map (Map, lookup)

type Properties = Map String String

getInfo :: String -> Properties -> Maybe String
getInfo = Data.Map.lookup
