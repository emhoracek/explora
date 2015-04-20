module Properties where

import Data.Map (Map, lookup)

type Properties = Map String String

-- | This is... useless? Or not?
getInfo :: String -> Properties -> Maybe String
getInfo = Data.Map.lookup
