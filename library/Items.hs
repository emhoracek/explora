module Items where

import Data.Map (Map(..), fromList, adjust, lookup)
import Graph
import DIYGraph
import Places
import Properties

data Item = Item { itemInfo :: Properties
                   } deriving (Eq, Show)

changeItem :: String -> (String -> String) -> Item -> Item
changeItem string f item =
    item { itemInfo = adjust f string (itemInfo item) }


