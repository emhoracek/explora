module Items where

import Data.Map (adjust)
import Properties

data Item = Item { itemName :: String,
                   itemInfo :: Properties
                   } deriving (Eq, Show)

changeItem :: String -> (String -> String) -> Item -> Item
changeItem string f item =
    item { itemInfo = adjust f string (itemInfo item) }


