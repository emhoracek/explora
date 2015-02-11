module Items where

import Data.Map (adjust)
import Properties

data Item = Item { itemName :: String,
                   itemInfo :: Properties
                   } deriving (Eq)
instance Show Item where
    show (Item name info) = name


changeItem :: String -> (String -> String) -> Item -> Item
changeItem string f item =
    item { itemInfo = adjust f string (itemInfo item) }

