module Items where

import Data.Map (adjust)
import Properties


data Item = Item { itemName :: String,
                   itemInfo :: Properties,
                   itemActions :: [String]
                   } deriving (Eq)
instance Show Item where
    show (Item name info actions) = name


class Inv a where
    findItem :: String -> a -> Maybe Item
    removeItem :: Item -> a -> a 
    addItem :: Item -> a -> a


changeItem :: String -> (String -> String) -> Item -> Item
changeItem string f item =
    item { itemInfo = adjust f string (itemInfo item) }

