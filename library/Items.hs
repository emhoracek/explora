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

-- | Change a property of an item.
changeItem :: String -- ^ Name of the property to be changed
              -> (String -> String) -- ^ A function to change the property
              -> Item -- The item to change
              -> Item -- The resulting item
changeItem string f item =
    item { itemInfo = adjust f string (itemInfo item) }
-- | Use it this way:
-- | changeItem "attack" (show $ (* 3) read) sword
-- | where sword is a named item representing a sword with a property called
-- "attack". Using changeItem this way will multiply that property by 3.
-- More simply, one can also supply a new value instead of a function, simply
-- by using const:
-- changeItem "attack" (const "15") sword
-- Now the sword's "attack" property is simply "15". 
