module Player where

import Data.Map (Map, fromList, adjust)
import Data.List (find)
import DIYGraph
import Items
import Places
import Properties

data Player = Player { 
                currentPlace :: NodeID,
                playerInventory :: [Item],
                playerInfo :: Properties
                } deriving (Eq, Show)
instance Inv Player where
    findItem str p = find (\x -> itemName x == str) (playerInventory p)
    removeItem i p= p { playerInventory = filter (/= i) (playerInventory p)}
    addItem i p= p { playerInventory = i : playerInventory p }



-- Player starts with no inventory at first place in graph.
makePlayer :: Graph Place Direction -> Player
makePlayer graph = Player { currentPlace = head $ nodes graph,
                            playerInventory = [],
                            playerInfo = fromList [("description", "As lovely as ever."), 
                                          ("Alive", "True"), ("Won", "False"), 
                                          ("score", "0")]}

changePlayer :: String -> String -> Player -> Player
changePlayer property value player =
        let f = adjust (const value) property in
        player { playerInfo = f $ playerInfo player }

killPlayer :: Player -> Player
killPlayer =  changePlayer "Alive" "False"

movePlayer :: Int -> Player ->  Player
movePlayer node player = player { currentPlace = node }

