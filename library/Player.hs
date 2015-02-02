module Player where

import Data.Map (Map(..), fromList, adjust, lookup)
import Graph
import DIYGraph
import Places

data Player = Player { currentPlace :: NodeID,
                       playerInventory :: [String],
                       playerInfo :: Info,
                       playerAttributes :: Attributes,
                       playerStats :: Stats } deriving (Eq, Show)

type Info = Data.Map.Map String String 
type Stats = Data.Map.Map String Int
type Attributes = Data.Map.Map String Bool

defaultStats :: Stats
defaultStats = fromList [("score", 0)]

-- Player starts with no inventory at first place in graph.
makePlayer :: Graph Place Direction -> Player
makePlayer graph = Player { currentPlace = head $ nodes graph,
                            playerInventory = [],
                            playerInfo = fromList [("description", "As lovely as ever.")],
                            playerAttributes = fromList [("Alive", True), ("Won", False)],
                            playerStats = fromList [("score", 0)] }

changeInfo :: String -> (String -> String) -> Player -> Player
changeInfo string f player =
    player { playerInfo = adjust f string (playerInfo player) }

getInfo :: String -> Player -> Maybe String
getInfo string player = Data.Map.lookup string (playerInfo player)

changeStat :: String -> (Int -> Int) -> Player -> Player
changeStat string f player = 
    player { playerStats = adjust f string (playerStats player) } 

getStat :: String -> Player -> Maybe Int
getStat string player = Data.Map.lookup string (playerStats player) 
 
changeAttribute :: String -> (Bool -> Bool) -> Player -> Player
changeAttribute string f player =
    player { playerAttributes = adjust f string (playerAttributes player) }

getAttribute :: String -> Player -> Maybe Bool
getAttribute string player = Data.Map.lookup string (playerAttributes player)

-- so not DRY oh noes D:


killPlayer :: Player -> Player
killPlayer = changeAttribute "Alive" (const False)

movePlayer :: Int -> Player ->  Player
movePlayer node player = player { currentPlace = node }

