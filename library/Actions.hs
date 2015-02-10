module Actions where

import DIYGraph 
import Game
import Graph
import Items
import Places
import Player
import Properties
import Response
import Input

import Data.Map (lookup)
import Data.List (find, delete)
import Data.Maybe (fromMaybe)

-- Checks if it's possible to verb that noun.
validateAction :: Either Response Input -> Game -> Response
validateAction input game = case input of
    Left response   -> response
    Right goodInput -> tryAction goodInput game 

tryAction :: Input -> Game -> Response 
tryAction ("go", direction) game = go direction game
tryAction ("kill", "player") game = Okay game { player = killPlayer (player game) } "You have died."
tryAction ("look", "") game = Okay game (look game)
tryAction ("examine", string) game = examine string game
tryAction _ _  = Impossible "You can't do that."

getPlace :: Game -> Place
getPlace game = label (mapGraph game) (currentPlace $ player game) 

knownItems :: Game -> [Item]
knownItems game = (playerInventory $ player game) ++ (inventory $ getPlace game)

findItem :: String -> [Item] -> Maybe Item
findItem str = find (\x -> itemName x == str) 

maybeItem :: String -> Game -> Maybe Item
maybeItem string game = findItem string (knownItems game)

go :: Direction -> Game -> Response
go dir game =
    let n = currentPlace $ player game in
    case findNodeByDirection n dir (mapGraph game) of
        Just newNode -> Okay game { player = movePlayer newNode (player game)} ""
        Nothing      -> Impossible ("You can't go " ++ dir ++ ".")

-- Shows description of a new place.
look :: Game -> String
look game  =
    let node = currentPlace $ player game 
        graph = mapGraph game in 
    name (label graph node) ++ "\n" ++ 
        description (label graph node)

examine :: String -> Game -> Response
examine "self" game = Okay game "As lovely as ever."
examine string game = 
      let maybeInfo s x = fromMaybe "Oops, no description." 
            (Data.Map.lookup s (itemInfo x)) in
      case maybeItem string game of 
        Just x -> Okay game (maybeInfo "description" x)
        Nothing -> Impossible $ "You don't see any \"" ++ string ++ "\" here."

takeItem :: String -> Game -> Response
takeItem string game =
    let curPlace = getPlace game
        currentNode = currentPlace $ player game
        changeGame item = Game { player = changePlayer item,
                             mapGraph = changeGraph item,
                             dictionary = dictionary game }
        changeGraph item = changeNode (currentPlace $ player game, changePlace item) (mapGraph game)
        changePlace item = curPlace { 
            inventory = delete item (inventory curPlace) }
        changePlayer item = (player game) { playerInventory = newPInv item }
        newPInv item = item : playerInventory (player game) in
    case maybeItem string game of 
        Just x -> Okay (changeGame x) "Taken."
        Nothing -> Impossible "You don't see that."
