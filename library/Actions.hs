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
import Data.List (find, delete, intercalate)
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
tryAction ("take", string) game = takeItem string game
tryAction ("drop", string) game = dropItem string game
tryAction ("inventory", "") game = listInventory game
tryAction _ _  = Impossible "You can't do that."

getPlace :: Game -> Place
getPlace game = label (mapGraph game) (currentPlace $ player game) 

-- Searching all items

-- hlint has no idea what it's talking about here, this is the best way to write this,
-- F. U. hlint
knownItems :: Game -> [Item]
knownItems game = (playerInventory $ player game) ++ (inventory $ getPlace game)

maybeItem :: String -> Game -> Maybe Item
maybeItem string game = find (\x -> itemName x == string) (knownItems game)

-- Actions

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
        graph = mapGraph game
        i = map show (inventory $ getPlace game)
        inventoryString = 
            case length i of 
                0 -> ""
                1 -> "\nThere is a " ++ head i ++ " here."
                otherwise -> "\nThere are " ++ intercalate " and " i ++ " here."
    in
    name (label graph node) ++ "\n" ++ 
        description (label graph node) ++ 
        inventoryString

examine :: String -> Game -> Response
examine "self" game = Okay game "As lovely as ever."
examine string game = 
      let maybeInfo s x = fromMaybe "Oops, no description." 
            (Data.Map.lookup s (itemInfo x)) in
      case maybeItem string game of 
        Just x -> Okay game (maybeInfo "description" x)
        Nothing -> Impossible $ "You don't see any \"" ++ string ++ "\" here."

changeGamePlayer :: (Player -> Player) -> Game -> Game
changeGamePlayer f g = g { player = f (player g) }

changeGameGraph :: (Place -> Place) -> Game -> Game
changeGameGraph f g = g { mapGraph = newGraph }
    where 
        newGraph = changeNode (currentPlace $ player g, changedPlace) (mapGraph g)
        changedPlace = f (getPlace g)

takeItem :: String -> Game -> Response
takeItem string game =
    case maybeItem string game of 
        Just x -> Okay (changeGame x) "Taken."
        Nothing -> Impossible "You don't see that."
    where
        changeGame i = changeGamePlayer (addItem i) 
                       $ changeGameGraph (removeItem i) game

dropItem :: String -> Game -> Response
dropItem string game = 
    case findItem string (player game) of
        Just x -> Okay (changeGame x) "Dropped."
        Nothing -> Impossible "You aren't carrying that."
    where 
        changeGame i = changeGamePlayer (removeItem i)
                       $ changeGameGraph (addItem i) game

listInventory :: Game -> Response
listInventory game = Okay game inventoryString
    where inventoryString = intercalate "\n" (map show (playerInventory $ player game))
