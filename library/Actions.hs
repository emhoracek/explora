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


-- TODO: fix this whole mess. Per R0ml: Actions are actions, i.e. functions. Lookup reification.

-- | Checks if it's possible to verb that noun.
validateAction :: Either Response Input -> Game -> Response
validateAction input game = case input of
    Left response   -> response
    Right goodInput -> tryAction goodInput game 

-- | Takes a verb, noun input and find the appropriate response.
tryAction :: Input -> Game -> Response 
tryAction ("go", direction) game = go direction game
tryAction ("kill", "player") game = Okay game { player = killPlayer (player game) } "You have died."
tryAction ("look", "") game = Okay game (look game)
tryAction ("examine", string) game = examine string game
tryAction ("take", string) game = takeItem string game
tryAction ("drop", string) game = dropItem string game
tryAction ("inventory", "") game = listInventory game
tryAction _ _  = Impossible "You can't do that."

-- | Shorthand to get current location in the game.
getPlace :: Game -> Place
getPlace game = label (mapGraph game) (currentPlace $ player game) 

-- | Lists all the items the player can currently access.
knownItems :: Game -> [Item]
knownItems game = (playerInventory $ player game) ++ (inventory $ getPlace game)

-- | Shorthand to maybe find an item the player can currently access by its name.
maybeItem :: String -> Game -> Maybe Item
maybeItem string game = find (\x -> itemName x == string) (knownItems game)

-- Actions

-- | Game verb: Go in a direction.
go :: Direction -> Game -> Response
go dir game =
    let n = currentPlace $ player game in
    case findNodeByDirection n dir (mapGraph game) of
        Just newNode -> Okay game { player = movePlayer newNode (player game)} ""
        Nothing      -> Impossible ("You can't go " ++ dir ++ ".")

-- | Game verb: Shows description of a place.
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

-- | Game verb: Show the description of an item.
examine :: String -> Game -> Response
examine "self" game = Okay game "As lovely as ever."
examine string game = 
      let maybeInfo s x = fromMaybe "Oops, no description." 
            (Data.Map.lookup s (itemInfo x)) in
      case maybeItem string game of 
        Just x -> Okay game (maybeInfo "description" x)
        Nothing -> Impossible $ "You don't see any \"" ++ string ++ "\" here."

-- | Change the player.
changeGamePlayer :: (Player -> Player) -> Game -> Game
changeGamePlayer f g = g { player = f (player g) }

-- | Change the graph.
changeGameGraph :: (Place -> Place) -> Game -> Game
changeGameGraph f g = g { mapGraph = newGraph }
    where
        newGraph = changeNode (currentPlace $ player g, changedPlace) (mapGraph g)
        changedPlace = f (getPlace g)

-- | Game verb: Take an item from the environment.
takeItem :: String -> Game -> Response
takeItem string game =
    case maybeItem string game of 
        Just x -> Okay (changeGame x) "Taken."
        Nothing -> Impossible $ "You can't see any " ++ string ++ "."
    where
        changeGame i = changeGamePlayer (addItem i) 
                       $ changeGameGraph (removeItem i) game

-- | Game verb: Drop an item into the current location.
dropItem :: String -> Game -> Response
dropItem string game = 
    case findItem string (player game) of
        Just x -> Okay (changeGame x) "Dropped."
        Nothing -> Impossible "You aren't carrying that."
    where 
        changeGame i = changeGamePlayer (removeItem i)
                       $ changeGameGraph (addItem i) game

-- | Game verb: Lists the players inventory.
listInventory :: Game -> Response
listInventory game = Okay game inventoryString
    where inventoryString = intercalate "\n" (map show (playerInventory $ player game))
