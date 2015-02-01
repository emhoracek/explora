module Actions where

import DIYGraph   
import Game
import Graph
import Dictionary
import Places
import Player
import Response
import Input

-- Checks if it's possible to verb that noun.
validateAction :: Either Response Input -> Game -> Response
validateAction input game = case input of
    Left response   -> response
    Right goodInput -> tryAction goodInput game 

tryAction :: Input -> Game -> Response 
tryAction ("go", direction) game = go direction game
tryAction ("kill", "player") game = Okay game { player = killPlayer (player game) } "You have died."
tryAction ("look", "") game = Okay game (look game)
tryAction input _  = Impossible "You can't do that."

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

