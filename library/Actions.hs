module Actions where
   
import Game
import Graph
import Dictionary
import Places
import Response
import Input

-- Checks if it's possible to verb that noun.
validateAction :: Either Response Input -> Game -> Response
validateAction input game = case input of
    Left response   -> response
    Right goodInput -> tryAction goodInput game 

tryAction :: Input -> Game -> Response 
tryAction ("go", direction) world = go direction world
tryAction ("kill", "player") world = killPlayer world
tryAction input _  = Impossible "You can't do that."

go :: Direction -> Game -> Response
go dir (Game (Player n i s a) graph dict) = 
    case findNodeByDirection n dir graph of
        Just newNode -> Okay (Game (Player newNode i s a) graph dict)
        Nothing      -> Impossible ("You can't go " ++ dir ++ ".")

killPlayer :: Game -> Response
killPlayer (Game (Player n i s a) graph dict) =
    Okay $ Game  (Player n i s False) graph dict
