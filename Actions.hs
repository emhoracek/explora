module Actions where
   
import Game
import Graph
import Dictionary
import Places
import Response
import Input

tryAction :: Input -> World -> Response 
tryAction ("go", direction) world = go direction world
tryAction input _  = Impossible "You can't do that."

go :: Direction -> World -> Response
go dir (World n graph) = 
    case findNodeByDirection n dir graph of
        Just newNode -> Okay (World newNode graph)
        Nothing      -> Impossible ("You can't go " ++ dir ++ ".")
