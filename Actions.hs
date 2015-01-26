module Actions where
   
import Game
import Graph
import Dictionary
import Places
import Response
import Input

tryAction :: (String, String) -> World -> Response 
tryAction ("go", direction) = go direction
tryAction = Impossible "You can't do that."

go :: Direction -> World -> Response
go dir (World n graph) = 
    case findNodeByDirection n dir graph of
        Just newNode -> Okay (World newNode graph)
        Nothing      -> Impossible ("You can't go " ++ dir ++ ".")
