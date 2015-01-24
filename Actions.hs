module Actions where
   
import World

newtype Action = Action { action :: World -> World }
instance Eq Action where
    (==) x y = True -- maybe a bad idea but ???

go :: Int -> World -> World  
go n (World _ graph) = World n graph
