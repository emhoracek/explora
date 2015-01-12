module Places where

data Place =  Place { num          :: Int
                    , name         :: String
                    , description  :: String
                    , exits        :: [Exit]
                      } deriving Eq
instance Show Place where
    show (Place _ name desc exits) = name ++ "\n" ++ desc ++ "\n" ++ 
                                    show exits

defaultPlace :: Place
defaultPlace = Place 1 "A place" "Description of the place." [defaultExit]

type Direction = String

data Exit = Exit { direction :: Direction
                 , synonyms  :: [ String ]
                 , node      :: Int } deriving (Eq, Ord)
instance Show Exit where
   show (Exit dir syn node) = dir ++ ": " ++ show syn ++ " " ++ show node
defaultExit = Exit "" [] 1
