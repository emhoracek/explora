module Places where

import Inventory

data Place =  Place { num          :: Int
                    , name         :: String
                    , description  :: String
                    , inventory    :: Inventory
                    , exits        :: [Exit]
                      } deriving Eq
instance Show Place where
    show place = name place ++ "\n" ++ 
                 description place ++ "\n" ++
                 show (inventory place) ++ 
                 show (exits place)

defaultPlace :: Place
defaultPlace = Place 1 "A place" "Description of the place." [] [defaultExit]

type Direction = String

data Exit = Exit { direction :: Direction
                 , synonyms  :: [ String ]
                 , node      :: Int } deriving (Eq, Ord)
instance Show Exit where
   show exit = direction exit ++ ": " ++ 
               show (synonyms exit) ++ " " ++ 
               show (node exit)

defaultExit :: Exit
defaultExit = Exit "" [] 1
