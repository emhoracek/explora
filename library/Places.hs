module Places where

import Data.List (find)
import Items

data Place =  Place { num          :: Int
                    , name         :: String
                    , description  :: String
                    , inventory    :: [Item]
                    , onEntry      :: String
                    , exits        :: [Exit]
                      } deriving Eq
instance Show Place where
    show place = name place ++ "\n" ++ 
                 description place ++ "\n" ++
                 show (inventory place) ++ 
                 show (exits place)
instance Inv Place where
    findItem str p = find (\x -> itemName x == str) (inventory p)
    removeItem i p = p { inventory = filter (/= i) (inventory p)}
    addItem i p = p { inventory = i : inventory p }

defaultPlace :: Place
defaultPlace = Place 1 "A place" "Description of the place." [] [] [defaultExit]

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
