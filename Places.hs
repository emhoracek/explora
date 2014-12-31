module Places where

import Dictionary (Direction)

data Place =  Place { num          :: Int
                    , name         :: String
                    , description  :: String
                    , exits        :: [Exit]
                      } deriving Eq
instance Show Place where
    show (Place _ name desc exits) = name ++ "\n" ++ desc ++ 
                                    (show exits)

defaultPlace :: Place
defaultPlace = Place 1 "A place" "Description of the place." [defaultExit]

data Exit = Exit { direction :: Direction
                 , synonyms  :: [ String ]
                 , node      :: Int } deriving (Eq, Show, Ord)

defaultExit = Exit "" [] 1
