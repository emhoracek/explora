module Places where

import Dictionary (Direction)

data Place =  Place { num          :: Int
                    , name         :: String
                    , description  :: String
                    , exits        :: [Exit]
                      } deriving Eq
instance Show Place where
    show (Place _ name desc _) = name ++ "\n" ++ desc

defaultPlace :: Place
defaultPlace = Place 1 "A place" "Description of the place." [defaultExit]

data Exit = Exit { direction :: Direction
                 , node      :: Int } deriving (Eq, Show)

defaultExit = Exit "" 1
