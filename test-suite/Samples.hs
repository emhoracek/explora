module Samples where

import Test.Hspec
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Places
import Game
import Graph
import DIYGraph
import Dictionary
import Parse
import Player
import Text.ParserCombinators.Parsec.Error(ParseError(..), Message, newErrorMessage, errorMessages, messageEq)

import Text.Parsec.Pos(SourcePos, initialPos)

-- ParseError isn't an instance of Eq
instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

sampleFile :: String
sampleFile = 
    "1. A place\n\
    \    description\n\
    \-> South (s): 2\n\
    \2. A place\n\
    \    description\n\
    \-> North (n): 1"

sampleDefinitions :: Dictionary 
sampleDefinitions = Map.fromList [ ("s", "South"), ("n", "North"), ("south", "South"), ("north", "North") ]

sampleExits :: [Exit]
sampleExits = [ Exit "South" ["s"] 2 ]

samplePlaces :: [ Place ]
samplePlaces = [ Place 1 "A place" "description" [] [] [Exit "South" ["s"] 2] ,
                 Place 2 "A place" "description" [] [] [Exit "North" ["n"] 1] ]

sampleGraph = ( [("North", 2)], (1, head samplePlaces), [("South", 2)]) :&:
              (([("South", 1)], (2, last samplePlaces), [("North", 1)]) :&: EmptyGraph)

sampleMap :: String
sampleMap = "1. A place\n description\n-> South (s): 2\n" ++
            "2. A place\n description\n-> North (n): 1"

sampleMap2 :: String
sampleMap2 = "1. A place\n description\n-> South (s): 2\n" ++
             "2. A pit\n description\n" ++
             "3. A place\n description\n-> North (n): 1, West (w): 2"

samplePlaces2 :: [ Place ]
samplePlaces2 = [ Place 1 "A place" "description" [] [] [Exit "South" ["s"] 2],
                  Place 2 "A pit" "description" [] [] [],
                  Place 3 "A place" "description" [] [] [Exit "North" ["n"] 1, Exit "West" ["w"] 2]]

sampleMapExitsGood = "-> South (s): 2"

sampleMap2Defs :: Dictionary 
sampleMap2Defs = Map.fromList [ ("s", "South"), ("south", "South"),
                                ("n", "North"), ("north", "North"),
                                ("w", "West"), ("west", "West") ]

sampleMap2Exits = [ Exit "South" ["s"] 2, Exit "North" ["n"] 1, 
                    Exit "West" ["w"] 2]

sampleMapExitsBad ::  String
sampleMapExitsBad = "-> South @#4 f(s): 2"

samplePlayer :: Player 
samplePlayer = makePlayer sampleGraph

sampleGame :: Game
sampleGame = Game { player = samplePlayer, 
                    mapGraph = sampleGraph,
                    dictionary = sampleDefinitions }
