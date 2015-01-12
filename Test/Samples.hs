module Test.Samples where

import Test.Hspec
import Test.QuickCheck
import Data.Graph.Inductive.Graph
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Places
import Graph
import Dictionary
import Parse
import Text.ParserCombinators.Parsec.Error(ParseError(..), Message, newErrorMessage, errorMessages, messageEq)
import Main

import Text.Parsec.Pos(SourcePos, initialPos)

-- ParseError isn't an instance of Eq
instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

sampleDefinitions :: Dictionary 
sampleDefinitions = Map.fromList [ ("s", "South"), ("n", "North"), ("south", "South"), ("north", "North") ]

sampleExits :: [Exit]
sampleExits = [ Exit "South" ["s"] 2 ]

samplePlaces :: [ Place ]
samplePlaces = [ Place 1 "A place" "description" [Exit "South" ["s"] 2] ,
                 Place 2 "A place" "description" [Exit "North" ["n"] 1] ]

sampleGraph = createGraph samplePlaces

sampleMap :: String
sampleMap = "1. A place\n description\n -> South (s): 2, West (w): 3 \n " ++
            "2. A place\n description\n -> North (n): 1"

sampleMapExitsGood :: String
sampleMapExitsGood = "-> South (s): 2"

sampleMapExitsDefinition :: Dictionary
sampleMapExitsDefinition = Map.fromList [ ("s", "South"), ("south", "South") ]

sampleMapExitsBad :: String
sampleMapExitsBad = "-> South @#4 f(s): 2"

sampleWorld :: World
sampleWorld = World { currentPlace = 1,
                      mapGraph = sampleGraph }

sampleGame :: Game
sampleGame = Game { world = sampleWorld,
                    dictionary = sampleDefinitions }
