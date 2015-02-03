module SamplesItems where

import Test.Hspec
import Test.QuickCheck
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

sampleDefinitions :: Dictionary 
sampleDefinitions = Map.fromList [ ("s", "South"), ("n", "North"), ("south", "South"), ("north", "North") ]


hairDye = Item { itemName = "box of hair dye",
                 itemInfo = fromList [("description", "You can change the color of your hair with this.")] }

itemPlaces = [ Place 1 "A place" "description" [hairDye] [] [Exit "South" ["s"] 2] ,
                 Place 2 "A place" "description" [] [] [Exit "North" ["n"] 1] ]

itemGraph = ( [("North", 2)], (1, head samplePlaces), [("South", 2)]) :&:
              (([("South", 1)], (2, last samplePlaces), [("North", 1)]) :&: EmptyGraph)

itemsPlayer :: Player 
itemsPlayer = makePlayer itemsGraph

itemsGame :: Game
itemsGame = Game { player = itemsPlayer, 
                   mapGraph = itemsGraph,
                   dictionary = itemsDefinitions }
