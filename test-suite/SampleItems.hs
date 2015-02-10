module SampleItems where

import Test.Hspec
import Test.QuickCheck
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Places
import Game
import Graph
import Items
import DIYGraph
import Dictionary
import Parse
import Player
import Text.ParserCombinators.Parsec.Error(ParseError(..), Message, newErrorMessage, errorMessages, messageEq)
import Samples 
import Text.Parsec.Pos(SourcePos, initialPos)

itemString = "\n# item: The description of the item."

sampleInventory = [ Item { itemName = "item",
                           itemInfo = fromList [("description", "The description of the item.")]} ]

itemsDefinitions :: Dictionary 
itemsDefinitions = Map.fromList [ ("s", "South"), ("n", "North"), ("south", "South"), ("north", "North") ]

hairDye = Item { itemName = "box of hair dye",
                 itemInfo = fromList [("description", "You can change the color of your hair with this.")] }

itemsPlaces = [ Place 1 "A place" "description" [hairDye] [] [Exit "South" ["s"] 2] ,
                 Place 2 "A place" "description" [] [] [Exit "North" ["n"] 1] ]

itemsPlacesChanged = [ Place 1 "A place" "description" [] [] [Exit "South" ["s"] 2] ,
                 Place 2 "A place" "description" [] [] [Exit "North" ["n"] 1] ]

itemsGraph = ( [("North", 2)], (1, head itemsPlaces), [("South", 2)]) :&:
              (([("South", 1)], (2, last itemsPlaces), [("North", 1)]) :&: EmptyGraph)

itemsGraphChanged = ( [("North", 2)], (1, head itemsPlacesChanged), [("South", 2)]) :&:
              (([("South", 1)], (2, last itemsPlaces), [("North", 1)]) :&: EmptyGraph)

itemsPlayer :: Player 
itemsPlayer = makePlayer itemsGraph

itemsPlayerChanged :: Player
itemsPlayerChanged = (makePlayer itemsGraphChanged) { playerInventory = [ hairDye ] }

itemsGame :: Game
itemsGame = Game { player = itemsPlayer, 
                   mapGraph = itemsGraph,
                   dictionary = itemsDefinitions }

itemsGameChanged :: Game
itemsGameChanged = 
    Game {player = Player {currentPlace = 1, 
                           playerInventory = [Item {itemName = "box of hair dye", 
                                                itemInfo = fromList [("description","You can change the color of your hair with this.")]}], 
                           playerInfo = fromList [("Alive","True"),("Won","False"),("description","As lovely as ever."),("score","0")]}, 
          mapGraph = itemsGraphChanged, 
          dictionary = itemsDefinitions }

