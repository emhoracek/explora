module World where

import DIYGraph
import Parse
import Dictionary
import Graph
import Places

import Text.ParserCombinators.Parsec (ParseError) 

data World = World { currentPlace :: NodeID,
                     mapGraph :: Graph Place String } deriving Show
