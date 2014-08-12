{-# LANGUAGE OverloadedStrings #-}



module Main where

import Data.Graph.Inductive
import Data.Char (toLower)
import Data.Map (Map, fromList, findWithDefault)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec


-- I got most of this parsec stuff from Real World Haskell
-- Then I warped it to my own purposes in probably not the best fashion.
dslFile = many1 line

line = do
  char '{'
  rawNum  <- many digit
  char '.'
  char ' '
  rawName <- placeString
  char '\n'
  skipMany space
  rawDesc <- placeString
  char '\n'
  skipMany space
  char '['
  rawExits <- listexits
  char ']'
  char '}'
  char '\n'
  return (Place (read rawNum) rawName rawDesc rawExits)

placeString = many (noneOf "\n{}\\")
listexits = option [NoExit] $ sepBy getExits (char ',')

getExits = do
   skipMany space
   ddir <- many (noneOf "[],:()")
   char ':'
   skipMany space
   dnode <- many digit
   let dexit = Exit ddir (read dnode)
   return dexit

parseDSL :: String -> Either ParseError [Place]
parseDSL = parse dslFile "(unknown)"

dslFileName :: FilePath
dslFileName = "places.exp"

initGraph = do
    file <- readFile dslFileName
    let p = parseDSL file
    let makeGraph l = placeGraph (placesToNodes l) (concat $ listAllExits l)
    let pl = either defaultPlaces id p
    let grph = makeGraph pl
    return grph

data Place =  Place { num   :: Int
                      , name  :: String
                      , desc  :: String
                      , exits :: [Exit]
                      }
instance Show Place where
    show (Place _ name desc _) = name ++ "\n" ++ desc

defaultPlaces s = [Place 1 "Failure" (show s) []]

data Exit = Exit { dir  :: String
                 , node :: Int }
            | NoExit deriving Show

defaultExit = Exit "" 1

placesToNodes :: [Place] -> [(Int, Place)]
placesToNodes = map (\x -> (num x, x))

-- We want to end up with a list of all the exits for all the places, in edge format
listAllExits :: [Place] -> [[(Int, Int, String)]]
listAllExits = map listExits

listExits :: Place -> [(Int, Int, String)]
listExits x = map ((\ n x -> (n, node x, dir x)) (num x)) (exits x)

-- This turns the lists of nodes and list of edges into an inductive graph.
placeGraph :: [(Int, Place)] -> [(Int, Int, String)] -> Gr Place String
placeGraph = mkGraph

-- this maps all the things the player should be allowed to type in to the
-- directions on the edges
dictDirs :: Data.Map.Map String String
dictDirs =  fromList [("n", "North"),("s", "South"),("e", "East"), ("w", "West"),
                      ("north", "North"),("south", "South"),("east", "East"), ("west", "West"),
                      ("d", "Down"), ("down", "Down"), ("u", "Up"), ("up", "Up"),
                      ("xyzzy", "xyzzy"), ("jump", "jump")]
strToDir :: String -> String
strToDir dir = findWithDefault "error" (map toLower dir) dictDirs

-- "out" gives a list of edges that go out from a node. This folds a function
-- over the the list that looks at each edge and sees if it is going in the
-- same direction as the player wanted. If it's going in the right direction,
-- then that exit's node is the new node. Otherwise, it's the node stays the same.
tryExits :: String -> Node -> Gr Place String -> Node
tryExits exit node graph = foldr (tryEdge exit) node $ out graph node

tryEdge :: String -> LEdge String -> Node -> Node
tryEdge exit (_, newNode, label) oldNode | label == exit = newNode
                                         | otherwise     = oldNode

-- Shows description of a new place.
showDesc :: Node -> Gr Place String -> String
showDesc place graph = "\n" ++ show (lab' $ context graph place)

-- Shows why you're in the same place
showError :: String -> String -> String
showError input dir | input == ""    = "Enter a direction, any direction."
                    | dir == "error" = "I don't know what \"" ++ input ++ "\" means."
                    | otherwise      = "You can't go that way."

loop :: Node -> Gr Place String -> IO ()
loop place grph = do
    putStrLn "\nWhere do you want to go? \nEnter a direction (e, w, n, s)"
    inputDir <- getLine
    let direction = strToDir inputDir
    let newPlace = tryExits direction place grph
    putStrLn $
        if place == newPlace then showError inputDir direction
        else                      showDesc newPlace grph
    loop newPlace grph

--main :: IO ()
main = do
    grph <- initGraph
    let startPlace = 1
    print $ lab' $ context grph startPlace
    loop startPlace grph
