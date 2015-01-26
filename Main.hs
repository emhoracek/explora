module Main where

import Actions
import DIYGraph
import Parse
import Dictionary
import Graph
import Input
import Places
import Game
import Response

import Text.ParserCombinators.Parsec (ParseError) 
import System.Environment 

--Location of default game
exampleFile :: String
exampleFile = "games/example.exp"


initGame :: String -> Either String Game
initGame file =    
    let p = parsePlaces file
        d = parseDictionary file in
    parseGame p d

-- is this the best way to "add" these Eithers?
-- If both are Right, make the game. If the place file was file, but the
-- dictionary wasn't, show the error for the dictionary. Otherwise, show 
-- the error for the dictionary.
parseGame :: Either ParseError [Place] -> Either ParseError Dictionary ->
                Either String Game
parseGame (Right p) (Right d) = Right $ makeGame p d
parseGame (Right p) (Left  d) = Left $ show d
parseGame (Left  p)  _        = Left $ show p

-- Games start at the first node listed in the graph.
makeGame :: [Place] -> Dictionary -> Game
makeGame p d = let graph = createGraph p in
               Game (World (head $ nodes graph) graph) d

-- Shows description of a new place.
showDesc :: World -> String
showDesc (World node graph) = name (label graph node) ++ "\n" ++ 
                              description (label graph node)

-- Checks if it's possible to verb that noun.
validateAction :: Either Response Input -> Game -> Response
validateAction input game = case input of
    Left response   -> response
    Right goodInput -> tryAction goodInput (world game) 

-- Game loop. Show description of current state, get input from player,
-- determine correct response and display, step world if possible, and loop.
loop :: Game -> IO ()
loop game = do
    putStrLn $ showDesc $ world game
    inputDir <- getLine
    let input = validateInput inputDir (dictionary game)
    let response = validateAction input game 
    print response
    let newWorld = stepWorld response (world game)
    loop (Game newWorld (dictionary game))

-- If the response is "Okay", change the world, otherwise it says the same.
stepWorld :: Response -> World -> World
stepWorld (Okay newWorld) world = newWorld
stepWorld _ world = world

-- Open a game file (need better way), check the game, and 
-- initialize the loop or print the error. 
main :: IO ()
main = do
    putStrLn "What game file would you like to play?" 
    file <- getLine
    f <- if file == "" then readFile exampleFile else readFile file
    let game = initGame f
    either print loop game
