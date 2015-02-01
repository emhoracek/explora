module Main where

import Actions
import DIYGraph
import Parse
import Dictionary
import Graph
import Input
import Places
import Player
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


-- Shows description of a new place.
showDesc :: Game -> String
showDesc game  =
    let node = currentPlace $ player game 
        graph = mapGraph game in 
    name (label graph node) ++ "\n" ++ 
        description (label graph node)

-- Game loop. Show description of current state, get input from player,
-- determine correct response and display, step world if possible, and loop.
loop :: Game -> IO ()
loop game = do
    putStrLn $ showDesc game 
    inputDir <- getLine
    let input = validateInput inputDir (dictionary game)
    let response = validateAction input game 
    let newWorld = stepWorld response game 
    print response
    let todo = onEntry $ label (mapGraph newWorld) (currentPlace $ player newWorld)
    nextWorld <- if null todo then return newWorld
                        else entryAction todo newWorld
    if getAttribute "Alive" (player nextWorld) == Just True then loop nextWorld else putStrLn $ showDesc newWorld  

entryAction :: String -> Game -> IO Game
entryAction string game = do
    let input = validateInput string (dictionary game) 
    let response = validateAction input game
    let newWorld = stepWorld response game
    return newWorld



-- If the response is "Okay", change the world, otherwise it says the same.
stepWorld :: Response -> Game -> Game
stepWorld (Okay newWorld string) _ =
    newWorld
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
