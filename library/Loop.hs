module Loop where

import Places
import Player
import DIYGraph
import Input
import Actions
import Properties
import Response
import Game

startGame :: Game -> IO ()
startGame game = do
    putStrLn $ look game
    loop game

-- Game loop. Show description of current state, get input from player,
-- determine correct response and display, step world if possible, and loop.
loop :: Game -> IO ()
loop game = do
    putStrLn ""
    inputDir <- getLine
    let input = validateInput inputDir (dictionary game)
    let response = validateAction input game 
    let newWorld = stepWorld response game 
    print response
    let todo = onEntry $ label (mapGraph newWorld) (currentPlace $ player newWorld)
    nextWorld <- if null todo then return newWorld
                        else entryAction todo newWorld
    if nextWorld == game then return ()
        else putStrLn $ look nextWorld
    if getInfo "Alive" (playerInfo $ player nextWorld) == Just "True" 
        then loop nextWorld 
        else return ()

-- This SUCKS
entryAction :: String -> Game -> IO Game
entryAction string game = do
    let input = validateInput string (dictionary game) 
    let response = validateAction input game
    let newWorld = stepWorld response game
    return newWorld

-- If the response is "Okay", change the world, otherwise it says the same.
stepWorld :: Response -> Game -> Game
stepWorld (Okay newWorld _) _ =
    newWorld
stepWorld _ world = world
