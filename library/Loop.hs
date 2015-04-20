module Loop where

import Places
import Player
import DIYGraph
import Input
import Actions
import Properties
import Response
import Game

-- | Starts the game loop.
startGame :: Game -> IO ()
startGame game = do
    putStrLn $ look game
    loop game

-- | Game loop.
loop :: Game -> IO ()
loop game = do
    -- | Print a blank line.
    putStrLn ""
    -- | Get player input.
    inputDir <- getLine
    let input = validateInput inputDir (dictionary game)
    -- | Get the response.
    let response = validateAction input game 
    let newWorld = stepWorld response game 
    -- | Print the response to the input
    print response
    -- | See if previous response requires another response (what a mess).
    let todo = onEntry $ label (mapGraph newWorld) (currentPlace $ player newWorld)
    -- | Change the world.
    nextWorld <- if null todo then return newWorld
                        else entryAction todo newWorld
    -- | Print response if needed.
    if nextWorld == game then return ()
        else putStrLn $ look nextWorld
    -- | End the game if the player is dead, else loop.
    if getInfo "Alive" (playerInfo $ player nextWorld) == Just "True" 
        then loop nextWorld 
        else return ()
    -- | Seriously, please forgive me.

-- | Steps the world on entry to a place. 
entryAction :: String -> Game -> IO Game
entryAction string game = do
    let input = validateInput string (dictionary game) 
    let response = validateAction input game
    let newWorld = stepWorld response game
    return newWorld

-- | If the response is "Okay", change the world, otherwise it says the same.
stepWorld :: Response -> Game -> Game
stepWorld (Okay newWorld _) _ =
    newWorld
stepWorld _ world = world
