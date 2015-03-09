module Main where

import Game
import Loop

-- | Location of default game
exampleFile :: String
exampleFile = "games/example.exp"

-- | Opens a game file and either starts the game or reports an error. 
main :: IO ()
main = do
    putStrLn "What game file would you like to play?" 
    file <- getLine
    f <- if file == "" then readFile exampleFile else readFile file
    -- TODO: what if the file isn't there?
    let game = initGame f
    either print startGame game
