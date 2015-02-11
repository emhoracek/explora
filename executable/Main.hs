module Main where

import Game
import Loop
import System.Environment 

--Location of default game
exampleFile :: String
exampleFile = "games/example.exp"

-- Open a game file (need better way), check the game, and 
-- initialize the loop or print the error. 
main :: IO ()
main = do
    putStrLn "What game file would you like to play?" 
    file <- getLine
    f <- if file == "" then readFile exampleFile else readFile file
    let game = initGame f
    either print startGame game
