module Main where

import DIYGraph
import Parse
import Dictionary
import Graph
import Places
import World

import Text.ParserCombinators.Parsec (ParseError) 
import System.Environment 

--Location of default game
exampleFile :: String
exampleFile = "games/example.exp"

data Game = Game { world :: World,
                   dictionary :: Dictionary } deriving Show

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

-- These are all the responses the player can get for their input.
-- "Okay Action" means it's okay to take that action.
data Response = NoInput
              | BadInput String
              | Impossible Direction
              | Okay Action
              deriving Eq
instance Show Response where
    show NoInput = "Enter a direction, any direction."
    show (BadInput input) = "I don't know what \"" ++ input ++ "\" means."
    show (Impossible dir) = "You can't go " ++ dir ++ "."
    show (Okay action) = "Okay."
    
newtype Action = Action { action :: World -> World }
instance Eq Action where
    (==) x y = True

go :: NodeID -> World -> World  
go n (World _ graph) = World n graph

-- Checks whether input is in dictionary.
validateInput :: String -> Game -> Response 
validateInput "" _ = NoInput
validateInput input game = 
    case inputToDirection input (dictionary game) of 
        Just n -> validateDirection n game
        Nothing -> BadInput input

-- Checks if the inputed direction is possible.
validateDirection :: Direction -> Game -> Response
validateDirection dir game = 
    case findNodeByDirection 
            (currentPlace $ world game) dir (mapGraph $ world game) of
        Just n  -> Okay $ Action (go n)
        Nothing -> Impossible dir 

-- Game loop. Show description of current state, get input from player,
-- determine correct response and display, step world if possible, and loop.
loop :: Game -> IO ()
loop game = do
    putStrLn $ showDesc $ world game
    inputDir <- getLine
    let response = validateInput inputDir game
    print response
    let newWorld = stepWorld response (world game)
    loop (Game newWorld (dictionary game))

-- If the response is "Okay", change the world, otherwise it says the same.
stepWorld :: Response -> World -> World
stepWorld (Okay (Action change)) world = change world
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
