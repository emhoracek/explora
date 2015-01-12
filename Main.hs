module Main where

import Data.Graph.Inductive

import Parse
import Dictionary
import Graph
import Places
import Data.Map (Map)
import Data.Maybe (isNothing, fromJust)


--Locations of dictionary and map (for now)
dictFile = "dictionary.exp"
placesFile = "places.exp"

-- because Dictionary never changes
data Game = Game { world :: World,
                   dictionary :: Dictionary } deriving Show

-- idea from Mary on making the World a data type like in Elm
data World = World { currentPlace :: Node,
                     mapGraph :: Gr Place String } deriving Show

-- Initialize the dictionary
--initDictionary file = do
--    f <- readFile file
--    let p = parseDictionary f
--    let dictionary = either (errorDictionary . show) createDictionary p
--    return dictionary  

-- Shows description of a new place.
showDesc :: World -> String
showDesc (World place graph) = description $ lab' $ context graph place

data Response = NoInput
              | BadInput String
              | Impossible Direction
              | Okay Node 
              deriving Eq
instance Show Response where
    show NoInput = "Enter a direction, any direction."
    show (BadInput input) = "I don't know what \"" ++ input ++ "\" means."
    show (Impossible dir) = "You can't go " ++ dir ++ "."
    show (Okay n) = "Okay."

validateInput :: String -> Game -> Response
validateInput "" _ = NoInput
validateInput input game = case inputToDirection input (dictionary game) of 
    Just n -> validateDirection n game
    Nothing -> BadInput input


validateDirection :: Direction -> Game -> Response
validateDirection dir game =  case findNodeByDirection 
                       (currentPlace $ world game) dir (mapGraph $ world game) of
    Just n -> Okay n
    Nothing -> Impossible dir 


stepWorld :: Response -> World -> World
stepWorld (Okay n)  (World place graph) =
    World n graph 
stepWorld _ world = world

loop game = do
    putStrLn "\n Hello"
    inputDir <- getLine
    let response = validateInput inputDir game
    print $ show response
    let newWorld = stepWorld response (world game)
    print $ showDesc newWorld
    loop (Game newWorld (dictionary game))


{--
loop :: World  -> Dictionary -> IO ()
loop (World graph place) dict = do
    putStrLn "\nWhere do you want to go? \nEnter a direction (e, w, n, s)"
    inputDir <- getLine
    let direction = findDirection inputDir dict
    let newPlace = tryExits direction place grph
    putStrLn $
        if place == newPlace then showError inputDir direction
        else                      showDesc newPlace grph
    loop (World graph newPlace) dict

--main :: IO ()
main = do
    grph <- initGraph placesFile
    dict <- initDictionary dictFile
    let startPlace = 1
    print $ lab' $ context grph startPlace
    loop startPlace grph dict

--}
