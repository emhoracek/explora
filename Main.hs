module Main where

import Data.Graph.Inductive

import Parse
import Dictionary
import Graph
import Places
import Data.Map (Map)

--Locations of dictionary and map (for now)
dictFile = "dictionary.exp"
placesFile = "places.exp"

-- idea from Mary on making the World a data type like in Elm
data World = World { mapGraph :: Gr Place String,
                     currentPlace :: Node }

-- Initialize the dictionary
--initDictionary file = do
--    f <- readFile file
--    let p = parseDictionary f
--    let dictionary = either (errorDictionary . show) createDictionary p
--    return dictionary  

-- Shows description of a new place.
showDesc :: Node -> Gr Place String -> String
showDesc place graph = description $ lab' $ context graph place

-- Shows why you're in the same place
--showError :: String -> String -> String
--showError input dir | input == ""    = "Enter a direction, any direction."
--                    | dir == "error" = "I don't know what \"" ++ input ++ "\" means."
--                    | otherwise      = "You can't go that way."

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
