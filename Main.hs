module Main where

import Data.Graph.Inductive

import Parse
import Dictionary
import Graph
import Places
import Data.Map (Map)
import Data.Maybe (isNothing, fromJust)

import Text.ParserCombinators.Parsec (ParseError) 


--Locations of dictionary and map (for now)
dictFile = "dictionary.exp"
placesFile = "places.exp"

-- because Dictionary never changes
data Game = Game { world :: World,
                   dictionary :: Dictionary } deriving Show

-- idea from Mary on making the World a data type like in Elm
data World = World { currentPlace :: Node,
                     mapGraph :: Gr Place String } deriving Show

initZeShit file = do    
    f <- readFile file
    let p = parsePlaces f
    let d = parseDictionary f
    let init = parseGame p d
    return init

parseGame ::  Either ParseError [Place] -> Either ParseError Dictionary  
                -> Either String Game
parseGame (Right p) (Right d) = let graph = createGraph p in
                                Right (Game (World (head $ nodes graph) graph) 
                                        d)
parseGame (Left a)  (Left b)  = Left (show a ++ " " ++ show b)
parseGame (Left a)  (Right b) = Left $ show a
parseGame (Right a) (Left b)  = Left $ show b

-- Shows description of a new place.
showDesc :: World -> String
showDesc (World node graph) = name (lab' $ context graph node) ++ "\n" ++ 
                               description (lab' $ context graph node)

data Response = BadDictionary
              | BadPlaceFile
              | NoInput
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
    putStrLn $ showDesc $ world game
    inputDir <- getLine
    let response = validateInput inputDir game
    print response
    let newWorld = stepWorld response (world game)
    loop (Game newWorld (dictionary game))

main = do
    game <- initZeShit placesFile
    either print loop game
