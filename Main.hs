module Main where

import DIYGraph
import Parse
import Dictionary
import Graph
import Places

import Text.ParserCombinators.Parsec (ParseError) 
import System.Environment 

--Location of game file (for now)
placesFile :: String
placesFile = "places.exp"

-- because Dictionary never changes
data Game = Game { world :: World,
                   dictionary :: Dictionary } deriving Show

-- idea from Mary on making the World a data type like in Elm
data World = World { currentPlace :: NodeID,
                     mapGraph :: Graph Place String } deriving Show

initGame :: String -> Either String Game
initGame file =    
    let p = parsePlaces file
        d = parseDictionary file in
    parseGame p d

-- is this the best way to "add" these functions?
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

data Response = NoInput
              | BadInput String
              | Impossible Direction
              | Okay NodeID
              deriving Eq
instance Show Response where
    show NoInput = "Enter a direction, any direction."
    show (BadInput input) = "I don't know what \"" ++ input ++ "\" means."
    show (Impossible dir) = "You can't go " ++ dir ++ "."
    show (Okay _) = "Okay."

validateInput :: String -> Game -> Response
validateInput "" _ = NoInput
validateInput input game = 
    case inputToDirection input (dictionary game) of 
        Just n -> validateDirection n game
        Nothing -> BadInput input

validateDirection :: Direction -> Game -> Response
validateDirection dir game = 
    case findNodeByDirection 
            (currentPlace $ world game) dir (mapGraph $ world game) of
        Just n -> Okay n
        Nothing -> Impossible dir 

stepWorld :: Response -> World -> World
stepWorld (Okay n) (World _ graph) = World n graph 
stepWorld _ world = world

loop :: Game -> IO ()
loop game = do
    putStrLn $ showDesc $ world game
    inputDir <- getLine
    let response = validateInput inputDir game
    print response
    let newWorld = stepWorld response (world game)
    loop (Game newWorld (dictionary game))

main :: IO ()
main = do
    [file] <- getArgs
    f <- readFile file
    let game = initGame f
    either print loop game
