module Dict where

import Data.Char (toLower)
import Data.Map (Map, fromList, findWithDefault)
import Parse (parseDictionary)
import Text.ParserCombinators.Parsec 

-- this maps all the things the player should be allowed to type in to the
-- directions on the edges
dictDirs :: [[(String, String)]] -> Data.Map.Map String String
dictDirs x =  fromList $ concat x

-- this gets a matching edge to look for for each input the play puts in. 
strToDir :: String -> Data.Map.Map String String -> String
strToDir dir dictionary = findWithDefault "no match" (map toLower dir) dictionary

dictFileName = "dictionary.exp"

--defaultDict :: String -> Data.Map.Map String String
defaultDict s =  fromList [("n", "North"),("s", "South"),("e", "East"), ("w", "West"),
                      ("north", "North"),("south", "South"),("east", "East"), ("west", "West"),
                      ("d", "Down"), ("down", "Down"), ("u", "Up"), ("up", "Up"),
                      ("xyzzy", "xyzzy"), ("jump", "jump"), ("error", s)]

dictionary :: Either ParseError [[(String, String)]] ->Data.Map.Map String String
dictionary p = either (defaultDict . show) dictDirs p


-- Is this the best way to do this?
initDictionary = do
  file <- readFile dictFileName
  let p = parseDictionary file
  -- print $ show p -- for debugging -- I need to learn how to test!!
  return $ dictionary p
