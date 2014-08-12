module Dict where

import Data.Char (toLower)
import Data.Map (Map, fromList, findWithDefault)

-- this maps all the things the player should be allowed to type in to the
-- directions on the edges
dictDirs :: Data.Map.Map String String
dictDirs =  fromList [("n", "North"),("s", "South"),("e", "East"), ("w", "West"),
                      ("north", "North"),("south", "South"),("east", "East"), ("west", "West"),
                      ("d", "Down"), ("down", "Down"), ("u", "Up"), ("up", "Up"),
                      ("xyzzy", "xyzzy"), ("jump", "jump")]
strToDir :: String -> String
strToDir dir = findWithDefault "error" (map toLower dir) dictDirs
