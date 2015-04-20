module Init where

import System.Directory
import Data.List(isSuffixOf)

-- | Lists the games in a directory.
listGames :: FilePath -> IO [FilePath]
listGames dir = fmap (filter (isSuffixOf "exp")) (getDirectoryContents dir)
