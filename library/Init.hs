module Init where

import System.Directory
import Data.List(isSuffixOf)

listGames :: FilePath -> IO [FilePath]
listGames dir = fmap (filter (isSuffixOf ("exp"))) (getDirectoryContents dir)
