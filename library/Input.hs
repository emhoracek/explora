module Input where

import Dictionary
import Data.Maybe (isJust)
import Response

type Verb = String
type Noun = String
type Input = (Verb, Noun)

isADirection :: String -> Dictionary -> Bool
isADirection string dict = not (null $ words string) && 
                           isJust (inputToDirection string dict)

toDirection :: String -> Dictionary -> Either Response Input 
toDirection input dict = 
    case inputToDirection input dict of 
       Just dir -> Right ("go", dir) 
       Nothing  -> Left (BadInput input)

validateInput :: String -> Dictionary -> Either Response Input
validateInput "" _ = Left NoInput
validateInput string dict 
    | isADirection string dict = toDirection string dict
    | firstWord == "go"        = toDirection rest dict
    | otherwise                = Left $ BadInput string 
    where firstWord = head $ words string
          rest = concat $ tail $ words string
        
