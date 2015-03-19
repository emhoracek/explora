module Input where

import Dictionary
import Data.Maybe (isJust)
import Response

type Verb = String
type Noun = String
type Input = (Verb, Noun)

-- | This strips spaces from the beginning and end but not the middle
stripExtraSpaces :: String -> String
stripExtraSpaces string
    | string == " " || string == ""  = ""
    | head string == ' ' = stripExtraSpaces $ tail string
    | last string == ' ' = stripExtraSpaces $ init string
    | otherwise          = string

-- | Tells if a string is a direction in the Dictionary.
isADirection :: String -> Dictionary -> Bool
isADirection string dict = isJust (inputToDirection string dict)

-- | Turns a string into a direction.
toDirection :: String -> Dictionary -> Either Response Input 
toDirection input dict = 
    case inputToDirection input dict of 
       Just dir -> Right ("go", dir) 
       Nothing  -> Left (BadInput input)

-- | If string is valid input, change it to a verb, noun pain, otherwise the
-- response is a "BadInput" or "NoInput" error.
validateInput :: String -> Dictionary -> Either Response Input
validateInput string dict
    | stripExtraSpaces string == "" = Left NoInput 
    | isADirection string dict      = toDirection string dict
    | firstWord == "go"             = toDirection rest dict
    | firstWord == "look"           = Right ("look", "")
    | firstWord == "examine"        = Right ("examine", rest)
    | firstWord == "take"          =  Right ("take", rest)
    | string == "kill player"       = Right ("kill", "player")
    | string == "inventory"         = Right ("inventory", "")
    | otherwise                     = Left $ BadInput string 
    where firstWord = stripExtraSpaces $ head $ words string
          rest = concat $ tail $ words string 
