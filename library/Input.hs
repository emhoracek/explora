module Input where

import Dictionary
import Data.Maybe (isJust)
import Response

type Verb = String
type Noun = String
type Input = (Verb, Noun)

-- strip spaces from the beginning and end but not the middle
stripExtraSpaces :: String -> String
stripExtraSpaces string
    | string == " " || string == ""  = ""
    | head string == ' ' = stripExtraSpaces $ tail string
    | last string == ' ' = stripExtraSpaces $ init string
    | otherwise          = string

isADirection :: String -> Dictionary -> Bool
isADirection string dict = isJust (inputToDirection string dict)

toDirection :: String -> Dictionary -> Either Response Input 
toDirection input dict = 
    case inputToDirection input dict of 
       Just dir -> Right ("go", dir) 
       Nothing  -> Left (BadInput input)

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
