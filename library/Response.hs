module Response where

import Game (Game(..))

-- These are all the responses the player can get for their input.
-- "Okay Action" means it's okay to take that action.
data Response = NoInput
              | BadInput String
              | Impossible String
              | Okay Game String
              deriving Eq
instance Show Response where
    show NoInput = "Enter a direction, any direction."
    show (BadInput input) = "I don't know what \"" ++ input ++ "\" means."
    show (Impossible reason) = "That's impossible. " ++ reason 
    show (Okay _ string) = string
