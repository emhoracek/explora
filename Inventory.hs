module Inventory where

data Object = Object { objName :: String,
                       objDesc :: String } deriving (Eq, Show)

type Inventory = [ Object ]

playerInventory = [ Object "flashlight" "a shiny black Maglite flashlight",
                    Object "piece of paper" "it says \"welcome to the game\"" ]
