module Inventory where

data Object = Object { objName :: String,
                       objDesc :: String } deriving (Eq, Show)

type Inventory = [ Object ]


