module Region where

import Prelude

data Region 
    = Boat
    | Water
    | Unknown

derive instance eqRegion :: Eq Region
instance showRegion :: Show Region where
    show Boat = "Boat"
    show Water = "Water"
    show Unknown = "Unknown"

rotate :: Region -> Region
rotate Unknown = Boat
rotate Water = Unknown
rotate Boat = Water

back :: Region -> Region
back = rotate >>> rotate

isBoat :: Region -> Boolean
isBoat Boat = true
isBoat _ = false

isWater :: Region -> Boolean
isWater Water = true
isWater _ = false

isUnknown :: Region -> Boolean
isUnknown Unknown = true
isUnknown _ = false