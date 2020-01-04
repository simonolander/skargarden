module TerrainType where

import Prelude

data TerrainType 
    = Land
    | Water
    | Unknown

derive instance eqTerrainType :: Eq TerrainType
instance showTerrainType :: Show TerrainType where
    show Land = "Land"
    show Water = "Water"
    show Unknown = "Unknown"

rotate :: TerrainType -> TerrainType
rotate Unknown = Land
rotate Water = Unknown
rotate Land = Water

back :: TerrainType -> TerrainType
back = rotate >>> rotate

isLand :: TerrainType -> Boolean
isLand Land = true
isLand _ = false

isWater :: TerrainType -> Boolean
isWater Water = true
isWater _ = false

isUnknown :: TerrainType -> Boolean
isUnknown Unknown = true
isUnknown _ = false