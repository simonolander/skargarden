module Progress where

import Prelude

import Data.Foldable (all)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Region (Region)
  
type Progress =
    { rowProgress :: Array (Tuple Int Int) 
    , columnProgress :: Array (Tuple Int Int) 
    , islandProgress :: Array (Tuple Region (Tuple Int Int))
    , unknownTerrainTypes :: Int
    }

isSolved :: Progress -> Boolean 
isSolved progress =
    all (Tuple.uncurry (==)) progress.rowProgress
        && all (Tuple.uncurry (==)) progress.columnProgress 
        && all (Tuple.uncurry (==)) (map Tuple.snd progress.islandProgress)
        && progress.unknownTerrainTypes == 0