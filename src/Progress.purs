module Progress where

import Prelude

import Data.Foldable (all)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Rectangle (Size)
  
type Progress =
    { rowProgress :: Array (Tuple Int Int) 
    , columnProgress :: Array (Tuple Int Int) 
    , boatProgress :: Array (Tuple Size (Tuple Int Int))
    , unknownRegions :: Int
    }

isSolved :: Progress -> Boolean 
isSolved progress =
    all (Tuple.uncurry (==)) progress.rowProgress
        && all (Tuple.uncurry (==)) progress.columnProgress 
        && all (Tuple.uncurry (==)) (map Tuple.snd progress.boatProgress)
        && progress.unknownRegions == 0