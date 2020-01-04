module Array.Extra where

import Prelude
import Data.Array as Array
import Data.Maybe as Maybe

updateAt_ :: forall a. Int -> a -> Array a -> Array a
updateAt_ index value array =
    Array.updateAt index value array 
        # Maybe.fromMaybe array