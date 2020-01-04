module Array.Extra where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random as Random

updateAt_ :: forall a. Int -> a -> Array a -> Array a
updateAt_ index value array =
    Array.updateAt index value array 
        # Maybe.fromMaybe array

chooseOne :: forall a. Array a -> Effect (Maybe a)
chooseOne array = 
    Array.length array - 1
        # Random.randomInt 0
        <#> Array.index array

shuffle :: forall a. Array a -> Effect (Array a)
shuffle array = 
    Array.length array
        # flip replicateA Random.random
        <#> Array.zip array
        <#> Array.sortWith Tuple.snd
        <#> map Tuple.fst