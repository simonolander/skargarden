module Util where

import Prelude

import Control.Monad.List.Trans (foldl)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (random)


choose :: forall a. Int -> Array a -> Effect (Array a)
choose k array =
    let n = Array.length array in
    if n <= k then 
        pure array
    else do 
        shuffled <- shuffle array
        pure $ Array.take k shuffled

pairs :: forall a b. Array a -> Array b -> Array (Tuple a b)
pairs as bs =
    Array.concat $ map ( \ a -> map (Tuple a) bs ) as

shuffle :: forall a. Array a -> Effect (Array a)
shuffle array =
    let 
        n = Array.length array
    in do
        randoms <- replicateA n random
        Array.zip randoms array
            # Array.sortWith Tuple.fst
            # map Tuple.snd
            # pure

count :: forall a. Ord a => Array a -> Array (Tuple a Int)
count array = 
    Array.nub array 
        # map (\ a -> Tuple a $ Array.length $ Array.filter ((==) a) array )

countIf :: forall a. (a -> Boolean) -> Array a -> Int
countIf fn array =
    Array.foldl (\ n a -> if fn a then n + 1 else n) 0 array