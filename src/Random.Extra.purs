module Random.Extra where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Semigroup.Foldable (class Foldable1)
import Data.Tuple as Tuple
import Data.Unfoldable (replicateA)
import Data.Unfoldable1 (replicate1A)
import Effect (Effect)
import Effect.Random as Random

shuffle :: forall a. Array a -> Effect (Array a)
shuffle array =
    let 
        n = Array.length array
    in do
        randoms <- replicateA n Random.random
        Array.zip randoms array
            # Array.sortWith Tuple.fst
            # map Tuple.snd
            # pure

shuffleNonEmpty :: forall a. NonEmptyArray a -> Effect (NonEmptyArray a)
shuffleNonEmpty array =
    do 
        randoms <- replicate1A (Array.NonEmpty.length array) Random.random
        Array.NonEmpty.zip randoms array
            # Array.NonEmpty.sortWith Tuple.fst
            # map Tuple.snd
            # pure


chooseNonEmpty :: forall f a. Foldable1 f => f a -> Effect a
chooseNonEmpty foldable = 
    Array.NonEmpty.fromFoldable1 foldable
        # shuffleNonEmpty
        <#> Array.NonEmpty.head
