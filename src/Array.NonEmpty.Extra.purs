module Array.NonEmpty.Extra where

import Prelude
import Array.Extra as Array.Extra
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Maybe as Maybe
import Effect (Effect)

chooseOne :: forall a. NonEmptyArray a -> Effect a
chooseOne array =
    Array.NonEmpty.toArray array 
        # Array.Extra.chooseOne
        <#> Maybe.fromMaybe (Array.NonEmpty.head array)