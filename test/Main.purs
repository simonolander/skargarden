-- | While I would love to demonstrate how to test Halogen applications, I quite simply haven't
-- | had the time! If you would like to see tests for a particular part of the app, please consider
-- | filing an issue or participating in an existing one.
module Test.Main where

import Prelude

import Effect (Effect)
import Test.Region as Test.Region

main :: Effect Unit
main = do
    Test.Region.main