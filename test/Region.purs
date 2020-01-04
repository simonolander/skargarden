module Test.Region where

import Prelude

import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Region (Region)
import Region as Region
import Test.Assert (assertEqual, assertEqual')

main :: Effect Unit
main = do
    testGetBounds
    testRotateClockwise

testGetBounds :: Effect Unit
testGetBounds = do 
    assertEqual
        { actual : Region.getBounds $ Set.empty
        , expected : 
            { minRow : 0
            , maxRow : 0
            , minColumn : 0
            , maxColumn : 0
            }
        }
    assertEqual 
        { actual : 
            Tuple 0 0
                # Set.singleton
                # Region.getBounds
        , expected : 
            { minRow : 0
            , maxRow : 0
            , minColumn : 0
            , maxColumn : 0
            }
        }
    assertEqual 
        { actual : 
            Tuple 10 20
                # Set.singleton
                # Region.getBounds
        , expected : 
            { minRow : 10
            , maxRow : 10
            , minColumn : 20
            , maxColumn : 20
            }
        }
    assertEqual 
        { actual : 
            Set.fromFoldable
                [ Tuple 10 10
                , Tuple 5 20
                , Tuple 15 15
                ]
                # Region.getBounds
        , expected : 
            { minRow : 5
            , maxRow : 15
            , minColumn : 10
            , maxColumn : 20
            }
        }

testRotateClockwise :: Effect Unit
testRotateClockwise = 
    let 
        assertRotationEquals :: Region -> Region -> Region -> Region -> Effect Unit
        assertRotationEquals r0 r1 r2 r3 = do 
            assertEqual' "Region.rotateClockwise r0 == r1"
                { actual : Region.rotateClockwise r0, expected : r1 }
            assertEqual' "Region.rotateClockwise r1 == r2"
                { actual : Region.rotateClockwise r1, expected : r2 }
            assertEqual' "Region.rotateClockwise r2 == r3"
                { actual : Region.rotateClockwise r2, expected : r3 }
            assertEqual' "Region.rotateClockwise r3 == r0"
                { actual : Region.rotateClockwise r3, expected : r0 }
    in do
    let r0 = Set.empty
        r1 = r0
        r2 = r0
        r3 = r0
    assertRotationEquals r0 r1 r2 r3

    let r0 = (Set.singleton $ Tuple 0 0)
        r1 = r0
        r2 = r0
        r3 = r0
    assertRotationEquals r0 r1 r2 r3
        
    let r0 = (Set.singleton $ Tuple 1 1)
        r1 = r0
        r2 = r0
        r3 = r0
    assertRotationEquals r0 r1 r2 r3
        
    let r0 = Set.fromFoldable [ Tuple 0 0, Tuple 1 1 ]
        r1 = Set.fromFoldable [ Tuple 1 0, Tuple 0 1 ]
        r2 = r0
        r3 = r1
    assertRotationEquals r0 r1 r2 r3