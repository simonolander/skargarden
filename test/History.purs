module Test.History where 

import Prelude

import Effect (Effect)
import History as History
import Test.Assert (assertEqual, assertFalse', assertTrue)

main :: Effect Unit
main = do 
    testSingleton
    testAppend
    testBack
    testForward
    testCurrent
    testHasPast
    testHasFuture

testSingleton :: Effect Unit
testSingleton = do
    let history = History.singleton 15
    assertEqual
        { actual : History.current history
        , expected : 15
        }
    assertFalse' "History.singleton x has no past" $ History.hasPast history
    assertFalse' "History.singleton x has no future" $ History.hasFuture history

testAppend :: Effect Unit
testAppend = do
    let history = History.append 1 $ History.singleton 0
    assertTrue $ History.hasPast history
    assertEqual
        { actual : History.current history
        , expected : 1
        }

testBack :: Effect Unit
testBack = do
    let history0 = History.singleton 0
        history1 = History.append 1 history0
        history2 = History.append 2 history1
    assertEqual
        { actual : History.back history0
        , expected : history0
        }
    assertEqual
        { actual : History.current $ History.back history1
        , expected : 0
        }
    assertEqual
        { actual : History.current $ History.back history2
        , expected : 1
        }

testForward :: Effect Unit
testForward = do
    let history0 = History.singleton 0
        history1 = History.append 1 history0
        history2 = History.append 2 history1
        history3 = History.append 3 history2
    assertEqual 
        { actual : History.forward history0
        , expected : history0
        }
    assertEqual 
        { actual : History.forward history1
        , expected : history1
        }
    assertEqual 
        { actual : History.forward history2
        , expected : history2
        }
    assertEqual 
        { actual : History.forward history3
        , expected : history3
        }
    assertEqual
        { actual : 
            history3
                # History.back
                # History.back
                # History.back
                # History.forward
                # History.forward
                # History.forward
        , expected : history3
        }

testCurrent :: Effect Unit
testCurrent = do
    assertEqual
        { actual : History.current $ History.singleton 3
        , expected : 3
        }

testHasPast :: Effect Unit
testHasPast = do
    pure unit

testHasFuture :: Effect Unit
testHasFuture = do
    pure unit