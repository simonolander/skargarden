module Rectangle where

import Prelude

type Size = 
    { height :: Int
    , width :: Int
    }

type Rectangle = 
    { r0 :: Int
    , c0 :: Int
    , r1 :: Int
    , c1 :: Int
    }
    
size :: Rectangle -> Size
size { r0, c0, r1, c1 } = 
    { height : r1 - r0 + 1
    , width : c1 - c0 + 1
    }

area :: Size -> Int
area { width, height } = 
    width * height

layDown :: Size -> Size
layDown size@{ width, height } =
    if width < height then 
        { width : height, height : width }
    else 
        size