module Matrix where

import Prelude

import CSS (position)
import Data.Array (all)
import Data.Array as Array
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Rectangle (Rectangle)

data Matrix a = Matrix (Array (Array a))

type Position = Tuple Int Int

data Line = Row Int | Column Int

instance functorWithIndexMatrix :: FunctorWithIndex (Tuple Int Int) Matrix where
    mapWithIndex fn matrix = 
        toRows matrix 
            # Array.mapWithIndex 
                (\ rowIndex -> 
                    Array.mapWithIndex 
                        \ colIndex cell -> 
                            fn (Tuple rowIndex colIndex) cell
                )
            # Matrix

instance showMatrix :: Show a => Show (Matrix a) where
    show = toRows >>> show

instance functorMatrix :: Functor Matrix where
    map fn m = 
        toRows m 
            # map (map fn)
            # Matrix

init :: forall a. Int -> Int -> a -> Matrix a
init numberOfRows numberOfColumns a = 
    Matrix $ Array.replicate numberOfRows (Array.replicate numberOfColumns a)

generate :: forall a. Int -> Int -> Effect a -> Effect (Matrix a)
generate numberOfRows numberOfColumns e =
    do 
        rows :: Array (Array a) <- replicateA numberOfRows $ replicateA numberOfColumns e
        pure (Matrix rows)

get :: forall a. Position -> Matrix a -> Maybe a
get (Tuple r c) matrix = 
  do
    let rows = toRows matrix
    row <- Array.index rows r
    Array.index row c

getRow :: forall a. Int -> Matrix a -> Maybe (Array a)
getRow index matrix = 
    toRows matrix 
        # flip Array.index index

getColumn :: forall a. Int -> Matrix a -> Maybe (Array a)
getColumn index matrix =
    toColumns matrix 
        # flip Array.index index

update :: forall a. (a -> a) -> Position -> Matrix a -> Matrix a
update f (Tuple r c) matrix =
    toRows matrix
        # Array.modifyAtIndices [r] (Array.modifyAtIndices [c] f)
        # Matrix

updateRow :: forall a. (a -> a) -> Int -> Matrix a -> Matrix a
updateRow fn index matrix = 
    toRows matrix 
        # Array.modifyAt index (map fn)
        # map Matrix
        # fromMaybe matrix

updateColumn :: forall a. (a -> a) -> Int -> Matrix a -> Matrix a
updateColumn fn index matrix =
    transpose matrix 
        # updateRow fn index
        # transpose

updateLine :: forall a. (a -> a) -> Line -> Matrix a -> Matrix a
updateLine fn (Row index) = updateRow fn index
updateLine fn (Column index) = updateColumn fn index

getLine :: forall a. Line -> Matrix a -> Maybe (Array a)
getLine (Row index) matrix = flip Array.index index $ toRows matrix
getLine (Column index) matrix = getLine (Row index) (transpose matrix)

set :: forall a. a -> Position -> Matrix a -> Matrix a
set a index matrix = 
    update (const a) index matrix

setMultiple :: forall a. a -> Array Position -> Matrix a -> Matrix a
setMultiple a positions matrix =
    Array.foldl (flip $ set a) matrix positions

toIndexedArray :: forall a. Matrix a -> Array (Tuple Position a)
toIndexedArray matrix = 
    toRows matrix
        # map (Array.mapWithIndex Tuple)
        # Array.mapWithIndex 
            (\ index row -> 
                flip map row
                    \ (Tuple col val) -> Tuple (Tuple index col) val)
        # Array.concat

toRows :: forall a. Matrix a -> Array (Array a)
toRows (Matrix matrix) = matrix

transpose :: forall a. Matrix a -> Matrix a
transpose matrix = 
    let 
        transpose' :: forall b. Array (Array b) -> Array (Array b)
        transpose' rows = 
            case Array.uncons rows of
                Nothing -> []
                Just { head: l', tail: xss } -> 
                    case Array.uncons l' of
                        Nothing ->
                            transpose' xss
                        Just { head: x, tail: xs } ->
                            (x `Array.cons` Array.mapMaybe Array.head xss) `Array.cons` transpose' (xs `Array.cons` Array.mapMaybe Array.tail xss)
    in
    Matrix (transpose' $ toRows matrix)

toColumns :: forall a. Matrix a -> Array (Array a)
toColumns matrix = 
    toRows $ transpose matrix

getNumberOfColumns :: forall a. Matrix a -> Int
getNumberOfColumns matrix =
    case Array.head $ toRows matrix of 
        Just row -> Array.length row
        Nothing -> 0

getNumberOfRows :: forall a. Matrix a -> Int
getNumberOfRows matrix = 
    Array.length $ toRows matrix

getIndexes :: forall a. Matrix a -> Array Position
getIndexes matrix = 
    toIndexedArray matrix
        # map Tuple.fst

northWest :: Position -> Position 
northWest (Tuple r c) = 
    Tuple (r - 1) (c - 1)

north :: Position -> Position 
north (Tuple r c) = 
    Tuple (r - 1) c

northEast :: Position -> Position 
northEast (Tuple r c) = 
    Tuple (r - 1) (c + 1)

west :: Position -> Position 
west (Tuple r c) = 
    Tuple r (c - 1)

east :: Position -> Position 
east (Tuple r c) = 
    Tuple r (c + 1)

south :: Position -> Position 
south (Tuple r c) = 
    Tuple (r + 1) c

southWest :: Position -> Position 
southWest (Tuple r c) = 
    Tuple (r + 1) (c - 1)

southEast :: Position -> Position 
southEast (Tuple r c) = 
    Tuple (r + 1) (c + 1)

findRectangles :: forall a. (a -> Boolean) -> Matrix a -> Array Rectangle
findRectangles fn matrix = 
    let
        numberOfRows = getNumberOfRows matrix
        numberOfColumns = getNumberOfColumns matrix

        f :: Position -> Boolean
        f = flip get matrix >>> map fn >>> fromMaybe false

        s :: Matrix Boolean -> Position -> Boolean 
        s matrix pos= 
            get pos matrix # fromMaybe true
    
        out :: Position -> Boolean
        out (Tuple r c) = r >= numberOfRows || c >= numberOfColumns

        findRectangles' :: Tuple (Array Rectangle) (Matrix Boolean) -> Position -> Tuple (Array Rectangle) (Matrix Boolean)
        findRectangles' t@(Tuple rects searched) p@(Tuple r c) = 
            if r >= numberOfRows then
                t
            else if c >= numberOfColumns then 
                findRectangles' t (Tuple (r + 1) 0)
            else if s searched p || not (f p) then 
                findRectangles' t (east p)
            else 
                findRectangles' (findRectangles'' t p p) (east p)

        findRectangles'' :: Tuple (Array Rectangle) (Matrix Boolean) -> Position -> Position -> Tuple (Array Rectangle) (Matrix Boolean)
        findRectangles'' (Tuple rects searched) p0@(Tuple r0 c0) p1@(Tuple r1 c1) =
            let 
                southEast' = southEast p1
                easts = 
                    Array.range r0 r1 
                    # map (flip Tuple (c1 + 1)) 
                souths = 
                    Array.range c0 c1 
                    # map (Tuple (r1 + 1))
                southEasts = Array.concat [ easts, souths, [southEast'] ]
            in 
                if all f southEasts then 
                    let 
                        searched' = setMultiple true southEasts searched
                    in
                        findRectangles'' (Tuple rects searched') p0 southEast'
                else if all f easts then 
                    let 
                        searched' = setMultiple true easts searched
                    in
                        findRectangles'' (Tuple rects searched') p0 (east p1)  
                else if all f souths then 
                    let 
                        searched' = setMultiple true souths searched
                    in
                        findRectangles'' (Tuple rects searched') p0 (south p1)
                else 
                    Tuple (Array.cons { r0, c0, r1, c1 } rects) searched
    in 
        findRectangles' (Tuple [] (init numberOfRows numberOfColumns false)) (Tuple 0 0) 
            # Tuple.fst

findTerrainTypes :: forall a. (a -> Boolean) -> Matrix a -> List (Set Position)
findTerrainTypes fn matrix =
    let
        findTerrainType :: Set Position -> Set Position -> Set Position -> Set Position
        findTerrainType includedPositions queue candidatePositions = 
            case Set.findMin queue of 
                Just position -> 
                    let
                        adjacentPositions = 
                            flap [ west, north, east, south ] position
                                # Array.filter (not <<< flip Set.member queue)
                                # Array.filter (not <<< flip Set.member includedPositions)
                                # Array.filter (flip Set.member candidatePositions)
                                # Set.fromFoldable
                        
                        queue' = 
                            Set.delete position queue
                                # Set.union adjacentPositions

                        includedPositions' = 
                            Set.insert position includedPositions
                    in
                        findTerrainType includedPositions' queue' candidatePositions
                Nothing -> 
                    includedPositions

        findTerrainTypesR :: Set Position -> List (Set Position)
        findTerrainTypesR candidatePositions = 
            case Set.findMin candidatePositions of 
                Just candidatePosition -> 
                    let 
                        region = 
                            findTerrainType Set.empty (Set.singleton candidatePosition) candidatePositions

                        candidatePositions' = 
                            Set.difference candidatePositions region
                    in 
                        region : findTerrainTypesR candidatePositions'

                Nothing -> 
                    Nil

    in
        toIndexedArray matrix
            # Array.filter (Tuple.snd >>> fn)
            # map Tuple.fst
            # Set.fromFoldable
            # findTerrainTypesR
