module Region where

import Prelude

import Array.Extra as Array.Extra
import Data.Array as Array
import Data.Bifunctor (bimap, rmap)
import Data.Bitraversable (bisequence)
import Data.Foldable (sum)
import Data.Function (applyN)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as Set.NonEmpty
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Class.Console (log)
import Matrix (Position)
import Matrix as Matrix
import Random.Extra as Random.Extra
import Util as Util

type Region =
    Set Position

type Bounds =
    { minRow :: Int
    , minColumn :: Int
    , maxRow :: Int
    , maxColumn :: Int
    }

minRow :: Region -> Int
minRow region =
    Set.map Tuple.fst region
        # Set.findMin 
        # Maybe.fromMaybe 0

minColumn :: Region -> Int
minColumn region =
    Set.map Tuple.snd region
        # Set.findMin 
        # Maybe.fromMaybe 0

maxRow :: Region -> Int
maxRow region =
    Set.map Tuple.fst region
        # Set.findMax 
        # Maybe.fromMaybe 0

maxColumn :: Region -> Int
maxColumn region =
    Set.map Tuple.snd region
        # Set.findMax 
        # Maybe.fromMaybe 0

getBounds :: Region -> Bounds
getBounds region =
    let 
        rows = 
            Set.map Tuple.fst region
        
        columns = 
            Set.map Tuple.snd region
    in
        { minRow : Maybe.fromMaybe 0 $ Set.findMin rows
        , minColumn : Maybe.fromMaybe 0 $ Set.findMin columns
        , maxRow : Maybe.fromMaybe 0 $ Set.findMax rows
        , maxColumn : Maybe.fromMaybe 0 $ Set.findMax columns
        }

getPositionsInBounds :: Bounds -> List Position
getPositionsInBounds bounds = 
    List.range bounds.minRow bounds.maxRow 
        # List.concatMap (\ row -> map (Tuple row) (List.range bounds.minColumn bounds.maxColumn))

rotateClockwise :: Region -> Region
rotateClockwise region =
    let 
        bounds =
            getBounds region

        rotatedBounds =
            bounds 
                { maxRow = bounds.maxRow
                , maxColumn = bounds.maxRow
                }

        move (Tuple row column) =
            Tuple 
                (rotatedBounds.minRow + column - bounds.minColumn)
                (rotatedBounds.minColumn + row - bounds.minRow)
    in 
        Set.map move region
            
layDown :: Region -> Region
layDown region =
    List.range 0 3
        # map (applyN rotateClockwise)
        # flip flap region
        # Set.fromFoldable
        # Set.findMin
        # Maybe.fromMaybe region

normalize :: Region -> Region
normalize region =
    let 
        rowOffset = 
            minRow region

        columnOffset = 
            minColumn region

        offset (Tuple row column) =
            Tuple (row - rowOffset) (column - columnOffset)
    in
        layDown $ Set.map offset region

generateRegions :: Bounds -> Int -> Int -> Effect (List Region)
generateRegions bounds numberOfRegions numberOfPositions = 
    let 
        numberOfRegions' :: Int
        numberOfRegions' =
            min numberOfRegions numberOfPositions
        
        positionsInBounds :: Set Position
        positionsInBounds = 
            Set.fromFoldable $ getPositionsInBounds bounds

        getAdjacentPositions :: Position -> Set Position
        getAdjacentPositions position =
           flap
                [ identity
                , Matrix.northWest
                , Matrix.north
                , Matrix.northEast
                , Matrix.west
                , Matrix.east
                , Matrix.southWest
                , Matrix.south
                , Matrix.southEast
                ]
                position
                # Set.fromFoldable
                # Set.intersection positionsInBounds

        getExpandablePositions :: Position -> Set Position 
        getExpandablePositions position =
            flap
                [ Matrix.north
                , Matrix.west
                , Matrix.east
                , Matrix.south
                ]
                position
                # Set.fromFoldable
                # Set.intersection positionsInBounds

        getExpandablePositionsForRegion :: Region -> Set Position
        getExpandablePositionsForRegion region =
            Set.map getExpandablePositions region 
                # Set.unions
                # flip Set.difference region

        selectStartPositions :: Set Position -> Set Position -> List Position -> Set Position
        selectStartPositions selectedPositions excludedPositions candidatePositions =
            if Set.size selectedPositions < numberOfRegions' then 
                case List.uncons candidatePositions of 
                    Just { head, tail } -> 
                        let 
                            adjacentPositions = 
                                getAdjacentPositions head
                        in 
                            if Array.any (flip Set.member excludedPositions) adjacentPositions then 
                                selectStartPositions selectedPositions excludedPositions tail
                            else 
                                let
                                    selectedPositions' = 
                                        Set.insert head selectedPositions
                                    
                                    excludedPositions' = 
                                        Set.fromFoldable adjacentPositions
                                            # Set.union excludedPositions
                                in
                                    selectStartPositions selectedPositions' excludedPositions' tail

                    Nothing -> 
                        selectedPositions
            else 
                selectedPositions

        getRegionExpansionsForRegion :: Array Region -> Region -> Tuple Region (Set Position)
        getRegionExpansionsForRegion regions region =
            let 
                nonExpandablePositions =
                    Set.unions regions
                        # flip Set.difference region
                        # Set.map getAdjacentPositions
                        # Set.unions
                
                potentialExpandablePositions = 
                    getExpandablePositionsForRegion region
            in 
                Set.difference potentialExpandablePositions nonExpandablePositions
                    # Tuple region

        getRegionExpansions :: Array Region -> Array (Tuple Region (NonEmptySet Position))
        getRegionExpansions regions =
            map (getRegionExpansionsForRegion regions) regions
                <#> bimap Just Set.NonEmpty.fromFoldable
                # Array.mapMaybe bisequence

        expandRegions :: Array Region -> Effect (Array Region)
        expandRegions regions =
            let 
                currentNumberOfPositions = 
                    map Set.size regions
                        # sum
            in 
                if currentNumberOfPositions >= numberOfPositions then 
                    pure regions
                else 
                    do
                        shuffledRegions :: Array (Tuple Int (Tuple Region (NonEmptySet Position))) <- 
                            Array.mapWithIndex Tuple regions
                                <#> rmap (getRegionExpansionsForRegion regions)
                                <#> rmap (bimap Just Set.NonEmpty.fromFoldable)
                                <#> bimap Just bisequence
                                # Array.mapMaybe bisequence
                                # Random.Extra.shuffle

                        case Array.head shuffledRegions of 
                            Just (Tuple index (Tuple region positions)) ->
                                do 
                                    position <- 
                                        Random.Extra.chooseNonEmpty positions 
                                    let region' = Set.insert position region
                                    expandRegions $ Array.Extra.updateAt_ index region' regions
                            Nothing -> 
                                pure regions
    in do
        startPositions :: Set Position <- 
            Array.fromFoldable positionsInBounds
                # Util.shuffle 
                <#> List.fromFoldable
                <#> selectStartPositions Set.empty Set.empty

        log $ "startPositions: " <> show startPositions

        let 
            startRegions :: Array Region
            startRegions = 
                Set.map Set.singleton startPositions
                    # Array.fromFoldable

        expandRegions startRegions
            <#> List.fromFoldable