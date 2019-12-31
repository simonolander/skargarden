module Board where

import Prelude

import Data.Array (all, any)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Class.Console (log)
import History (History)
import History as History
import Matrix (Matrix, Position, Line(..))
import Matrix as Matrix
import Progress (Progress)
import Rectangle as Rectangle
import Region (Region(..), isBoat)
import Region as Region
import Util as Util

type Regions = Matrix Region

type Board = 
    { boats :: Matrix Boolean
    , regions :: History Regions
    , disabled :: Set Position
    }

updateRegions :: (Regions -> Regions) -> Board -> Board
updateRegions fn board =
    board 
        { regions = 
            History.current board.regions
                # fn 
                # flip History.append board.regions
        }

empty :: Int -> Board
empty size = 
    { boats : Matrix.init size size false
    , regions : History.singleton $ Matrix.init size size Unknown
    , disabled : Set.empty
    }

initialize :: Board -> Effect Board
initialize board = 
    let 
        numberOfRows = 
            Matrix.getNumberOfRows board.boats

        maxRow = numberOfRows - 1

        numberOfColumns = 
            Matrix.getNumberOfColumns board.boats

        maxColumn = numberOfColumns - 1

        numberOfHints = 
            2 * (numberOfColumns + numberOfRows)

        boatSizes = 
            [7, 6, 5, 4, 3, 3, 2, 2, 2]

        getPotentialPositions :: Int -> Effect (Array (Tuple Position Boolean))
        getPotentialPositions size =
            do 
                let
                    vp :: Array (Tuple Position Boolean)
                    vp = 
                        let 
                            rs = 
                                Array.range 0 maxRow
                                    # Array.filter ((<=) 0)
                            cs = 
                                Array.range 0 (maxColumn - size + 1) 
                                    # Array.filter ((<=) 0)
                        in
                            Util.pairs rs cs
                                # map (flip Tuple.Tuple true)
                    
                    hp :: Array (Tuple Position Boolean)
                    hp = 
                        let 
                            rs = 
                                Array.range 0 (maxRow - size + 1)
                                    # Array.filter ((<=) 0)
                            cs = 
                                Array.range 0 maxColumn
                                    # Array.filter ((<=) 0)
                        in
                            Util.pairs rs cs
                                # map (flip Tuple.Tuple false)
                Util.shuffle $ Array.concat [vp, hp]

        expandPosition :: Int -> Tuple Position Boolean -> Array Position
        expandPosition size (Tuple (Tuple rowIndex colIndex) vertical) = 
            if vertical then 
                Array.range colIndex (colIndex + size - 1)
                    # map (flip Tuple.Tuple rowIndex)
            else 
                Array.range rowIndex (rowIndex + size - 1)
                    # map (Tuple.Tuple colIndex)

        isPositionPlacable :: Position -> Matrix Boolean -> Boolean
        isPositionPlacable position sea =
            let 
                center = 
                    Matrix.get position sea
                        # Maybe.fromMaybe true

                northWest = 
                    Matrix.northWest position
                        # flip Matrix.get sea
                        # Maybe.fromMaybe false

                north = 
                    Matrix.north position
                        # flip Matrix.get sea
                        # Maybe.fromMaybe false

                northEast = 
                    Matrix.northEast position
                        # flip Matrix.get sea
                        # Maybe.fromMaybe false

                east = 
                    Matrix.east position
                        # flip Matrix.get sea
                        # Maybe.fromMaybe false

                west = 
                    Matrix.west position
                        # flip Matrix.get sea
                        # Maybe.fromMaybe false

                southWest = 
                    Matrix.southWest position
                        # flip Matrix.get sea
                        # Maybe.fromMaybe false

                south = 
                    Matrix.south position
                        # flip Matrix.get sea
                        # Maybe.fromMaybe false

                southEast = 
                    Matrix.southEast position
                        # flip Matrix.get sea
                        # Maybe.fromMaybe false
            in
                not $ any identity
                        [ center
                        , northWest
                        , north
                        , northEast
                        , west
                        , east
                        , southWest
                        , south
                        , southEast
                        ]

        isPlacable :: Array Position -> Matrix Boolean -> Boolean
        isPlacable positions sea =
            all (flip isPositionPlacable sea) positions

        placePositions :: Array Position -> Matrix Boolean -> Matrix Boolean
        placePositions positions sea =
            Matrix.setMultiple true positions sea

        placeBoatOnPotentialPositions :: Int -> Array (Tuple Position Boolean) -> Matrix Boolean -> Tuple (Matrix Boolean) Boolean
        placeBoatOnPotentialPositions size potentialPositions sea =
            case Array.uncons potentialPositions of 
                Just {head, tail} -> 
                    let 
                        positions = 
                            expandPosition size head
                    in 
                        if isPlacable positions sea then 
                            Tuple.Tuple (placePositions positions sea) true
                        else 
                            placeBoatOnPotentialPositions size tail sea
                Nothing -> 
                    Tuple.Tuple sea false 

        placeBoat :: Int -> Matrix Boolean -> Effect (Tuple (Matrix Boolean) Boolean)
        placeBoat size sea =
            do
                potentialPositions <- getPotentialPositions size
                pure $ placeBoatOnPotentialPositions size potentialPositions sea

        placeBoats :: Array Int -> Effect (Matrix Boolean)
        placeBoats sizes =
            do
                let 
                    sea = Matrix.init numberOfRows numberOfColumns false

                    foldlPlaceBoat :: Matrix Boolean -> Int -> Effect (Matrix Boolean)
                    foldlPlaceBoat sea size =
                        do 
                            result <- placeBoat size sea
                            if Tuple.snd result then 
                                pure $ Tuple.fst result
                            else 
                                pure sea 
                foldM foldlPlaceBoat sea sizes
    in
    do
        boats <- placeBoats boatSizes
        hintedIndices <- map Set.fromFoldable $ Util.choose numberOfHints $ Matrix.getIndexes boats
        let 
            hint index boat =
                if Set.member index hintedIndices then 
                    if boat then 
                        Boat
                    else 
                        Water
                else
                    Unknown

            regions = 
                mapWithIndex hint boats
                    # History.singleton
        pure { boats : boats, regions : regions, disabled : hintedIndices }

getProgress :: Board -> Progress
getProgress board =
    let 
        rowProgress = 
            Array.zip 
                (History.current board.regions # Matrix.toRows # map (Util.countIf isBoat))
                (Matrix.toRows board.boats # map (Util.countIf identity)) 

        columnProgress = 
            Array.zip 
                (History.current board.regions # Matrix.toColumns # map (Util.countIf isBoat))
                (Matrix.toColumns board.boats # map (Util.countIf identity)) 

        boatProgress = 
            let 
                regionSizes = 
                    History.current board.regions
                        # Matrix.findRectangles Region.isBoat
                        # map Rectangle.size
                        # map Rectangle.layDown
                        # Util.count
                        # Map.fromFoldable

                boatSizes = 
                    Matrix.findRectangles identity board.boats
                        # map Rectangle.size
                        # map Rectangle.layDown
                        # Util.count
                        # Map.fromFoldable

                bothSizes =
                    Set.union (Map.keys regionSizes) (Map.keys boatSizes)
                        # Array.fromFoldable
                        # Array.sortWith Rectangle.area
                        # Array.reverse
                        # map 
                            \ key ->
                                Tuple key 
                                    $ Tuple 
                                        (Map.lookup key regionSizes # fromMaybe 0)
                                        (Map.lookup key boatSizes # fromMaybe 0)
            in 
                bothSizes
    in 
        { rowProgress, columnProgress, boatProgress }

rotateRegion :: Int -> Int -> Board -> Board
rotateRegion rowIndex columnIndex board =
    updateRegions (Matrix.update Region.rotate (Tuple.Tuple rowIndex columnIndex)) board

setUnknownRegions :: Region -> Board -> Board
setUnknownRegions region board = 
    updateRegions (map (\ r -> if r == Unknown then region else r)) board

clearEnabledRegions :: Board -> Board
clearEnabledRegions board = 
    let update =
            mapWithIndex
                \ position region -> 
                    if Set.member position board.disabled then 
                        region
                    else 
                        Unknown
    in 
    updateRegions update board

canFillLine :: Boolean -> Line -> Board -> Boolean
canFillLine fillWater line {boats, regions} =
    fromMaybe false $ do 
        nBoats <- 
            Matrix.getLine line boats
                # map (Util.countIf $ if fillWater then identity else not)
        regionLine <- Matrix.getLine line $ History.current regions
        let 
            nRegions =
                Util.countIf (if fillWater then Region.isBoat else Region.isWater) regionLine
            hasUnknowns =
                Array.any Region.isUnknown regionLine
        pure $ nBoats == nRegions && hasUnknowns

fillLine :: Line -> Board -> Board
fillLine line board =
    if canFillLine false line board then 
        let update = 
                Matrix.updateLine 
                    (\ region -> if Region.isUnknown region then Boat else region)
                    line
        in 
            updateRegions update board
    else if canFillLine true line board then 
        let update =
                Matrix.updateLine 
                    (\ region -> if Region.isUnknown region then Water else region)
                    line
        in
            updateRegions update board
    else 
        board

undo :: Board -> Board
undo board =
    board { regions = History.back board.regions }