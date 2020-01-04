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
import Region (maxColumn)
import Region as Region
import TerrainType (TerrainType(..), isLand)
import TerrainType as TerrainType
import Util as Util

type TerrainTypes = Matrix TerrainType

type Board = 
    { boats :: Matrix Boolean
    , regions :: History TerrainTypes
    , disabled :: Set Position
    }

updateTerrainTypes :: (TerrainTypes -> TerrainTypes) -> Board -> Board
updateTerrainTypes fn board =
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

        numberOfRegions = 
            (numberOfRows + numberOfColumns) / 2

        numberOfPositions = 
            (numberOfRows * numberOfColumns) / 3
    in
    do
        regions <- 
            Region.generateRegions
                { minRow : 0, minColumn : 0, maxRow, maxColumn }
                numberOfRegions
                numberOfPositions

        let 
            islandPositions = 
                Set.unions regions

            boats =
                Matrix.init numberOfRows numberOfColumns false
                    # mapWithIndex (\ index _ -> Set.member index islandPositions)

        hintedIndices <- map Set.fromFoldable $ Util.choose numberOfHints $ Matrix.getIndexes boats
        let 
            hint index boat =
                if Set.member index hintedIndices then 
                    if boat then 
                        Land
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
                (History.current board.regions # Matrix.toRows # map (Util.countIf isLand))
                (Matrix.toRows board.boats # map (Util.countIf identity)) 

        columnProgress = 
            Array.zip 
                (History.current board.regions # Matrix.toColumns # map (Util.countIf isLand))
                (Matrix.toColumns board.boats # map (Util.countIf identity)) 

        islandProgress = 
            let 
                currentIslands = 
                    History.current board.regions
                        # Matrix.findRegions TerrainType.isLand
                        <#> Region.normalize
                        # Array.fromFoldable
                        # Util.count
                        # Map.fromFoldable

                sougthIslands = 
                    Matrix.findRegions identity board.boats
                        <#> Region.normalize
                        # Array.fromFoldable
                        # Util.count
                        # Map.fromFoldable

                bothSizes =
                    Set.union (Map.keys currentIslands) (Map.keys sougthIslands)
                        # Array.fromFoldable
                        # Array.sortWith Set.size
                        # Array.reverse
                        # map 
                            \ key ->
                                Tuple key 
                                    $ Tuple 
                                        (Map.lookup key currentIslands # fromMaybe 0)
                                        (Map.lookup key sougthIslands # fromMaybe 0)
            in 
                bothSizes

        unknownTerrainTypes = 
            History.current board.regions
                # Matrix.toIndexedArray
                # map Tuple.snd
                # Util.countIf TerrainType.isUnknown
    in 
        { rowProgress
        , columnProgress
        , islandProgress
        , unknownTerrainTypes
        }

rotateTerrainType :: Int -> Int -> Board -> Board
rotateTerrainType rowIndex columnIndex board =
    updateTerrainTypes (Matrix.update TerrainType.rotate (Tuple.Tuple rowIndex columnIndex)) board

setUnknownTerrainTypes :: TerrainType -> Board -> Board
setUnknownTerrainTypes region board = 
    updateTerrainTypes (map (\ r -> if r == Unknown then region else r)) board

clearEnabledTerrainTypes :: Board -> Board
clearEnabledTerrainTypes board = 
    let update =
            mapWithIndex
                \ position region -> 
                    if Set.member position board.disabled then 
                        region
                    else 
                        Unknown
    in 
    updateTerrainTypes update board

canFillLine :: Boolean -> Line -> Board -> Boolean
canFillLine fillWater line {boats, regions} =
    fromMaybe false $ do 
        nLands <- 
            Matrix.getLine line boats
                # map (Util.countIf $ if fillWater then identity else not)
        regionLine <- Matrix.getLine line $ History.current regions
        let 
            nTerrainTypes =
                Util.countIf (if fillWater then TerrainType.isLand else TerrainType.isWater) regionLine
            hasUnknowns =
                Array.any TerrainType.isUnknown regionLine
        pure $ nLands == nTerrainTypes && hasUnknowns

fillLine :: Line -> Board -> Board
fillLine line board =
    if canFillLine false line board then 
        let update = 
                Matrix.updateLine 
                    (\ region -> if TerrainType.isUnknown region then Land else region)
                    line
        in 
            updateTerrainTypes update board
    else if canFillLine true line board then 
        let update =
                Matrix.updateLine 
                    (\ region -> if TerrainType.isUnknown region then Water else region)
                    line
        in
            updateTerrainTypes update board
    else 
        board

undo :: Board -> Board
undo board =
    board { regions = History.back board.regions }