module Hello where

import Prelude

import Board (Board, rotateTerrainType)
import Board as Board
import CSS (TextDecoration(..), absolute, alignItems, alignSelf, backgroundColor, black, blanchedalmond, border, borderBottom, borderLeft, borderRadius, borderRight, borderTop, bottom, burlywood, color, column, display, dotted, flex, flexDirection, fontFamily, fontSize, height, hover, justifyContent, lineHeight, margin, marginBottom, marginLeft, marginTop, maxHeight, maxWidth, minHeight, minWidth, noneTextDecoration, padding, paddingBottom, paddingLeft, paddingRight, paddingTop, pct, position, pt, px, rgb, ridge, right, row, solid, star, textDecoration, white, whitesmoke, width, with, zIndex, (?))
import CSS as Grid
import CSS.Common (center, none)
import CSS.Flexbox as Flexbox
import CSS.Overflow (overflow, overflowY, scroll)
import CSS.TextAlign (textAlign)
import CSS.TextAlign as TextAlign
import Data.Array (any)
import Data.Array as Array
import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Grid (gridColumn, gridColumnN, gridRow, gridRowN, justifySelf)
import Halogen as H
import Halogen.HTML (ClassName(..), HTML(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_, classes)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (disabled)
import Halogen.Query.EventSource as ES
import History as History
import Matrix (Line(..), Matrix(..))
import Matrix as Matrix
import Progress (Progress)
import Progress as Progress
import Rectangle (Rectangle)
import Rectangle as Rectangle
import TerrainType (TerrainType(..))
import TerrainType as TerrainType
import Util as Util
import Version as Version
import ViewPort (ViewPort)
import Web.Event.Event (Event, EventType(..))
import Web.HTML (History, Window)
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLTableRowElement (rowIndex)
import Web.HTML.Window as Web
import Web.HTML.Window as Window

type Input = ViewPort
type Message = Void
type Query = Const Void
type NoChildSlots = ()
type MonadType = Aff

type State = 
  { board :: Board 
  , viewPort :: ViewPort
  }

updateBoard :: (Board -> Board) -> State -> State
updateBoard f state = 
  state { board = f state.board }

data Action 
  = Toggle Int Int
  | Initialize
  | Resize Event
  | ClickedClear
  | ClickedHeader Line
  | ClickedUndo

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: Input -> State
initialState viewPort = 
  { board : Board.empty 10, viewPort }

render :: State -> H.ComponentHTML Action NoChildSlots Aff
render state =
  let
    spacing = 
      10.0

    landscape = 
      state.viewPort.width > state.viewPort.height

    progress = 
      Board.getProgress state.board

    solved = 
      Progress.isSolved progress

    maxBoardWidth =
      (Int.toNumber state.viewPort.width) - 2.0 * spacing

    maxBoardHeight =
      (Int.toNumber state.viewPort.height) - 2.0 * spacing

    { boardView, boardWidth, boardHeight } =
      renderBoard
        { maxWidth : maxBoardWidth
        , maxHeight : maxBoardHeight
        }
        state.board

    renderLandLegend :: forall w. Board -> HTML w Action
    renderLandLegend board =
      let 
        renderLandRow (Tuple size (Tuple currentNumberOfLands soughtNumberOfLands)) = 
          let 
            correct = 
              currentNumberOfLands == soughtNumberOfLands

            tooMany = 
              currentNumberOfLands > soughtNumberOfLands

            minSize = 
              20.0
            
            boatView = 
              HH.div 
                [ classes
                    $ map ClassName
                    $ Array.catMaybes
                        [ Just "small-boat"
                        , if correct then 
                            Just "correct" 
                          else if tooMany then 
                            Just "too-many" 
                          else 
                            Nothing
                        ]
                , CSS.style $ do 
                    borderLeft solid (px 1.0) black
                    borderTop solid (px 1.0) black
                ]
                ( Array.replicate size.height
                    $ HH.div 
                        [ CSS.style $ do 
                            display flex
                            flexDirection row
                        ]
                    $ Array.replicate size.width
                    $ HH.div 
                      [ CSS.style $ do 
                          borderRight solid (px 1.0) black
                          borderBottom solid (px 1.0) black
                          minWidth $ px minSize
                          minHeight $ px minSize
                      ] 
                      []
                )
            
            countView = 
              HH.div 
                [ classes
                    $ map ClassName
                    $ Array.catMaybes
                        [ Just "small-boat-count"
                        , if correct then 
                            Just "correct" 
                          else if tooMany then 
                            Just "too-many" 
                          else 
                            Nothing 
                        ]
                , CSS.style do 
                    marginLeft $ px $ spacing * 2.0
                    marginLeft $ px $ spacing / 2.0
                    lineHeight $ px $ Int.toNumber size.height * minSize
                ]
                [ HH.span 
                    []
                    [ HH.text $ show currentNumberOfLands ]
                , HH.span 
                    []
                    [ HH.text $ "/" ]
                , HH.span 
                    []
                    [ HH.text $ show soughtNumberOfLands ]
                ]
          in
            HH.div
              [ class_ $ ClassName "boat-legend-row"
              , CSS.style $ do
                  display flex
                  flexDirection row
                  marginBottom $ px $ spacing / 2.0
              ]
              [ boatView, countView ]
          
      in 
        map renderLandRow progress.boatProgress
          # HH.div
              [ class_ $ ClassName "boat-legend"
              , CSS.style $ do 
                  display flex
                  flexDirection column
                  paddingLeft $ px $ spacing / 2.0
                  paddingTop $ px $ spacing / 2.0
                  paddingRight $ px $ spacing / 2.0
                  if landscape then do 
                    overflowY scroll
                    pure unit
                  else do 
                    maxHeight $ px $ Int.toNumber state.viewPort.height - spacing * 3.0 - boardHeight
                    overflowY scroll
                    marginTop $ px $ spacing
              ]

    boatLegentView = 
      renderLandLegend state.board

    buttons =
      let 
        buttonStyle = 
          CSS.style $ do 
            display flex
            padding (px 10.0) (px 10.0) (px 10.0) (px 10.0)
            fontSize $ px 18.0
            justifyContent center
            marginTop $ px spacing
            borderRadius (px 5.0) (px 5.0) (px 5.0) (px 5.0)
            if landscape then do 
              marginTop $ px spacing
            else do 
              width $ pct 100.0

        newGameButton = 
          HH.button 
            [ buttonStyle
            , HE.onClick $ const $ Just Initialize
            ]
            [ HH.text "New game" ]

        clearButton =
          HH.button
            [ buttonStyle
            , HE.onClick $ const $ Just ClickedClear
            ]
            [ HH.text "Clear" ] 

        undoButton =
          HH.button
            [ buttonStyle
            , HE.onClick $ const $ Just ClickedUndo
            ]
            [ HH.text "Undo" ] 
      in
      [ if solved then 
          [ newGameButton ]
        else 
          [ clearButton
          , undoButton
          ]
      ]
        # Array.concat
        # HH.div
            [ CSS.style $ do 
                display flex
                flexDirection column
                if landscape then do
                  pure unit
                else do 
                  marginLeft $ px spacing
                  Flexbox.flex 1 1 (pct 0.0)
            ]

    versionView = 
      HH.a
        [ class_ $ ClassName "version"
        , CSS.style $ do 
            position absolute
            right $ px 0.0
            bottom $ px 0.0
            padding (px spacing) (px spacing) (px spacing) (px spacing)
        , HP.href "https://github.com/simonolander/skargarden"
        ]
        [ HH.text Version.version
        ]
  in
    HH.div
      [ CSS.style $ do 
          display flex
          padding (px spacing) (px spacing) (px spacing) (px spacing)
          justifyContent center
          alignItems center
          height $ px $ Int.toNumber state.viewPort.height - spacing * 2.0
      ]
      [ versionView
      , HH.div 
          [ CSS.style $ do 
              display flex
              flexDirection $
                if landscape then 
                  row
                else
                  column
          ]
          [ boardView
          , HH.div
              [ CSS.style $ do
                  display flex
                  if landscape then do 
                    flexDirection column
                    marginLeft (px spacing)
                    overflowY scroll
                    maxHeight $ px boardHeight
                  else do 
                    flexDirection row
                    pure unit
              ]
              [ boatLegentView, buttons ]
          ]
      ]

renderBoard :: forall w. { maxHeight :: Number, maxWidth :: Number } -> Board -> { boardHeight :: Number, boardView :: HTML w Action, boardWidth :: Number }
renderBoard { maxWidth, maxHeight } board =
  let
    minCellSize = 20.0
    maxCellSize = 60.0
    headerSizeInUnits = 1.0

    regions = 
      History.current board.regions

    numberOfRows = 
      Matrix.getNumberOfRows regions

    numberOfColumns = 
      Matrix.getNumberOfColumns regions

    maxWantedWidth = maxCellSize * (headerSizeInUnits + Int.toNumber numberOfColumns)

    maxWantedHeight = maxCellSize * (headerSizeInUnits + Int.toNumber numberOfRows)

    progress = Board.getProgress board

    solved = Progress.isSolved progress

    widthByHeight =
      let
        widthInUnits = headerSizeInUnits + Int.toNumber numberOfColumns
        heightInUnits = headerSizeInUnits + Int.toNumber numberOfRows
      in
        widthInUnits / heightInUnits
    
    { widthInPx, heightInPx, pxInUnit } =
      let 
        maxWidthByHeight = 
          maxWidth / maxHeight
      in
        if maxWidthByHeight < widthByHeight then 
          let 
            widthInPx = 
              min maxWidth maxWantedWidth

            heightInPx = 
              widthInPx / widthByHeight

            pxInUnit = 
              widthInPx / (headerSizeInUnits + Int.toNumber numberOfColumns)
          in 
          { widthInPx, heightInPx, pxInUnit }
        else
          let 
            heightInPx = 
              min maxHeight maxWantedHeight
            
            widthInPx = 
              heightInPx * widthByHeight

            pxInUnit = 
              widthInPx / (headerSizeInUnits + Int.toNumber numberOfColumns)
          in 
          { widthInPx, heightInPx, pxInUnit }

    units = 
      (*) pxInUnit >>> px

    columnHeadersRow = 
      let 
        corner = 
          HH.div 
            [ CSS.style $ do
                width $ units headerSizeInUnits
                height $ units headerSizeInUnits
            ]
            []
        
        columnHeaders = 
          let
            renderColumnHeader index columnTerrainTypes = 
              let 
                currentNumberOfUnknowns =
                  Util.countIf TerrainType.isUnknown columnTerrainTypes

                hasUnknowns = 
                  currentNumberOfUnknowns > 0

                Tuple currentNumberOfLands soughtNumberOfLands = 
                  Array.index progress.columnProgress index
                    # fromMaybe (Tuple 0 0)
                
                line =
                  Column index

                finished = 
                  not hasUnknowns && currentNumberOfLands == soughtNumberOfLands

                canFill = 
                  hasUnknowns 
                    && Board.canFillLine false line board 
                    || Board.canFillLine true line board

                tooMany = 
                  currentNumberOfLands > soughtNumberOfLands

                tooFew = 
                  currentNumberOfUnknowns + currentNumberOfLands < soughtNumberOfLands
              in 
                HH.div 
                  [ classes 
                      $ map ClassName
                      $ Array.catMaybes
                          [ Just "header"
                          , if finished then 
                              Just "finished" 
                            else if canFill then 
                              Just "can-fill" 
                            else if tooMany then 
                              Just "too-many" 
                            else if tooFew then 
                              Just "too-few" 
                            else 
                              Nothing
                          ]
                  , CSS.style $ do
                      width $ units 1.0
                      height $ units headerSizeInUnits
                      lineHeight $ units headerSizeInUnits
                  , HE.onClick 
                      $ const 
                      $ if canFill then 
                          Just $ ClickedHeader line 
                        else 
                          Nothing
                  ]
                  [ HH.text $ show soughtNumberOfLands ]
          in
            Matrix.toColumns regions
              # mapWithIndex renderColumnHeader
      in 
        HH.div
          [ CSS.style $ do 
              width $ px widthInPx
              height $ units headerSizeInUnits
              display flex
              flexDirection row
          ]
          $ Array.cons corner columnHeaders

    rows = 
      let
        renderRow index rowTerrainTypes =
          let
            rowHeader = 
              let 
                currentNumberOfUnknowns = 
                  Util.countIf TerrainType.isUnknown rowTerrainTypes

                hasUnknowns = 
                  currentNumberOfUnknowns > 0

                Tuple currentNumberOfLands soughtNumberOfLands = 
                  Array.index progress.rowProgress index
                    # fromMaybe (Tuple 0 0)
                
                line =
                  Row index 

                finished = 
                  not hasUnknowns && currentNumberOfLands == soughtNumberOfLands

                canFill = 
                  hasUnknowns 
                    && Board.canFillLine false line board 
                    || Board.canFillLine true line board

                tooMany = 
                  currentNumberOfLands > soughtNumberOfLands

                tooFew = 
                  currentNumberOfLands + currentNumberOfUnknowns < soughtNumberOfLands
              in 
                HH.div 
                  [ classes 
                      $ map ClassName
                      $ Array.catMaybes
                          [ Just "header"
                          , if finished then 
                              Just "finished" 
                            else if canFill then 
                              Just "can-fill"
                            else if tooMany then 
                              Just "too-many"  
                            else if tooFew then 
                              Just "too-many"  
                            else 
                              Nothing
                          ]
                  , CSS.style $ do
                      width $ units headerSizeInUnits
                      height $ units 1.0
                      lineHeight $ units 1.0
                  , HE.onClick 
                      $ const 
                      $ if canFill then 
                          Just $ ClickedHeader line 
                        else 
                          Nothing
                  ]
                  [ HH.text $ show soughtNumberOfLands ]
            
            renderTerrainType c region =
              let
                position = 
                  Tuple index c

                disabled = 
                  Set.member position board.disabled
              in
                HH.div
                  [ classes
                      $ map ClassName
                          [ "region"
                          , case region of 
                              Unknown -> "unknown"
                              Land -> "boat"
                              Water -> "water"
                          , if disabled then "disabled" else ""
                          ]
                  , CSS.style $ do
                      width $ units 1.0
                      height $ units 1.0
                      fontSize $ units 0.25
                      lineHeight $ units 1.0
                      textAlign TextAlign.center
                      color whitesmoke
                  , HE.onClick 
                      $ const 
                      $ if disabled then 
                          Nothing
                        else if solved then 
                          Nothing
                        else 
                          Just $ Toggle index c
                  ]
                  [ if disabled then 
                      HH.text "*"
                    else 
                      HH.text ""
                  ]
          in 
            HH.div 
              [ CSS.style $ do 
                  width $ px widthInPx
                  height $ units 1.0
                  display flex
                  flexDirection row
              ]
              $ Array.cons rowHeader
              $ mapWithIndex renderTerrainType rowTerrainTypes
      in
        Matrix.toRows regions
          # mapWithIndex renderRow
  in
  { boardView : 
      HH.div
        [ CSS.style $ do
            width $ px widthInPx
            height $ px heightInPx
            display flex
            flexDirection column
        ]
        $ Array.cons columnHeadersRow rows
  , boardWidth : widthInPx
  , boardHeight : heightInPx
  }


handleAction :: Action → H.HalogenM State Action NoChildSlots Message Aff Unit
handleAction = 
  let 
    setBoard board state = 
      state { board = board }
  in
  case _ of
    Resize _ -> 
      do
        window <- H.liftEffect Web.HTML.window
        width <- H.liftEffect $ Window.innerWidth window
        height <- H.liftEffect $ Window.innerHeight window
        let viewPort = { width, height }
        H.modify_ 
          \ state -> state { viewPort = viewPort }
    Toggle row col -> 
      H.modify_ $ updateBoard $ Board.rotateTerrainType row col
    Initialize ->
      do
        state <- H.get
        board <- H.liftEffect $ Board.initialize state.board
        window <- H.liftEffect $ Web.HTML.window
        H.subscribe'
          $ const
          $ ES.eventListenerEventSource
              (EventType "resize")
              (Window.toEventTarget window)
              (Just <<< Resize)
        H.modify_ $ updateBoard (const board)
    ClickedClear -> 
      H.modify_ $ updateBoard Board.clearEnabledTerrainTypes
    ClickedHeader line -> 
      H.modify_ $ updateBoard $ Board.fillLine line
    ClickedUndo -> 
      H.modify_ $ updateBoard Board.undo