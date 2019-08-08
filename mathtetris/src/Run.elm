module Run exposing (..)

import Random
import Time
import Array
import Html exposing (Html, div, button, input, text, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Level exposing (Level(..))
import Grid exposing (..)
import Panel exposing (..)
import Tile exposing (..)
import Value exposing (..)

type alias Model =
  { level: Level
  , interval: Float
  , generator: Random.Generator (Int, Int)
  , currTile: Maybe Tile
  , nextTile: Maybe Tile
  , grid: Grid.Model }

type Msg
  = Tick Time.Posix
  | NewTile (Int, Int)
  | PanelMsg Panel.Msg

init : Level -> (Model, Cmd Msg)
init level =
  let
    (range, interval) =
      case level of
        Easy ->
          (10, 4000)
        Normal ->
          (10, 2000)
        Hard ->
          (10, 1000)
        Professional ->
          (10, 500)
    generator = Random.pair (Random.int 0 2) (Random.int 1 range)
    grid = Grid.init 12 9
    model =
      { level = level
      , grid = grid
      , generator = generator
      , currTile = Nothing
      , nextTile = Nothing
      , interval = interval }
    
  in
    (model, Random.generate NewTile generator)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick _ ->
      case model.currTile of
        Just tile ->
          let
            grid = model.grid
            index = tile.y * model.grid.cols + tile.x
            cell = Array.get (index + model.grid.cols) model.grid.cells
            (currTile, newGrid) =
              case cell of
                Just (Just _) ->
                  (Nothing, { grid | cells = Array.set index (Just tile.value) grid.cells })
                Just Nothing ->
                  (Just {tile | y = tile.y + 1}, model.grid)
                Nothing ->
                  (Just {tile | y = tile.y + 1}, model.grid)
          in
            ({ model | currTile = currTile, grid = newGrid }, Cmd.none)
        Nothing ->
          case model.nextTile of
            Just tile ->
              ({ model | currTile = Just tile, nextTile = Nothing }, Random.generate NewTile model.generator)
            Nothing ->
              (model, Random.generate NewTile model.generator)
    NewTile (ty, val) ->
      let
        value = Value.init ty val
        tile =
          { x = model.grid.cols // 2
          , y = 0 
          , value = value }
      in
        ({ model | nextTile = Just tile}, Cmd.none)
    PanelMsg MoveLeft ->
      (model, Cmd.none)
    PanelMsg MoveRight ->
      (model, Cmd.none)
    PanelMsg MoveDown ->
      (model, Cmd.none)

view : Model -> Html Msg
view model =
  let
    currTile =
      case model.currTile of
        Just tile ->
          let
            grid = model.grid
            top = String.fromFloat ((toFloat tile.y) / (toFloat grid.rows) * 100) ++ "%"
            left = String.fromFloat ((toFloat tile.x) / (toFloat grid.cols) * 100) ++ "%"
            w = String.fromFloat (1.0 / (toFloat grid.cols) * 100) ++ "%"
            h = String.fromFloat (1.0 / (toFloat grid.rows) * 100) ++ "%"
          in
            div [ class "cell", style "top" top, style "left" left, style "width" w, style "height" h ] [Tile.view tile]
        Nothing ->
          div [ class "cell" ] []
    nextTile =
      case model.nextTile of
        Just tile -> div [] [ Value.view tile.value ]
        Nothing -> div [] []
  in
    div [ class "page" ]
      [ nextTile
      , div [ class "grid__container" ] [ Grid.view model.grid, currTile ]
      , Html.map PanelMsg (Panel.view ()) ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every model.interval Tick
