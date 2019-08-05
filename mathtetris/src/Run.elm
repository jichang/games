module Run exposing (..)

import Random
import Html exposing (Html, div, button, input, text, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Level exposing (Level(..))
import Grid exposing (..)
import Panel exposing (..)
import Tile exposing (..)
import Time

type alias Model =
  { level: Level
  , interval: Float
  , generator: Random.Generator (Int, Int)
  , currTile: Maybe Tile
  , nextTile: Maybe Tile
  , grid: Grid }

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
    grid =
      { rows = 12
      , cols = 9
      , tiles = [ ] }
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
            y = tile.y + 1
            currGrid = model.grid
            (currTile, grid) =
              case y < model.grid.rows of
                True ->
                  (Just { tile | y = y }, model.grid)
                False ->
                  (Nothing, {currGrid | tiles = List.append currGrid.tiles [tile]})
          in
            ({ model | currTile = currTile, grid = grid}, Cmd.none)
        Nothing ->
          case model.nextTile of
            Just tile ->
              ({ model | currTile = Just tile, nextTile = Nothing }, Random.generate NewTile model.generator)
            Nothing ->
              (model, Random.generate NewTile model.generator)
    NewTile (ty, val) ->
      let
        value = Tile.newValue ty val
        tile =
          { x = model.grid.cols // 2
          , y = 0 
          , value = value }
      in
        ({ model | nextTile = Just tile}, Cmd.none)
    PanelMsg _ ->
      (model, Cmd.none)

view : Model -> Html Msg
view model =
  let
    tiles =
      case model.currTile of
        Just tile -> List.append model.grid.tiles [tile]
        Nothing -> model.grid.tiles
    nextTile =
      case model.nextTile of
        Just tile -> div [] [ text (Tile.toText tile.value) ]
        Nothing -> div [] []
  in
    div [ class "page" ]
      [ nextTile
      , Grid.view model.grid tiles
      , Html.map PanelMsg (Panel.view ()) ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every model.interval Tick
