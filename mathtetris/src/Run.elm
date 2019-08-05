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

type alias Tile =
  { x: Int
  , y: Int }

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
      { rows = 9
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
      (model, Cmd.none)
    NewTile (tp, val) ->
      (model, Cmd.none)
    PanelMsg _ ->
      (model, Cmd.none)

view : Model -> Html Msg
view model =
  div [ class "page" ]
    [ Grid.view model.grid 
    , Html.map PanelMsg (Panel.view ()) ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every model.interval Tick
