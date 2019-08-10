module Run exposing (..)

import Random
import Time
import Array
import Html exposing (Html, div, button, input, text, label, span, p, h2)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Level exposing (Level(..))
import Grid exposing (..)
import Panel exposing (..)
import Tile exposing (..)
import Term exposing (..)

type alias Model =
  { level: Level
  , interval: Float
  , generator: Random.Generator (Int, Int)
  , score: Int
  , currTile: Maybe Tile
  , nextTile: Maybe Tile
  , grid: Grid.Model
  , running: Bool }

type Msg
  = Tick Time.Posix
  | NewTile (Int, Int)
  | PanelMsg Panel.Msg
  | Restart

init : Level -> (Model, Cmd Msg)
init level =
  let
    (range, interval) = (Level.toRange level, Level.toInterval level)
    generator = Random.pair (Random.int 0 2) (Random.int 0 range)
    grid = Grid.init 12 9
    model =
      { level = level
      , grid = grid
      , generator = generator
      , score = 0
      , currTile = Nothing
      , nextTile = Nothing
      , interval = interval
      , running = True }
    
  in
    (model, Random.generate NewTile generator)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick _ ->
      case model.currTile of
        Just tile ->
            case isAvailable tile.x (tile.y + 1) model.grid of
              True ->
                let
                  currTile = model.currTile
                in
                  ({model | currTile = Just { tile | y = tile.y + 1} }, Cmd.none)
              False ->
                let
                  (count, grid) =
                    Grid.update tile.x tile.y tile.term model.grid
                    |> Grid.eliminate
                in
                  ({ model | score = model.score + count * 10, currTile = Nothing, grid = grid}, Cmd.none)
        Nothing ->
          case model.nextTile of
            Just tile ->
              case isAvailable tile.x tile.y model.grid of
                True ->
                  ({ model | currTile = Just tile, nextTile = Nothing }, Random.generate NewTile model.generator)
                False ->
                  ({ model | running = False }, Cmd.none)
            Nothing ->
              (model, Random.generate NewTile model.generator)
    NewTile (ty, val) ->
      let
        term = Term.init ty val
        tile =
          { x = model.grid.cols // 2
          , y = 0 
          , term = term }
      in
        ({ model | nextTile = Just tile}, Cmd.none)
    PanelMsg MoveLeft ->
      case model.currTile of
        Just tile ->
            case isAvailable (tile.x - 1) tile.y model.grid of
              True ->
                let
                  currTile = model.currTile
                in
                  ({model | currTile = Just { tile | x = tile.x - 1} }, Cmd.none)
              False ->
                (model, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    PanelMsg MoveRight ->
      case model.currTile of
        Just tile ->
            case isAvailable (tile.x + 1) tile.y model.grid of
              True ->
                let
                  currTile = model.currTile
                in
                  ({model | currTile = Just { tile | x = tile.x + 1} }, Cmd.none)
              False ->
                (model, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    PanelMsg MoveDown ->
      case model.currTile of
        Just tile ->
            case isAvailable tile.x (tile.y + 1) model.grid of
              True ->
                let
                  currTile = model.currTile
                in
                  ({model | currTile = Just { tile | y = tile.y + 1} }, Cmd.none)
              False ->
                (model, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    Restart ->
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
        Just tile ->
          p [ class "tile--next" ]
            [ span [] [ text "下一个：" ]
            , Term.view tile.term ]
        Nothing -> div [] []
    score = div [] [ text ("得分：" ++ String.fromInt model.score)]
    modal =
      case model.running of
        True -> div [] []
        False ->
          div [class "modal"]
            [ h2 [] [text "游戏结束"]
            , div [] [ p [] [text ("最终得分：" ++ String.fromInt model.score)] ]
            , button [class "button", onClick Restart ] [ text "重新开始" ] ]
  in
    div [ class "page" ]
      [ div [ class "flex__box"] [ div [class "flex__item"] [nextTile], score ]
      , div [ class "grid__container" ] [ Grid.view model.grid, currTile ]
      , Html.map PanelMsg (Panel.view ())
      , modal ]

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.running of
    True ->
      Time.every model.interval Tick
    False ->
      Sub.none
