module Grid exposing (..)

import Browser
import Array exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (..)
import Value exposing (..)

type alias Model =
  { rows: Int
  , cols: Int
  , cells: List (Maybe Value) }

init : Int -> Int -> Model
init rows cols =
  let
    cells = List.repeat (rows * cols) Nothing
  in
    { rows = rows
    , cols = cols
    , cells = cells }

view : Model -> Html a
view grid =
  let
    paddingBottom = String.fromFloat ((toFloat grid.rows) / (toFloat grid.cols) * 100) ++ "%"
    cells = List.indexedMap (\index cell -> cellView grid index cell) grid.cells
  in
    div
      [ class "grid", style "padding-bottom" paddingBottom ]
      [ div [ class "cells" ] cells ]

cellView : Model -> Int ->  Maybe Value -> Html a
cellView grid index cell =
  case cell of
    Just value ->
      let
        x = index // grid.cols
        y = remainderBy index grid.cols
        top = String.fromFloat ((toFloat y) / (toFloat grid.rows) * 100) ++ "%"
        left = String.fromFloat ((toFloat x) / (toFloat grid.cols) * 100) ++ "%"
        w = String.fromFloat (1.0 / (toFloat grid.cols) * 100) ++ "%"
        h = String.fromFloat (1.0 / (toFloat grid.rows) * 100) ++ "%"
      in
        div
          [ class "cell", style "top" top, style "left" left, style "width" w, style "height" h ]
          [ Value.view value ]
    Nothing ->
      div [ class "cell" ] []
