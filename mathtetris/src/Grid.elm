module Grid exposing (..)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (..)
import Tile exposing (Tile)

type alias Grid =
  { rows: Int
  , cols: Int
  , tiles: List Tile }

view : Grid -> List Tile -> Html a
view grid tiles =
  let
    cells = List.map (\tile -> cellView grid.rows grid.cols tile) tiles
    paddingBottom = String.fromFloat ((toFloat grid.rows) / (toFloat grid.cols) * 100) ++ "%"
  in
    div
      [ class "grid", style "padding-bottom" paddingBottom ]
      [ div [ class "cells" ] cells ]

cellView : Int -> Int -> Tile -> Html a
cellView rows cols tile =
  let
    top = String.fromFloat ((toFloat tile.y) / (toFloat rows) * 100) ++ "%"
    left = String.fromFloat ((toFloat tile.x) / (toFloat cols) * 100) ++ "%"
    w = String.fromFloat (1.0 / (toFloat cols) * 100) ++ "%"
    h = String.fromFloat (1.0 / (toFloat rows) * 100) ++ "%"
  in
    div
      [ class "cell", style "top" top, style "left" left, style "width" w, style "height" h ]
      [ Tile.view tile ]
