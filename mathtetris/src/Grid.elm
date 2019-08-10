module Grid exposing (..)

import Browser
import Array exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (..)
import Tile exposing (..)
import Term exposing (..)

type alias Row = Array (Maybe Term)

type alias Model =
  { rows: Int
  , cols: Int
  , cells: Array Row }

init : Int -> Int -> Model
init rows cols =
  let
    cells = Array.repeat rows (Array.repeat cols Nothing)
  in
    { rows = rows
    , cols = cols
    , cells = cells }

view : Model -> Html a
view grid =
  let
    paddingBottom = String.fromFloat ((toFloat grid.rows) / (toFloat grid.cols) * 100) ++ "%"
    tiles =
      grid.cells
      |> Array.indexedMap toTile
      |> Array.foldr Array.append Array.empty
      |> Array.map (cellView grid)
      |> Array.toList
  in
    div
      [ class "grid", style "padding-bottom" paddingBottom ]
      [ div [ class "cells" ] tiles ]

mapTile : Int -> Int -> Maybe Term -> (Int, Int, Maybe Term)
mapTile rowIndex colIndex maybeTerm =
  (rowIndex, colIndex, maybeTerm)

toTile : Int -> Row -> Array (Int, Int, Maybe Term)
toTile rowIndex row =
  row
  |> Array.indexedMap (mapTile rowIndex)

cellView : Model -> (Int, Int, Maybe Term) -> Html a
cellView grid (rowIndex, colIndex, maybeTerm) =
  let
    top = String.fromFloat ((toFloat rowIndex) / (toFloat grid.rows) * 100) ++ "%"
    left = String.fromFloat ((toFloat colIndex) / (toFloat grid.cols) * 100) ++ "%"
    w = String.fromFloat (1.0 / (toFloat grid.cols) * 100) ++ "%"
    h = String.fromFloat (1.0 / (toFloat grid.rows) * 100) ++ "%"
    tileView =
      case maybeTerm of
        Just term ->
          div
            [ class "cell", style "top" top, style "left" left, style "width" w, style "height" h ]
            [ Term.view term ]
        Nothing -> div [] []
  in
    tileView

isAvailable : Int -> Int -> Model -> Bool
isAvailable x y model =
  case Array.get y model.cells of
    Just row ->
      case Array.get x row of
        Just (Just _) -> False
        Just Nothing -> True
        Nothing -> False
    Nothing -> False

update : Int -> Int -> Term -> Model -> Model
update x y term grid =
  case Array.get y grid.cells of
    Just row ->
      let
        newRow = Array.set x (Just term) row
        cells = Array.set y newRow grid.cells
      in
        { grid | cells = cells }
    Nothing ->
      grid

eliminate : Model -> (Int, Model)
eliminate grid =
  let
    filterRow =
      \row ->
        case Term.eval row of
          Just 0 -> False
          _ -> True
    leftCells =
      grid.cells
      |> Array.filter filterRow
    count = grid.rows - Array.length leftCells
    cells =
      Array.append (Array.repeat count (Array.repeat grid.cols Nothing)) leftCells
  in
    (count, { grid | cells = cells })
