module Tile exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Value exposing(..)

type alias Tile =
  { x: Int
  , y: Int
  , value: Value }

view : Tile -> Html a
view tile =
  div [ class "tile" ] [ Value.view tile.value ]
