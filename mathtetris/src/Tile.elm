module Tile exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Term exposing(..)

type alias Tile =
  { x: Int
  , y: Int
  , term: Term }

view : Tile -> Html a
view tile =
  div [ class "tile" ] [ Term.view tile.term ]
