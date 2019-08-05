module Tile exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)

type Value
  = Operand Int
  | Operator Int

type alias Tile =
  { x: Int
  , y: Int
  , value: Value }

newValue : Int -> Int -> Value
newValue ty val =
  case ty of
    0 -> Operand val
    _ -> Operator val

view : Tile -> Html a
view tile =
  div [ class "tile" ] [ text (toText tile.value) ]

toText : Value -> String
toText value =
  case value of
    Operand operand -> String.fromInt operand
    Operator operator -> String.fromInt operator