module Value exposing (..)

import Browser
import Html exposing (Html, span, text)
import Html.Attributes exposing (..)

type Value
  = Operand Int
  | Operator Int

init : Int -> Int -> Value
init ty val =
  case ty of
    0 ->
      Operand val
    _ ->
      Operator val

view : Value -> Html a
view value =
  span [ class "value" ] [ text (toText value) ]

toText : Value -> String
toText value =
  case value of
    Operand operand -> String.fromInt operand
    Operator operator -> String.fromInt operator
