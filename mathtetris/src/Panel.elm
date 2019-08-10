module Panel exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

type Msg
  = MoveLeft
  | MoveRight
  | MoveDown

view : () -> Html Msg
view () =
  div [ class "flex__box" ]
    [ button [ class "button", onClick MoveLeft ] [ text "向左" ]
    , div [class "flex__item"] [ button [class "button", onClick MoveDown ] [ text "向下" ]]
    , button [ class "button", onClick MoveRight ] [ text "向右" ] ]
