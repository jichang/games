module Panel exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (..)

type Msg
  = MoveLeft
  | MoveRight
  | MoveDown

view : () -> Html Msg
view () =
  div []
    [ button [ onClick MoveLeft ] [ text "Left" ]
    , button [ onClick MoveRight ] [ text "Right" ]
    , button [ onClick MoveDown ] [ text "Down" ] ]
