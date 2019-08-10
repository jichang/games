module Prepare exposing (..)

import Html exposing (Html, div, h3, button, input, text, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Level exposing (Level(..))

type alias Model =
  { level: Level }

type Msg
  = Choose Level
  | Submit

init : () -> (Model, Cmd Msg)
init () =
  ({ level = Easy }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Choose level ->
      ({ model | level = level }, Cmd.none)
    Submit ->
      (model, Cmd.none)

view : Model -> Html Msg
view model =
  let
    levels = [Easy, Normal, Hard, Professional]
    options = List.map (\level -> optionView model.level level) levels
  in
    div [ class "page" ]
      [ h3 [] [ text "选择数学水平" ]
      , div [] options
      , button [class "button", onClick Submit] [ text "开始游戏" ] ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

optionView : Level -> Level -> Html Msg
optionView currLevel level =
  let
    isChecked = currLevel == level
    inputId = toId level
  in
    div [ class "form__field" ]
      [ input [ id inputId, checked isChecked, type_ "radio", name "level", onInput (\_ -> Choose level) ] []
      , label [ for inputId ] [ text (Level.toText level) ] ]

toId : Level -> String
toId level =
  case level of
    Easy -> "level-easy"
    Normal -> "level-normal"
    Hard -> "level-hard"
    Professional -> "level-professional"
