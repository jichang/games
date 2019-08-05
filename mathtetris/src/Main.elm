module Main exposing (main)

import Browser
import Html exposing (Html, div, p, text)
import Time
import Prepare
import Run

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions }

type StepModel
  = PrepareModel Prepare.Model
  | RunModel Run.Model

type alias Model =
  { stepModel: StepModel }

type Msg
  = PrepareMsg Prepare.Msg
  | RunMsg Run.Msg
  | Tick Time.Posix

init : () -> (Model, Cmd Msg)
init _ =
  let
    (prepareModel, prepareCmd) = Prepare.init ()
  in
    ({ stepModel = PrepareModel prepareModel }, Cmd.map PrepareMsg prepareCmd)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model.stepModel) of
    (PrepareMsg prepareMsg, PrepareModel prepareModel) ->
      case prepareMsg of
        Prepare.Choose level ->
          let
            (newPrepareModel, newPrepareCmd) = Prepare.update prepareMsg prepareModel
          in
            ({model | stepModel = PrepareModel newPrepareModel}, Cmd.map PrepareMsg newPrepareCmd)
        Prepare.Submit ->
          let
            (runModel, runCmd) = Run.init prepareModel.level
          in
            ({model | stepModel = RunModel runModel}, Cmd.map RunMsg runCmd)
    (RunMsg runMsg, RunModel runModel) ->
      let
        (newRunModel, runCmd) = Run.update runMsg runModel
      in
        ({ model | stepModel = RunModel newRunModel}, Cmd.map RunMsg runCmd)
    (Tick _, RunModel runModel) ->
      (model, Cmd.none)
    _ ->
      let
        (prepareModel, prepareCmd) = Prepare.init ()
      in
        ({ stepModel = PrepareModel prepareModel }, Cmd.map PrepareMsg prepareCmd)

view : Model -> Html Msg
view model =
  let
    content =
      case model.stepModel of
        PrepareModel prepareModel ->
          Html.map PrepareMsg (Prepare.view prepareModel)
        RunModel runModel ->
          Html.map RunMsg (Run.view runModel)
  in
    div [] [ content ]

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.stepModel of
    PrepareModel prepareModel ->
      Sub.map PrepareMsg (Prepare.subscriptions prepareModel)
    RunModel runModel ->
      Sub.map RunMsg (Run.subscriptions runModel)
