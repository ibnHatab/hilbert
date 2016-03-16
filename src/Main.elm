-- module Main (main) where

{-|
@docs main
-}

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import StartApp
import Window
import Task exposing (Task)

import Hilbert exposing (..) -- (init, update, view, Action)
import Dash exposing (init, update, view)
import Game exposing (Events)
import Teremin

import Debug

-- MODEL
type alias Model =
  {
    dash: Dash.Model
  , hilbert : Hilbert.Model
  }

init : (Model, Effects Action)
init =
  let
    _ = Teremin.init(0)
    order = 4
    (width, height) = (400, 600)

    showErrorAddress = Signal.forwardTo actionsMb.address ShowError
    game = Signal.forwardTo actionsMb.address Game

    (dash, dashFx) = Dash.init game order width
    (hilbert, hilbertFx) = Hilbert.init game order width
  in
    ( Model dash hilbert
    , Effects.batch [ Effects.map Dash dashFx, Effects.map Hilbert hilbertFx]
    )

-- UPDATE
type Action
  = WindowResize (Int, Int)
  | ShowError String
  | NoOp
  -- subforms
  | Dash Dash.Action
  | Hilbert Hilbert.Action
  | Game Game.Events


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message |> Debug.log "m_act" of
    NoOp -> (model, Effects.none)

    WindowResize (w, h) ->
      let
        (dash, dashFx) = Dash.update (Dash.Resize w) model.dash
        (hilbert, hilbertFx) = Hilbert.update (Hilbert.Resize w) model.hilbert
      in ({model | dash = dash, hilbert = hilbert},
          Effects.batch [ Effects.map Dash dashFx, Effects.map Hilbert hilbertFx]
         )

    Game act ->
      case act of
        Game.Rank order -> update (Hilbert (Hilbert.Order order)) model

    ShowError s ->
      (model, Effects.none)

    Dash act ->
      let
        (dash, dashFx) = Dash.update act model.dash
      in ({model | dash = dash}, Effects.map Dash dashFx)

    Hilbert act ->
      let
        (hilbert, hilbertFx) = Hilbert.update act model.hilbert
      in ({model | hilbert = hilbert}, Effects.map Hilbert hilbertFx)

-- VIEW
(=>) = (,)
view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [ style [ "display" => "flex", "flex-wrap" => "wrap", "margin" => "10px" ] ]
      [ Dash.view (Signal.forwardTo address Dash) model.dash
      , Hilbert.view (Signal.forwardTo address Hilbert) model.hilbert
      ]

-- APP
windowResize =
  Signal.map WindowResize Window.dimensions

actionsMb : Signal.Mailbox Action
actionsMb = Signal.mailbox NoOp

-- changeOrder : Signal.Address Int
-- changeOrder =
--   Signal.forwardTo actions.address Game

app =
  StartApp.start
             { init = init
             , update = update
             , view = view
             , inputs = [windowResize, actionsMb.signal]
             }
{-|
-}
main : Signal Html
main =
  app.html

port runner : Signal (Task.Task Never ())
port runner =
  app.tasks
