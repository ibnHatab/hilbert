module Main (main) where

{-|
@docs main
-}

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import StartApp
import Window

import Hilbert exposing (Action, view, init)
import Dash exposing (Action, view, init)

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
    order = 4
    (width, height) = (400, 600)
    (dash, dashFx) = Dash.init order
    (hilbert, hilbertFx) = Hilbert.init 4 width
  in
    ( Model dash hilbert
    , Effects.batch
               [ Effects.map Dash dashFx
               , Effects.map Hilbert hilbertFx
               ]
    )

-- UPDATE
type Action
  = WindowResize (Int, Int)
  | Dash Dash.Action
  | Hilbert Hilbert.Action


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message |> Debug.log "m_act" of
    WindowResize dimensions ->
      -- FIXME:
      (model, Effects.none)
    otherwise ->
      (model, Effects.none)

-- VIEW
(=>) = (,)
view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [ style [ "display" => "flex", "flex-wrap" => "wrap" ] ]
      [ Dash.view (Signal.forwardTo address Dash) model.dash
      , Hilbert.view (Signal.forwardTo address Hilbert) model.hilbert
      ]

-- APP
windowResize =
  Signal.map WindowResize Window.dimensions

app =
  StartApp.start
             { init = init
             , update = update
             , view = view
             , inputs = [windowResize]
             }
{-|
-}
main : Signal Html
main =
  app.html
