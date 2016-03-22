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
import Mouse
import Touch

import Hilbert exposing (..)
import Dash exposing (..)
import Game exposing (..)
import Teremin

import Debug

-- MODEL
type alias Model =
  {
    dash         : Dash.Model
  , hilbert      : Hilbert.Model
  , game         : Game.Model
  , statusString : Maybe String
  }

init : (Model, Effects Action)
init =
  let
    gameMsgBox = Signal.forwardTo actionsMb.address Game
    -- game
    (game, gameFx) = Game.init gameMsgBox
    -- dashboard
    (dash, dashFx) = Dash.init game gameMsgBox
    -- play ground
    (hilbert, hilbertFx) = Hilbert.init game gameMsgBox
    -- Webaudio & messaging system
    _ = Teremin.init 0
--    _ = Teremin.setFrequency 10 100
  in
    ( Model dash hilbert game Nothing
    , Effects.batch [ Effects.map Dash dashFx,
                      Effects.map Hilbert hilbertFx,
                      Effects.map Game gameFx,
                      sendInitial
                    ]
    )

-- UPDATE
type Action
  = WindowResize (Int, Int)
  | NoOp
  -- subforms
  | Dash Dash.Action
  | Hilbert Hilbert.Action
  | Game Game.Events


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message -- |> Debug.log "m_act"
  of
    Hilbert act ->
      let
        (hilbert, hilbertFx) = Hilbert.update act model.hilbert
      in ({model | hilbert = hilbert}, Effects.map Hilbert hilbertFx)

    Dash act ->
      let
        (dash, dashFx) = Dash.update act model.dash
      in ({model | dash = dash}, Effects.map Dash dashFx)

    NoOp -> (model, Effects.none)

    WindowResize dim ->
      update (Game (Game.Geometry dim)) model

    -- Broadcaast game events and state
    Game act ->
      let
        (game, gameFx) = Game.update act model.game

        (h, d)  = (model.hilbert, model.dash)
        (hilbert, hilbertFx) = Hilbert.update (Hilbert.Game act) { h | game = game }
        (dash, dashFx) = Dash.update (Dash.Game act) { d | game  = game }
        --
        (super, superFx) =
          case act of
            Game.Display (Game.Error s) ->
              ({model | statusString = Just ("Error" ++ s)}, Effects.none)
            Game.Display (Game.Info s) ->
              ({model | statusString = Just s}, Effects.none)
            otherwise ->
              (model, Effects.none)

      in ( { super | game = game, dash = dash, hilbert = hilbert}
         , Effects.batch [ Effects.map Game gameFx
                         , Effects.map Dash dashFx
                         , Effects.map Hilbert hilbertFx
                         , superFx])



-- VIEW
(=>) = (,)
view : Signal.Address Action -> Model -> Html.Html
view address model =
  div []
        [ div [ style [ "height" => "5vw"
                      , "font-size" => "4vw"
                      , "font-family" => "monospace"
                      , "text-align" => "right"
                      ]]
          [text (Maybe.withDefault "" model.statusString)]
        , Dash.view (Signal.forwardTo address Dash) model.dash
        , div [ style [ "height" => "2vw"]] []
        , Hilbert.view (Signal.forwardTo address Hilbert) model.hilbert
        , div [ style [ "height" => "5vw"]] []
        -- status line
      ]

-- Tasks and signals
windowResize = Signal.map WindowResize Window.dimensions
firstResize = Signal.sampleOn appStartMb.signal windowResize

appStartMb = Signal.mailbox ()
sendInitial = Signal.send appStartMb.address () |> Task.map (always NoOp) |> Effects.task

-- Game event mailbox
actionsMb : Signal.Mailbox Action
actionsMb = Signal.mailbox NoOp

mousePosition =
  Signal.map2 (,) Mouse.position Mouse.isDown
    |> Signal.filter snd ((0, 0), False)
    |> Signal.map fst
    |> Signal.map (\p -> (Hilbert (Hilbert.MousePos p)))


touchPosition = Signal.map (\{x, y} -> (Hilbert (Hilbert.MousePos (x,y)))) Touch.taps

-- APP
app =
  StartApp.start
             { init = init
             , update = update
             , view = view
             , inputs = [ firstResize
                        , windowResize
                        , actionsMb.signal
                        , mousePosition]
             }
{-|
-}
main : Signal Html
main = app.html

port runner : Signal (Task.Task Never ())
port runner = app.tasks
