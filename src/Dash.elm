module Dash (Model, Action(..), init, update, view) where

{-|
@docs Model, Action, init, update, view
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Effects exposing (Effects, none)
import Signal
import String exposing (toList)
import Dict exposing (fromList)

import Braille exposing (..)
import Game

-- MODEL
{-|-}
type alias Model =
  {
  -- last
    game : Game.Model
  , gameFx : Signal.Address Game.Events
  }


{-|-}
init : Game.Model -> Signal.Address Game.Events
     -> (Model, Effects Action)
init game gameFx =
  ( {
    -- last
      game = game
    , gameFx = gameFx
    }
  , Effects.none
  )

-- UPDATE

{-|-}
type Action
  = Game Game.Events
  | Mode
  | Increment
  | Decrement
  | Next
  | Prev
  | GlyphOnClick Int
{-|-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  let validOrder new = let c = Game.constants
                      in 0 < new && new <= c.maxRank
      notifyFx event = Signal.send model.gameFx event
                     |> Effects.task
                     |> Effects.map (Game << Game.TaskDone)
  in
    case action |> Debug.log "dash_act" of
      Increment ->
        if validOrder (model.game.order + 1)
        then (model, notifyFx (Game.Order (model.game.order + 1)))
        else (model, notifyFx (Game.Display (Game.Error "MAX rank!")))

      Decrement ->
        if validOrder (model.game.order - 1)
        then (model, notifyFx (Game.Order (model.game.order - 1)))
        else (model, notifyFx (Game.Display (Game.Error "MIN rank!")))

      Mode ->
        let newState = case model.game.state of
                         Game.Begin -> Game.FreeMode
                         Game.FreeMode -> Game.ChooseOne
                         Game.ChooseOne -> Game.FreeMode
                         _ -> Game.Begin
        in
          (model, notifyFx (Game.StateChange model.game.state newState))

      GlyphOnClick n ->
        ( model, notifyFx (Game.GiveAnswer (String.slice n (n+1) model.game.question )))
      -- Game (Game.AskQuestion str) ->
      --   ( {model | images = mkImages str }, Effects.none )

      -- Game (Game.Geometry _) ->
      --   ( {model | images = mkImages model.game.question }, Effects.none )

      otherwise ->
        ( model, Effects.none )


smallButtonStyle =
  [ "float" => "left"
  , "width" => "23vw"
  , "height" => "9vw"
  , "margin" => "1vw 1vw 1vw 1vw"
  , "border-radius" => "3vw"
  , "border" => "1vw"
  , "border-style" => "solid"
  ]
textStyle =
  [ "font-size" => "8vw"
  , "font-family" => "monospace"
  , "width" => "21vw"
  , "text-align" => "center"
  ]

-- VIEW
(=>) = (,)
{-|-}
view : Signal.Address Action -> Model -> Html
view address model =
  let
    mkImages str =
      let
        side = round <| (toFloat (fst model.game.geometry)) / (4*4.8)
        fx n = Signal.message address (GlyphOnClick n)
      in List.indexedMap (\idx c -> Braille.glyph side (Braille.dots c) (Just (fx idx))) (toList str)
  in
    div [ style ["height" => "25vw"] ]
          [ -- rank
            div [ style [ "float" => "left"
                        , "width" => "25vw"
                        , "height" => "25vw"]
                ]
            [ div [ style (smallButtonStyle ++ textStyle)
                  , onClick address Increment ]
              [ text (toString model.game.order)]
            , div [ style  (smallButtonStyle ++ textStyle)
                  , onClick address Decrement ]
              [text (toString (model.game.order - 1))]
          ]
          -- pics
          , div [ style [ "float" => "left"
                        , "width" => "43vw"
                        , "height" => "22vw"
                        , "border-radius" => "3vw"
                        , "border" => "1vw"
                        , "border-style" => "solid"
                        , "margin" => "1vw 1vw 1vw 1vw"
                        ]
                , align "center"
                ] [ div [ style [ "vertical-align" => "middle"
                                , "height" => "23vw"
                                , "display" => "table-cell"
                                ]
                        ] (mkImages model.game.question)
                  ]

          -- start
          , div [ style ([ "float" => "left"
                         , "width" => "21vw"
                         , "height" => "21vw"
                         , "border-radius" =>  "15vw"
                         , "margin" => "1vw 1vw 1vw 1vw"
                         , "border" => "1vw"
                         , "border-style" => "solid"
                         , "vertical-align" => "bottom"
                         ] ++ textStyle)
                , onClick address Mode
                ]
            [ text (case model.game.state of
                      Game.Begin -> "B"
                      Game.FreeMode -> "F"
                      Game.ChooseOne -> "C"
                      Game.TypeAnswer -> "T"
                 )
          ]
        ]
