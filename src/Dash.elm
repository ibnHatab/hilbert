module Dash (Action(Resize), Model, init, update, view) where

{-|
@docs Model, Action, init, update, view
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Effects exposing (Effects, none)
import Signal

import Braille exposing (..)
import Game

-- MODEL
{-|-}
type alias Model =
  { game : Signal.Address Game.Events
  , rank : Int
  , freeMode : Bool
  , dimention : (Int, Int)
  , images : List Html
  }


{-|-}
init : Signal.Address Game.Events
     -> Int
     -> (Int, Int)
     -> (Model, Effects Action)
init game rank dimention =
  ( { game = game
    , rank = rank
    , freeMode = False
    , dimention = dimention
    , images = []
    }
  , Effects.none
  )

-- UPDATE

{-|-}
type Action = Increment
            | Decrement
            | FreeMode
            | TaskDone ()
            | Resize (Int, Int)
            | SetImages (List Html)

{-|-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  let checkRank new =  0 < new && new <= Game.maxRank
      notifyFx event = Signal.send model.game event
                     |> Effects.task
                     |> Effects.map TaskDone
  in
    case action |> Debug.log "dash_act" of
      Resize dim ->
        ( { model | dimention = dim }, Effects.none )

      Increment ->
        if checkRank (model.rank + 1)
        then ({ model | rank = model.rank + 1 }, notifyFx (Game.Rank (model.rank + 1)))
        else (model, notifyFx (Game.ShowError "Max rank!") )

      Decrement ->
        if checkRank (model.rank - 1)
        then ( { model | rank = model.rank - 1 }
             , notifyFx (Game.Rank (model.rank - 1)))
        else (model, notifyFx (Game.ShowError "Min rank!"))

      FreeMode -> let freeMode = not model.freeMode
              in ( { model | freeMode = freeMode }
                 , notifyFx (Game.FreeMode freeMode))

      SetImages images ->
        ( model, Effects.none )

      TaskDone () ->
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
  div [ style ["height" => "25vw"] ]
        [ -- rank
          div [ style [ "float" => "left"
                      , "width" => "25vw"
                      , "height" => "25vw"]
              ]
          [ div [ style (smallButtonStyle ++ textStyle)
                , onClick address Increment ]
            [ text (toString model.rank)]
          , div [ style  (smallButtonStyle ++ textStyle)
                , onClick address Decrement ]
            [text (toString (model.rank - 1))]
          ]
        -- picters
        , div [ style [ "float" => "left"
                      , "width" => "43vw"
                      , "height" => "22vw"
                      , "border-radius" => "3vw"
                      , "border" => "1vw"
                      , "border-style" => "solid"
                      , "margin" => "1vw 1vw 1vw 1vw"
                      ]
              ,  align "center"
              ] model.images
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
              , onClick address FreeMode
              ]
          [ if model.freeMode
            then text "F"
            else text "R"
          ]
        ]
