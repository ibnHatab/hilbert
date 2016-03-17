module Dash (Action, Model, init, update, view) where

{-|
@docs Model, Action, init, update, view
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Effects exposing (Effects, none)
import Signal

import Game

-- MODEL
{-|-}
type alias Model =
  { game : Signal.Address Game.Events
  , rank : Int
  , freeMode : Bool
  }


{-|-}
init : Signal.Address Game.Events
     -> Int
     -> (Model, Effects Action)
init game rank =
  ( { game = game
    , rank = rank
    , freeMode = False
    }
  , Effects.none
  )

-- UPDATE

{-|-}
type Action = Increment
            | Decrement
            | FreeMode
            | TaskDone ()

{-|-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  let checkRank new =  0 < new && new <= Game.maxRank
      notifyFx event = Signal.send model.game event
                     |> Effects.task
                     |> Effects.map TaskDone
  in
    case action |> Debug.log "dash_act" of
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

      TaskDone () ->
        ( model, Effects.none )

-- VIEW
(=>) = (,)
{-|-}
view : Signal.Address Action -> Model -> Html
view address model =
  let rank = div [ style [ "float" => "left"
                         , "width" => "25vw"
                         , "height" => "25vw"]]
             [ -- red
               div [ style [ "float" => "left"
                           , "width" => "25vw"
                           , "border-radius" => "15vw"
                           , "height" => "12vw"
                           , "background-color" => "red"
                           ]
                   , onClick address Increment ]
               [div [textStyle] [text (toString model.rank)]]
             -- green
             , div [ style [ "float" => "left"
                           , "width" => "25vw"
                           , "border-radius" => "15vw"
                           , "height" => "13vw"
                           , "background-color" => "green"
                           ]
                   , onClick address Decrement ]
               [div [textStyle] [text (toString (model.rank - 1))]]
             ]
  in div [ style ["height" => "25vw"] ]
     [ rank
       -- picters
     , div [ style [ "float" => "left"
                   , "width" => "46vw"
                   , "height" => "25vw"
                   , "border-radius" => "15vw"
                   , "border" => "1px solid blue"
                   ]] []
     , div [ style [ "float" =>  "right"
                   , "width" =>  "25vw"
                   , "height" =>  "25vw"
                   , "border-radius" =>  "15vw"
                   , "background-color" => if model.freeMode
                                           then "green"
                                           else "red"
                   ]
           , onClick address FreeMode
           ] [  ]
     ]

textStyle : Attribute
textStyle =
  style [ "font-size" => "12vw"
        , "font-family" => "monospace"
        , "width" => "25vw"
        , "text-align" => "center"
        ]
