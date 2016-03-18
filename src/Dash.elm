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
      side = round <| (toFloat (fst model.dimention)) / toFloat (2^model.rank)
  in div [ style ["height" => "25vw"] ]
     [ rank
       -- picters
     , div [ style [ "float" => "left"
                   , "width" => "46vw"
                   , "height" => "25vw"
                   , "border-radius" => "15vw"
                   , "border" => "1px solid blue"
                   , "align" => "center"
                   ]]
             [
              Braille.glyph [2,3,4,6] (side // 2)
             , Braille.glyph [2,3,4,6] (side // 2)
             ]
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
