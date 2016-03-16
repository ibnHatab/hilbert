module Dash (Action(Resize), Model, init, update, view) where

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
  , width : Int
  }


{-|-}
init : Signal.Address Game.Events
     -> Int
     -> Int
     -> (Model, Effects Action)
init game rank width =
  ( { game = game
    , rank = rank
    , width = width
    }
  , Effects.none
  )

-- UPDATE

{-|-}
type Action = Increment | Decrement | Resize Int | TaskDone ()

{-|-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  let checkRank new =  0 < new && new <= Game.maxRank
      changeRankFx rank = Signal.send model.game (Game.Rank rank)
                    |> Effects.task
                    |> Effects.map TaskDone
      showErrorFx msg = Signal.send model.game (Game.ShowError msg)
                    |> Effects.task
                    |> Effects.map TaskDone

  in
    case action |> Debug.log "dash_act" of
      Increment ->
        if checkRank (model.rank + 1)
        then ({ model | rank = model.rank + 1 }, changeRankFx (model.rank + 1))
        else (model, showErrorFx "Max rank!" )
      Decrement ->
        if checkRank (model.rank - 1)
        then ({ model | rank = model.rank - 1 } , changeRankFx (model.rank - 1))
        else (model, showErrorFx "Min rank!")
      Resize width ->
        ( { model | width = width }, Effects.none)
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
                 --  button [ onClick address Decrement ] [ text "-" ]
                 -- , div [ textStyle ] [ text (toString model.rank) ]
                 -- , button [ onClick address Increment ] [ text "+" ]
  in div [ style ["height" => "25vw"] ]
     [ rank
       -- picters
     , div [ style [ "float" => "left"
                   , "width" => "47vw"
                   , "height" => "25vw"
                   , "border-radius" => "15vw"
                   , "border" => "1px solid blue"
                   ]] []
     , div [ style [ "float" =>  "right"
                   , "width" =>  "25vw"
                   , "height" =>  "25vw"
                   , "border-radius" =>  "15vw"
                   , "background-color" => "red"
                   ]
           -- FIXME: onClick
           ] []
     ]

textStyle : Attribute
textStyle =
  style [ "font-size" => "12vw"
        , "font-family" => "monospace"
        , "width" => "25vw"
        , "text-align" => "center"
        ]
