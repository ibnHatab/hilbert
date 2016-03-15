module Dash (Action(Resize, Error), Model, init, update, view) where

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
  , errorMessage : Maybe String
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
    , errorMessage = Nothing
    , width = width
    }
  , Effects.none
  )

-- UPDATE

{-|-}
type Action = Increment | Decrement | Resize Int | Error String | TaskDone ()

{-|-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  let checkRank new =  0 < new && new <= Game.maxRank
      changeRankFx rank = Signal.send model.game (Game.Rank (rank))
                    |> Effects.task
                    |> Effects.map TaskDone
  in
    case action |> Debug.log "dash_act" of
      Increment ->
        if checkRank (model.rank + 1)
        then ({ model | rank = model.rank + 1 }, changeRankFx (model.rank + 1))
        else ({ model | errorMessage = Just "Max rank!"}, Effects.none )
      Decrement ->
        if checkRank (model.rank - 1)
        then ({ model | rank = model.rank - 1 } , changeRankFx (model.rank - 1))
        else ({ model | errorMessage = Just "Min rank!"}, Effects.none )
      Resize width ->
        ( { model | width = width }, Effects.none)

      Error errorMessage ->
        ( { model | errorMessage = Just errorMessage }
        , Effects.none
        )
      TaskDone () ->
        ( model, Effects.none )

-- VIEW

{-|-}
view : Signal.Address Action -> Model -> Html
view address model =
  let rankDiv =  div []
                 [ button [ onClick address Decrement ] [ text "-" ]
                 , div [ countStyle ] [ text (toString model.rank) ]
                 , button [ onClick address Increment ] [ text "+" ]
                 ]
      errMsgDiv = div [] [ text (Maybe.withDefault "" model.errorMessage)]
      glifDiv   = div [  ] [ text "Glif" ]
      redButton = div [  ] [ text "Red button" ]
  in div []
     [ rankDiv
     , errMsgDiv
     , glifDiv
     , redButton
     ]

countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]
