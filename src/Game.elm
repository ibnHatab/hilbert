module Game (Model, Events(..), init, maxRank) where

{-| Game engine

@docs Model, Events, maxRank, init

-}
import Effects exposing (Effects, map, batch, Never)

import Debug

{-|
Game constants
-}
maxRank : Int
maxRank = 6

{-| Game MODEL
-}
type alias Model
  = { game : Signal.Address Events
    , order : Int
    , question : String
    }

{-|-}
init : Signal.Address Events -> Int ->
       (Model, Effects Events)
init game order =
  ({ game = game
   , order = order
   , question = ""}, Effects.none )


{-|
-}
type Events
  = Rank Int
  | FreeMode Bool
  | ShowError String
  | Question String
  | Answer String

step : Events -> Model -> (Model, Effects Events)
step message model =
  case message |> Debug.log "m_game" of
    Question q ->
      ({ model | question = q }, Effects.none)
    Answer a ->
      (model, Effects.none)
    otherwise ->
      (model, Effects.none)
