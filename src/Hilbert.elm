module Hilbert (Model, init, Action, update, view) where

{-|

@docs Model, init, Action, update, view

-}

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Model =
  { lsystem: LSystem
  }

init : Int -> Model


type Action

update : Action -> Model -> Model

view : Signal.Address Action -> Model -> Html

{-|-}
main : Element
main =
  collage 300 300
    [ hexagon red
    , hexagon purple
        |> scale 2
    , hexagon green
        |> move (100,0)
    , hexagon blue
        |> rotate (degrees 30)
    ]

{-| -}
hexagon : Color -> Form
hexagon clr =
  outlined (solid clr) (ngon 6 40)
