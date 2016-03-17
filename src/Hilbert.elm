module Hilbert (Model, init, Action(Order, Resize, FreeMode), update, view) where

{-|

@docs Model, init, Action, update, view

The Hilbert Curve can be expressed by a rewrite system (L-system).

Alphabet : A, B
 Constants : F + −
Axiom : A
Production rules:
  A → − B F + A F A + F B −
  B → + A F − B F B − F A +

Here, "F" means "draw forward",
      "−" means "turn left 90°",
      "+" means "turn right 90°"
      "A" and "B" are ignored during drawing.

-}

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Html exposing (Html, div, fromElement, text)
import Html.Attributes exposing (style)

import Effects exposing (..)

import String
import Dict

import LSystem exposing (generation, LSystem)
import Game exposing (Events)
import Teremin


{-| L-system rules for Hilbert crve -}
hilbert : LSystem
hilbert =
  { axiom = [ 'A' ],
    rules = [ ('A', String.toList "-BF+AFA+FB-")
            , ('B', String.toList "+AF-BFB-FA+") ] |> Dict.fromList
  }

computePath order = LSystem.generation order hilbert
                  |> .axiom
                  |> List.filter (\c -> c /= 'A' && c /= 'B')
{-|
-}
type alias Model =
  { game : Signal.Address Events
  , order : Int
  , path : List Char
  , dimention : Int
  , freeMode : Bool
  }

-- init : Int -> (Int,Int) ->(Int,Int) -> Model
{-|
-}
init : Signal.Address Events
     -> Int
     -> Int -> (Model, Effects Action)
init game order dimention =
  ({ game = game
   , order = order
   , path = computePath order
   , dimention = dimention
   , freeMode = False
   }, Effects.none)

{-|
-}
type Action
  = Resize Int
  | Order Int
  | FreeMode Bool

{-|
-}
update : Action -> Model -> (Model, Effects Action)
update act model =
  case act |> Debug.log "hilb_act" of
    Resize w ->
      ( { model | dimention = w }, Effects.none )
    Order order ->
      ( {model | order = order, path = computePath order }, Effects.none )

    FreeMode flag ->
      let
        _ = if not flag then Teremin.startOsc 1 else Teremin.stopOsc 0
      in
        ( {model | freeMode = flag}
        , Effects.none)

(=>) = (,)
{-|
-}
view : Signal.Address Action -> Model -> Html
view address model =
  let
    side = (toFloat model.dimention) / toFloat (2^model.order)
    offset = (toFloat model.dimention / 2)
    hpath = drawHilbert offset side model.path
    draw = collage model.dimention model.dimention
           [ traced (solid black) (path [ (offset,offset),
                                          (offset,-offset),
                                          (-offset,-offset),
                                          (-offset,offset),
                                          (offset,offset) ])
           , traced (solid red) hpath
           ]
  in
  div [ style [ "height" => "100vw"
              , "width" => "100vw"
              , "border-radius" => "10vw"
              , "background-color" => "yellow"
        ]] [ fromElement draw ]

{-|
direction (x, y) - unit vector
(x, y) rotated -90 degrees around (0, 0) is (-y, x), rotate clockwise (y, -x)
-}
-- drawHilbert : Float -> Float -> List Char -> Path
drawHilbert offset side p =
  (List.foldr (turttle side) ((1, 0), [(-offset+side/2, -offset+side/2)]) p)
    |> snd
    |> path

turttle side instruction ((dx, dy), path) =
  case (instruction, path) of
    ('F', (x,y)::_) -> ((dx, dy), (  (x + dx * side),  (y + dy * side)) :: path)
    ('-', _) -> ((-dy,dx), path)
    ('+', _) -> ((dy,-dx), path)
    otherwise -> Debug.crash (instruction `String.cons` " - unsuported instruction")
