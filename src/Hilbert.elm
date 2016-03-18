module Hilbert (Model, init, Action(Order, Resize, FreeMode, MousePos), update, view) where

{-|

@docs Model, init, Action, update, view

-}

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Html exposing (Html, div, fromElement, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)

import Effects exposing (..)

import Bitwise exposing (..)
import String
import Dict

import LSystem exposing (generation, LSystem)
import Game exposing (Events)
import Teremin

import Time exposing (Time, second)

{-| L-system rules for Hilbert crve

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
hilbert : LSystem
hilbert =
  { axiom = [ 'A' ],
    rules = [ ('A', String.toList "-BF+AFA+FB-")
            , ('B', String.toList "+AF-BFB-FA+") ] |> Dict.fromList
  }

computePath order = LSystem.generation order hilbert
                  |> .axiom
                  |> List.filter (\c -> c /= 'A' && c /= 'B')

{-| Hilbert Distance to point on plaine -}
hilbertDistance : Int -> (Int, Int) -> Int
hilbertDistance d (x,y) =
  let dist side area result x y =
      if side == 0 then result else
        let step = dist (side `shiftRight` 1) (area `shiftRight` 2)
        in case (compare x side, compare y side) of
             (LT, LT) -> step result y x
             (LT, _)  -> step (result + area) x (y - side)
             (_, LT)  -> step (result + area * 3) (side - y - 1) (side * 2 - x - 1)
             (_, _)   -> step (result + area * 2) (x - side) (y - side)
  in dist (1 `shiftLeft` (d - 1)) (1 `shiftLeft` ((d - 1) * 2)) 0 x y


{-| point on plaine from Hilbert Distance -}
hilbertPoint : Int -> Int -> (Int, Int)
hilbertPoint d n =
  let point side area (x, y) n =
        if side == 0 then (x, y) else
          let step = point (side `shiftRight` 1) (area `shiftRight` 2)
              divMod x y = (x // y, x % y)
          in case n `divMod` area  of
               (0, rest) -> step (x, y) rest
               (1, rest) -> step (x, y+side) rest
               (2, rest) -> step (x+side, y+side) rest
               (3, rest) -> step (x+side, y) rest
               otherwise -> Debug.crash "wrong devider"
  in point (1 `shiftLeft` (d - 1)) (1 `shiftLeft` ((d - 1) * 2)) (0, 0) n


-- MODEL

{-|
-}
type alias Model =
  { game : Signal.Address Events
  , order : Int
  , path : List Char
  , dimention : (Int, Int)
  , freeMode : Bool
  , play : Bool
  , mousePosition : (Int, Int)
  }

-- init : Int -> (Int,Int) ->(Int,Int) -> Model
{-|
-}
init : Signal.Address Events -> Int -> (Int,Int)
     -> (Model, Effects Action)
init game order dimention =
  ({ game = game
   , order = order
   , path = computePath order
   , dimention = dimention
   , freeMode = False
   , play = False
   , mousePosition = (0,0)
   }, Effects.none)

{-|
-}
type Action
  = Resize (Int, Int)
  | Order Int
  | FreeMode Bool
  | MousePos (Int, Int)
  | Tick Time
  | PlayStart
  | PlayStop

{-|
-}
update : Action -> Model -> (Model, Effects Action)
update act model =
  case act |> Debug.log "hilb_act" of
    Resize dim ->
      ( { model | dimention = dim }, Effects.none )
    Order order ->
      ( {model | order = order, path = computePath order }, Effects.none )

    FreeMode flag ->
        ( {model | freeMode = flag}, Effects.none)

    MousePos (ox, oy) ->
      let
        (w, _) = model.dimention
        side = round <| (toFloat w) / toFloat (2^model.order)
        -- sum of offses in 32vw percentage from page top to canvas left-bottom
        offset = round (toFloat w * 1.32)
        (x, y) = (ox, offset - oy) |> Debug.log "xy"
        distance = hilbertDistance model.order (x  // side, y  // side) |> Debug.log "dist"
        _ = if model.play then Teremin.setFrequency distance (2^(model.order*2))  else ()
      in
        ( { model | mousePosition = (x, y) }, Effects.none )

    Tick clockTime ->
      (model, Effects.tick Tick )

    PlayStart ->
      let
        _ = Teremin.startOsc 1
      in
      ( { model | play = True }, Effects.none )

    PlayStop ->
      let
        _ = Teremin.stopOsc 0
      in
      ( { model | play = False }, Effects.none )

(=>) = (,)
{-|
-}
view : Signal.Address Action -> Model -> Html
view address model =
  let
    (w, _) = model.dimention
    (x, y) = model.mousePosition
    side = (toFloat w) / toFloat (2^model.order)
    offset = (toFloat w / 2)
    hpath = drawHilbert offset side model.path
    draw = collage w w <|
           [ traced (solid black) (path [ (offset,offset),
                                          (offset,-offset),
                                          (-offset,-offset),
                                          (-offset,offset),
                                          (offset,offset) ])
           , traced (solid red) hpath
           ]
           ++ if model.play
              then [
               circle (side * 0.7)
                 |> filled blue
                 |> move ((toFloat x) - offset, (toFloat y) - offset)
              ]
              else []
  in
  div [ style [ "height" => "100vw"
              , "width" => "100vw"
              , "border-radius" => "10vw"
              , "background-color" => "yellow"
        ]
      , onMouseDown address PlayStart
      , onMouseUp address PlayStop ]
  [ fromElement draw ]

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
