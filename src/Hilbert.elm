module Hilbert (Model, Action(..), init, update, view) where

{-|

@docs Model, Action, init, update, view

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
  { path : List Char
  , play : Bool
  , mousePosition : (Int, Int)
  -- last
  , game : Game.Model
  , gameFx : Signal.Address Events
  }

{-|
-}
init : Game.Model -> Signal.Address Events
     -> (Model, Effects Action)
init game gameFx =
  let _ = Teremin.setVolume 0.0
      _ = Teremin.setFrequency 10 100
  in
  ({ path = computePath game.order
   , play = False
   , mousePosition = (0,0)
   -- last
   , game = game
   , gameFx = gameFx
   }, Effects.none)

{-|
-}
type Action
  = Game Game.Events
  | MousePos (Int, Int)
  | Tick Time
  | PlayStart
  | PlayStop

{-|
-}
update : Action -> Model -> (Model, Effects Action)
update act model =
  case act |> Debug.log "hilb_act" of
    Tick clockTime ->
      (model, Effects.tick Tick )

    MousePos (ox, oy) ->
      let
        (w, _) = model.game.geometry |> Debug.log ">> "
        side = round <| (toFloat w) / toFloat (2^model.game.order)
        offset = round (toFloat w * 1.32) -- sum offsets from top ~ 32vw percentage
        (x, y) = (ox, offset - oy)
        distance = hilbertDistance model.game.order (x // side, y // side)
        _ = if model.play then Teremin.setFrequency distance (2^(model.game.order*2))  else ()
      in
        ( { model | mousePosition = (x, y) }, Effects.none )

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

    Game (Game.Order order) ->
      ( {model | path = computePath model.game.order }, Effects.none )

    Game (Game.StateChange old new) ->
      case (old, new) of
        (_, Game.FreeMode) ->
          let _ = Teremin.setVolume 0.2 in
          (model, Effects.none )
        (Game.FreeMode, _) ->
          let _ = Teremin.setVolume 0.0 in
          (model, Effects.none )
        (_,_) ->
          (model, Effects.none )

    otherwise ->
      (model, Effects.none )


(=>) = (,)
{-|
-}
view : Signal.Address Action -> Model -> Html
view address model =
  let
    (w, _) = model.game.geometry
    (x, y) = model.mousePosition
    side = (toFloat w) / toFloat (2^model.game.order)
    offset = (toFloat w / 2)
    hpath = drawHilbert offset side model.path
    draw = collage w w <|
           [ traced (dashed black) (path [ (offset,offset),
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
              , "width" => "100"
              ]
      , onMouseDown address PlayStart
      , onMouseUp address PlayStop ]
  [ fromElement draw
  ]

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
