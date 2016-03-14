module Hilbert (Model, init, Action, update, view) where

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

import String exposing (toList)
import Effects exposing (..)

import Dict
import LSystem exposing (generation, LSystem)


{-| L-system rules for Hilbert crve -}
hilbert : LSystem
hilbert =
  { axiom = [ 'A' ],
    rules = [ ('A', toList "-BF+AFA+FB-")
            , ('B', toList "+AF-BFB-FA+") ] |> Dict.fromList
  }

{-|
-}
type alias Model =
  { order : Int
  , path : List Char
  , dimention : Int
  }

-- init : Int -> (Int,Int) ->(Int,Int) -> Model
{-|
-}
init : Int -> Int -> (Model, Effects Action)
init order dimention =
  let
    path = LSystem.generation order hilbert
         |> .axiom
         |> List.filter (\c -> c /= 'A' && c /= 'B')
  in ({ order = order
      , path = path
      , dimention = dimention
     }, Effects.none)


{-|
-}
type Action
  = Resize Int

{-|
-}
update : Action -> Model -> (Model, Effects Action)
update act model =
  case act of
    Resize w ->
      ( model, Effects.none )


(=>) = (,)
{-|
-}
view : Signal.Address Action -> Model -> Html
view address model =
  let
    side = (toFloat model.dimention) / (toFloat (2^model.order))
    hpath = drawHilbert side model.path
    draw = collage model.dimention model.dimention [traced (solid red) hpath]
  in
  div [] [ fromElement draw ]

{-|
direction (x, y) - unit vector
(x, y) rotated -90 degrees around (0, 0) is (-y, x), rotate clockwise (y, -x)
-}
drawHilbert : Float -> List Char -> Path
drawHilbert side p =
  (List.foldr (turttle side) ((1, 0), [(0.0, 0.0)]) p) |> snd |> path

turttle side instruction ((dx, dy), path) =
  case (instruction, path) of
    ('F', (x,y)::_) -> ((dx, dy), (  (x + dx * side),  (y + dy * side)) :: path)
    ('-', _) -> ((-dy,dx), path)
    ('+', _) -> ((dy,-dx), path)
    otherwise -> Debug.crash (instruction `String.cons` " - unsuported instruction")

-- {-|-}
-- app = StartApp.start
--       { init = init
--       , update = update
--       , view = view
--       , inputs = [Window.dimensions]
--       }


{-| -----------------------------  testing --------------
-}
-- main : Signal Html
-- main = app.html

--

{-| Hilbert_Distance to point on plaine -}
-- hilbertDistance d (x,y) =
--   let dist side area result x y =
--         if side == 0 then result else
--         let step = dist (side `shiftR` 1) (area `shiftR` 2)
--         in case (compare x side, compare y side) of
--              (LT, LT) -> step result y x
--              (LT, _)  -> step (result + area) x (y - side)
--              (_, LT)  -> step (result + area * 3) (side - y - 1) (side * 2 - x - 1)
--              (_, _)   -> step (result + area * 2) (x - side) (y - side)
--   in dist (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) 0 x y


-- hilbertPoint d n
--   | n >= 1 `shiftL` (2*d) = error "x bounds"
--   | otherwise = poin (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) (0, 0) n
--   where poin 0 _ (x, y) n = (x, y)
--         poin side area (x, y) n =
--           case n `divMod` area  of
--           (0, rest) -> step (x, y) rest
--           (1, rest) -> step (x, y+side) rest
--           (2, rest) -> step (x+side, y+side) rest
--           (3, rest) -> step (x+side, y) rest
--           where step = poin (side `shiftR` 1) (area `shiftR` 2)
