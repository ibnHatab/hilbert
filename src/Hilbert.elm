module Hilbert -- (Model, init, Action, update, view)
  where

{-|

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


@docs Model, init, Action, update, view

-}

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)



type alias Model =
  { path : List Char
  }

init : Int -> Model
init gen origin dimention =
  let
    hilbertPath = LSystem.generation gen hilbert
                |> .axiom
                |> List.filter (\c -> c != 'A' and c != 'B')
  in { path = hilbertPath }


type Action

update : Action -> Model -> Model

view : Signal.Address Action -> Model -> Html
view =


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

--
{-| L-system rules for Hilbert crve -}
hilbert : LSystem
hilbert =
  { axiom = [ 'A' ],
    rules = [ ('A', [ '−', 'B', 'F', '+', 'A', 'F', 'A', '+', 'F', 'B', '−'])
            , ('B', [ '+', 'A', 'F', '−', 'B', 'F', 'B', '−', 'F', 'A', '+']) ]
          |> Dict.fromList
  }

{-| Hilbert_Distance to point on plaine -}
hilbertDistance d (x,y) =
  let dist 0 _ result _ _ = result
      dist side area result x y =
        let step = dist (side `shiftR` 1) (area `shiftR` 2)
        in case (compare x side, compare y side) of
             (LT, LT) -> step result y x
             (LT, _)  -> step (result + area) x (y - side)
             (_, LT)  -> step (result + area * 3) (side - y - 1) (side * 2 - x - 1)
             (_, _)   -> step (result + area * 2) (x - side) (y - side)
  in dist (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) 0 x y


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
