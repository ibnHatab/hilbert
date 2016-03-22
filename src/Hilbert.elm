module Hilbert (Model, Action(..), init, update, view, hilbertDistance, hilbertPoint) where

{-|

@docs Model, Action, init, update, view

@docs hilbertDistance, hilbertPoint
-}

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Html exposing (Html, div, fromElement, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)

import Effects exposing (..)

import Bitwise exposing (..)
import String exposing (toList)
import Dict


import LSystem exposing (generation, LSystem)
import Game exposing (Events)
import Braille
import Teremin

import Time exposing (Time, second)


-- MODEL

{-|
-}
type alias Model =
  { path : List Char
  , play : Bool
  , mousePosition : (Int, Int)
  , markers : List Int
  , dots : List (Int, Int)
  , animationState : AnimationState
  -- last
  , game : Game.Model
  , gameFx : Signal.Address Events
  }

duration = 4 * second

type alias AnimationState = Maybe
  { prevClockTime : Time
  , elapsedTime : Time
  , current : Int
  }

{-|
-}
init : Game.Model -> Signal.Address Events
     -> (Model, Effects Action)
init game gameFx =
  let _ = Teremin.setVolume 0.2
      _ = Teremin.setFrequency 10 100
  in
  ({ path = computePath game.order
   , play = False
   , mousePosition = (0,0)
   -- animation
   , markers = []
   , dots = []
   , animationState = Nothing
   -- last
   , game = game
   , gameFx = gameFx
   }, Effects.none)

{-|
-}
type Action
  = Game Game.Events
  | MousePos (Int, Int)
  | TouchPos (List (Int, Int))
  | Tick Time
  | PlayStart
  | PlayStop


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then dropWhile predicate xs
               else list

{-|
-}
update : Action -> Model -> (Model, Effects Action)
update act model =
  let notifyFx event = Signal.send model.gameFx event
                 |> Effects.task
                 |> Effects.map (Game << Game.TaskDone)
  in
    case act |> Debug.log "hilb_act"
    of

      Tick clockTime ->
        if model.game.state == Game.ChooseOne
        then
          let area = 2^ (2*model.game.order)
              (newElapsedTime, lastCurrent) =
              case model.animationState of
                Nothing ->
                  (0, 0)
                Just { prevClockTime, elapsedTime, current} ->
                  (elapsedTime + ( clockTime - prevClockTime ), current)
          in
          if lastCurrent == (area - 1) then
            let _ = Teremin.stopOsc 0
            in
            ({ model | animationState = Nothing }, Effects.none)
          else
            let current = if newElapsedTime == 0 then 0
                          else if newElapsedTime > duration
                               then (area - 1)
                               else floor ( (toFloat area) *
                                            (newElapsedTime/duration) )
          in
            ( { model |
                animationState = Just { elapsedTime = newElapsedTime
                                      , prevClockTime = clockTime
                                      , current = current
                                      }
              , markers = dropWhile ((>) current) model.markers
              }
            , Effects.tick Tick )
        else
          ({model | animationState = Nothing }, Effects.none )

      MousePos (ox, oy) ->
        let
          (w, _) = model.game.geometry
          offset = round (toFloat w * 1.32)
          -- sum offsets from top ~ 32vw percentage
          (x, y) = (ox, offset - oy)
        in
          ({ model | mousePosition = (x, y) }, Effects.none)

      TouchPos (t::ts) ->
        let
          (ox,oy) = t
          (w, _) = model.game.geometry
          offset = round (toFloat w * 1.32)
          -- sum offsets from top ~ 32vw percentage
          (x, y) = (ox, offset - oy)
        in
          ({ model | mousePosition = (x, y) }, Effects.none)

      PlayStart ->
        let  _ = Teremin.startOsc 1
        in ({ model | play = True }, Effects.none)

      PlayStop ->
        let  _ = Teremin.stopOsc 0
        in ({ model | play = False }, Effects.none)

      Game (Game.Order order) ->
        ({ model | path = computePath model.game.order }, Effects.none)

      Game (Game.StateChange old new) ->
        case (old, new) of
          (Game.ChooseOne, Game.FreeMode) ->
            let _ = Teremin.stopOsc 0
                _ = Teremin.setVolume 0.2
            in
              ( {model | dots = []}, Effects.none )

          (_,_) ->
            (model, Effects.none )

      Game (Game.PlayHint) ->
        if model.game.state == Game.ChooseOne
        then let
          h3dots : Int -> (Int,Int)
          h3dots n = case n of
                       1 -> (0,3)
                       2 -> (0,2)
                       3 -> (0,1)
                       4 -> (1,3)
                       5 -> (1,2)
                       6 -> (1,1)
                       7 -> (0,0)
                       8 -> (0,0)
                       _ -> (-1,-1)
          braille2dot : Int -> Char -> List (Int,Int)
          braille2dot n c = List.map h3dots (Braille.dots c)
                          |> List.map (\(x,y) -> (x*2 + 4*n, y*2))

          an = List.head model.game.answer |> Maybe.withDefault 0
          (markers, dots) = (String.slice an (an+1) model.game.question)
                          |> String.toList
                          |> List.map (braille2dot an)
                          |> List.concat
                          |> List.map (\d -> (hilbertDistance model.game.order d, d))
                          |> List.sortBy fst
                          |> List.unzip
                          |> Debug.log "dots"

                    --          firstFreq = List.head dots
          _ = Teremin.startOsc 1
          _ = Teremin.setVolume 0.0
          _ = Teremin.setFrequency 1000 100
  --        _ = Teremin.setFrequency current (2^(model.game.order*2))

        in ( {model | markers = markers, dots = dots}, Effects.tick Tick )
        else (model, Effects.none )
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

    cursor = if not model.play then []
             else [ circle (side * 0.5)
                      |> filled blue
                      |> move ((toFloat x) - offset, (toFloat y) - offset)
                  ]

    dot color (x, y) = circle (side * 0.4)
                     |> filled color
                     |> move ( (toFloat x) * side + side / 2 - offset
                             , (toFloat y) * side + side / 2 - offset)

    pointer = case model.animationState of
                Nothing -> []
                Just { current } -> [ dot red (hilbertPoint model.game.order current)]


    dots = List.map (dot blue) model.dots

    draw = List.concat [
            [ traced (dashed black) (path [ (offset,offset),
                                            (offset,-offset),
                                            (-offset,-offset),
                                            (-offset,offset),
                                            (offset,offset) ])
            , traced (solid red) hpath
            ]
           , dots
           , cursor
           , pointer
           ]
         |> collage w w

    -- make a sound
    _ = case model.animationState of
          Nothing -> ()
          Just { current } ->
            let
              area = 2^ (2*model.game.order)
              _ = case List.head model.markers of
                    Nothing ->
                      Teremin.setVolume (0.2 * (1 - (toFloat current) / (toFloat area)))

                    Just m ->
                      if current == m
                      then
                        let
                          _ = Teremin.setVolume 0.2
                          _ = Teremin.setFrequency current area
                        in ()
                      else  ()
                      -- Teremin.setVolume 0.4
            in ()

    _ = if model.play
        then
          let
            scale = round side
            distance = hilbertDistance
                       model.game.order
                       (x // scale, y // scale)
          in Teremin.setFrequency distance (2^(model.game.order*2))
        else ()
  in
  div [ style [ "height" => "100vw"
              , "width" => "100"
              ]
      , onMouseDown address PlayStart
      , onMouseUp address PlayStop
      , onDoubleClick address (Game Game.PlayHint)]
  [ fromElement draw
  ]

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
