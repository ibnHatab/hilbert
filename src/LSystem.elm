module LSystem where

import Dict exposing (Dict)
import List exposing (..)

type alias Symbol = Char
type alias State = List Symbol
type alias Rules = Dict Symbol State

type alias LSystem = { axiom : State
                     , rules : Rules }

-- Growing from original axion by applying the rules
applyRules : Rules -> Symbol -> State
applyRules rs s =
  case Dict.get s rs of
    Nothing -> [s]
    Just x  ->  x

evolve : LSystem -> LSystem
evolve ls =
  let srinker symbol = applyRules ls.rules symbol
      newState = List.map (srinker) ls.axiom |> List.concat
  in
    { ls | axiom = newState }

unfold : Int -> (a -> a) -> a -> List a
unfold n f x = if n==0 then [] else
  let res=f x in (res :: unfold (n-1) f res)

-- forward : Int -> (LSystem -> LSystem) -> LSystem ->
forward gen ls =
  unfold gen evolve ls


-- compute nth generation of lSystem
-- generation : Int -> LSystem -> LSystem
-- generation gen ls =

--   ls
--   |> forward
--   |> List.drop gen
--   |> List.head
--   |> Maybe.withDefault ls


serpinski : LSystem
serpinski =
  { axiom = [ 'A' ],
    rules = [ ('A', [ 'B', '>', 'A', '>', 'B' ])
            , ('B', [ 'A', '<', 'B', '<', 'A' ]) ] |> Dict.fromList
  }
