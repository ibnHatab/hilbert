module LSystem where

import Dict exposing (Dict)
import List exposing (..)
import String exposing (toList)

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
      newState = List.concatMap (srinker) ls.axiom
  in
    { ls | axiom = newState }

iterate : Int -> state -> (state -> state) -> state
iterate n s f  = if n == 0 then s
                 else iterate (n-1) (f s) f

{-| compute nth generation of lSystem -}
generation : Int -> LSystem -> LSystem
generation gen ls = iterate gen ls evolve


-- serpinski : LSystem
-- serpinski =
--   { axiom = [ 'A' ],
--     rules = [ ('A', [ 'B', '>', 'A', '>', 'B' ])
--             , ('B', [ 'A', '<', 'B', '<', 'A' ]) ] |> Dict.fromList
--   }

-- hilbert : LSystem
-- hilbert =
--   { axiom = [ 'A' ],
--     rules = [ ('A', [ '−', 'B', 'F', '+', 'A', 'F', 'A', '+', 'F', 'B', '−'])
--             , ('B', [ '+', 'A', 'F', '−', 'B', 'F', 'B', '−', 'F', 'A', '+']) ]
--           |> Dict.fromList
--   }
(=>) a r = (a, toList r)
hilbert = { axiom = ['L'], rules = ['L' => "+RF-LFL-FR+", 'R' => "-LF+RFR+FL-"] |> Dict.fromList }
