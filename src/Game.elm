module Game (Model, Events(..), State(..), Message(..), init, update, constants, GameConstants) where

{-| Game engine

@docs Model, Events, State, Message, init, update, constants, GameConstants

-}
import Effects exposing (Effects, map, batch, Never)
import Random
import Char
import String
import Debug

{-|-}
type alias GameConstants = {maxRank : Int, initOrder : Int, geometry : (Int, Int)}
{-|-}
constants : GameConstants
constants =
  { maxRank = 6
  , initOrder = 3
  , geometry = (400, 600)
  }

{-| Game state we are in
-}
type State = Begin | FreeMode | ChooseOne | TypeAnswer

{-| Game MODEL
-}
type alias Model
  = { order    : Int
    , state    : State
    , geometry : (Int, Int)
    , question : String
    , answer   : List Int
    , seed     : Random.Seed
    -- last
    , gameFx : Signal.Address Events
    }


{-| Compute initial game state
-}
init : Signal.Address Events ->
       (Model, Effects Events)
init gameFx =
  let seed0 = Random.initialSeed 31415
  in
  ( { order = constants.initOrder
    , state = Begin
    , geometry = constants.geometry
    , question = ""
    , answer = []
    , seed = seed0
    -- last
    , gameFx = gameFx
    }
  , Effects.none
  )

{-| Display game messages
-}
type Message = Error String
             | Info String
             | Clear

{-| Game Brodcast
-}
type Events
  = Order Int             -- display resolution as feeling curve order
  | Geometry (Int, Int)
  | StateChange State State
  | Display Message
  | AskQuestion String
  | GiveAnswer String
  | PlayHint
  | TaskDone ()


generateQnA len =
  let
    rlst = Random.list len (Random.int 0x20 0x5F
                            |> Random.map Char.fromCode)
    rans = Random.int 0 len
  in Random.pair rlst rans

{-| Calculate new game state
-}
update : Events -> Model -> (Model, Effects Events)
update act model =
  let
    notifyFx event = Signal.send model.gameFx event
                   |> Effects.task
                   |> Effects.map TaskDone

  in
    case act |> Debug.log "m_game" of
      Order order ->
        ({ model | order = order}, Effects.none)
      Geometry dim ->
        ({ model | geometry = dim }, Effects.none)
      StateChange old new ->
        case (old, new) of
          (_, ChooseOne) ->
            let
              ((q, a), seed) = Random.generate (generateQnA 2) model.seed
              question = String.fromList q
            in
              ({ model | state = new
               , question = question
               , answer = [a]
               , seed = seed
               },
               Effects.batch [ notifyFx (Display (Info ("Choose One of: [" ++ question ++ "]")))
                             , notifyFx (AskQuestion question)
                             , notifyFx (PlayHint)
                             ])
          (ChooseOne, _) ->
            ({ model | state = new
             , question = ""
             , answer = []
             },
             Effects.batch [notifyFx (Display (Info "Game over!"))])
          (_, _) ->
            ({ model | state = new }, Effects.none)

      AskQuestion q ->
        ({model | question = q}, Effects.none)

      GiveAnswer _ ->
        (model, Effects.none)


      -- ignored events
      PlayHint ->
        (model, Effects.none)
      Display _ ->
        (model, Effects.none)
      TaskDone () ->
        (model, Effects.none)
