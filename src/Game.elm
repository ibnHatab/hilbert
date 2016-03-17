module Game (Events(Rank, FreeMode, ShowError), maxRank) where

{-| Game engine

@docs Events, maxRank

-}
import Debug

{-|
-}
type Events
  = Rank Int
  | FreeMode Bool
  | ShowError String

{-|
Game constants
-}
maxRank : Int
maxRank = 6
