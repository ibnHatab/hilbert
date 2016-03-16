module Teremin (init, startOsc, stopOsc) where

{-|
@docs init, startOsc, stopOsc

-}

import Native.Teremin

{-|-}
init : Int -> ()
init =
  Native.Teremin.init

{-|-}
startOsc : Int -> ()
startOsc =
  Native.Teremin.startOsc

{-|-}
stopOsc : Int -> ()
stopOsc =
  Native.Teremin.stopOsc
