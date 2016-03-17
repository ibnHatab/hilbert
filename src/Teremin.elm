module Teremin (init, startOsc, stopOsc, setFrequency, setVolume) where

{-|
@docs init, startOsc, stopOsc, setFrequency, setVolume

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

{-|-}
setFrequency : Int -> Int -> ()
setFrequency = Native.Teremin.setFrequency

{-|-}
setVolume : Int -> ()
setVolume = Native.Teremin.setVolume
