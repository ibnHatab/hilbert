module Teremin (init) where

{-|
@docs init

-}

import Native.Teremin

{-|-}
init : Int -> ()
init =
  Native.Teremin.init
