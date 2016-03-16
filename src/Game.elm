module Game where

{-| Game engine

@docs Events, maxRank

-}

{-|
-}
type Events
  = Rank Int

{-|
-}
maxRank : Int
maxRank = 5


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
