module Braille (braileASCII, glyph) where
{-|
  The North American Braille ASCII Code
  https://en.wikipedia.org/wiki/Braille_ASCII

@docs braileASCII, glyph

-}
import Svg exposing (svg, rect, g, text, text', circle)
import Svg.Attributes exposing (..)
import Html exposing (Html)

import Dict exposing (Dict)
import Array


(=>) prop val = prop (toString val)
{-|
 Glyph structures as:
 (1) (4)
 (2) (5)
 (3) (6)
 (7) (8)
 -}
glyph : List Int -> Int -> Html
glyph dots side =
  let
    (w, h) = (2 * side, 4 * side)
    radius = side // 3
    places = [ ((side // 2), (side // 2))                 -- (1)
             , ((side // 2), (side // 2) + side)          -- (2)
             , ((side // 2), (side // 2) + 2*side)        -- (3)
             , ((side // 2) + side, (side // 2) )         -- (4)
             , ((side // 2) + side , (side // 2) + side)  -- (5)
             , ((side // 2) + side, (side // 2) + 2*side) -- (6)
             , ((side // 2), (side // 2) + 3*side)        -- (7)
             , ((side // 2) + side, (side // 2) + 3*side) -- (8)
             ]
    fills = Array.initialize 8 (\n -> if List.member (n+1) dots
                                      then "black"
                                      else "none") |> Array.toList
    allDots = List.map2 (\(x,y) f -> [ cx => x,
                                       cy => y,
                                       r => radius,
                                       fill f, stroke "black"]
              ) places fills
    bg = rect [ x => 0, y => 0, width => w, height => h
              , rx => radius, ry => radius, style "fill: #60B5CC;"
              ] []
  in svg [ width => w,
           height => h,
           viewBox ("0 0 " ++ (toString w) ++ " " ++ (toString h))
       ] (bg :: List.map ((flip circle)[]) allDots)

{-| Braille ASCII table
Hex	ASCII Glyph	Braille Dots	Braille Meaning
-}
braileASCII : Dict Int (Char, List Int, String)
braileASCII = [
 ( 21, ('!', [2,3,4,6], "the"))

 ] |> Dict.fromList
