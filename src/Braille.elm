module Braille (braileASCII, glyph, dots) where
{-|
  The North American Braille ASCII Code
  https://en.wikipedia.org/wiki/Braille_ASCII

@docs braileASCII, glyph, dots

-}
import Svg exposing (svg, rect, g, text, text', circle)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

import Html exposing (Html)

import Dict exposing (Dict)
import Array
import Char

(=>) prop val = prop (toString val)
{-|
 Glyph structures as:
 (1) (4)
 (2) (5)
 (3) (6)
 (7) (8)
 -}
glyph : Int -> List Int -> Maybe Signal.Message -> Html
glyph side dots sig =
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
              , rx => radius, ry => radius, style "fill: grey"
              -- "fill: #60B5CC;"
              ] []
    allImg = (bg :: List.map ((flip circle)[]) allDots)
    lst = case sig of
            Just hnd ->
              [g [onClick hnd] allImg]
            Nothing -> allImg

  in svg [ width => w,
           height => h,
           viewBox ("0 0 " ++ (toString w) ++ " " ++ (toString h))
       ] lst


{-| Dots for ASCII code -}
dots : Char -> List Int
dots c = case Dict.get (Char.toCode c) braileASCII of
           Just (_, ds, _) -> ds
           Nothing -> []

{-| Braille ASCII table
                   Hex  ASCII    Braille Dots     Braille Meaning -}
braileASCII : Dict Int (Char, List Int, String)
braileASCII = [   (0x20, (' ',  [],                 "(space)"))
                , (0x21, ('!',  [2, 3, 4, 6],       "the"))
                , (0x22, ('\"', [5],                "(contraction)"))
                , (0x23, ('#',  [3, 4, 5, 6],       "(number prefix)"))
                , (0x24, ('$',  [1, 2, 4, 6],       "ed"))
                , (0x25, ('%',  [1, 4, 6],          "sh"))
                , (0x26, ('&',  [1, 2, 3, 4, 6],    "and"))
                , (0x27, ('\'', [3],                "'"))
                , (0x28, ('(',  [1, 2, 3, 5, 6],    "of"))
                , (0x29, ( ')', [2, 3, 4, 5, 6],    "with"))
                , (0x2A, ('*',  [1, 6],             "ch"))
                , (0x2B, ('+',  [3, 4, 6],          "ing"))
                , (0x2C, (',',  [6],                "(uppercase prefix)"))
                , (0x2D, ('-',  [3, 6],             "-"))
                , (0x2E, ('.',  [4, 6],             "(italic prefix)"))
                , (0x2F, ('/',  [3, 4],             "st"))
                , (0x30, ('0',  [3, 5, 6],          "\""))
                , (0x31, ('1',  [2],                ","))
                , (0x32, ('2',  [2, 3],             ";"))
                , (0x33, ('3',  [2, 5],             ":"))
                , (0x34, ('4',  [2, 5, 6],          "."))
                , (0x35, ('5',  [2, 6],             "en"))
                , (0x36, ('6',  [2, 3, 5],          "!"))
                , (0x37, ('7',  [2, 3, 5, 6],       "( or )"))
                , (0x38, ('8',  [2, 3, 6],          "\" or ?"))
                , (0x39, ('9',  [3, 5],             "in"))
                , (0x3A, (':',  [1, 5, 6],          "wh"))
                , (0x3B, (';',  [5, 6],             "(letter prefix)"))
                , (0x3C, ('<',  [1, 2, 6],          "gh"))
                , (0x3D, ('=',  [1, 2, 3, 4, 5, 6], "for"))
                , (0x3E, ('>',  [3, 4, 5],          "ar"))
                , (0x3F, ('?',  [1, 4, 5, 6],       "th"))
                , (0x40, ('@',  [4],                "(accent prefix)"))
                , (0x41, ('A',  [1],                "a"))
                , (0x42, ('B',  [1, 2],             "b"))
                , (0x43, ('C',  [1, 4],             "c"))
                , (0x44, ('D',  [1, 4, 5],          "d"))
                , (0x45, ('E',  [1, 5],             "e"))
                , (0x46, ('F',  [1, 2, 4],          "f"))
                , (0x47, ('G',  [1, 2, 4, 5],       "g"))
                , (0x48, ('H',  [1, 2, 5],          "h"))
                , (0x49, ('I',  [2, 4],             "i"))
                , (0x4A, ('J',  [2, 4, 5],          "j"))
                , (0x4B, ('K',  [1, 3],             "k"))
                , (0x4C, ('L',  [1, 2, 3],          "l"))
                , (0x4D, ('M',  [1, 3, 4],          "m"))
                , (0x4E, ('N',  [1, 3, 4, 5],       "n"))
                , (0x4F, ('O',  [1, 3, 5],          "o"))
                , (0x50, ('P',  [1, 2, 3, 4],       "p"))
                , (0x51, ('Q',  [1, 2, 3, 4, 5],    "q"))
                , (0x52, ('R',  [1, 2, 3, 5],       "r"))
                , (0x53, ('S',  [2, 3, 4],          "s"))
                , (0x54, ('T',  [2, 3, 4, 5],       "t"))
                , (0x55, ('U',  [1, 3, 6],          "u"))
                , (0x56, ('V',  [1, 2, 3, 6],       "v"))
                , (0x57, ('W',  [2, 4, 5, 6],       "w"))
                , (0x58, ('X',  [1, 3, 4, 6],       "x"))
                , (0x59, ('Y',  [1, 3, 4, 5, 6],    "y"))
                , (0x5A, ('Z',  [1, 3, 5, 6],       "z"))
                , (0x5B, ('[', [2, 4, 6],           "ow"))
                , (0x5C, ('\\', [1, 2, 5, 6],       "ou"))
                , (0x5D, (']',  [1, 2, 4, 5, 6],    "er"))
                , (0x5E, ('^',  [4, 5],             "(currency prefix)"))
                , (0x5F, ('_',  [4, 5, 6],          "(contraction)"))
              ] |> Dict.fromList
