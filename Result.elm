module Result where

import Char

data Result = RGB Int Int Int
            | NoResult

make r g b = RGB (clamp 0 255 r) (clamp 0 255 g) (clamp 0 255 b)

toColor : Result -> Color
toColor result =case result of
                  RGB r g b -> rgb r g b
                  NoResult  -> white

-- Display
toHex : Result -> String
toHex result =
    let toHexDigit n = if | n < 0  -> ""
                          | n < 10 -> show n
                          | n < 16 ->
                              let charCode = n + Char.toCode 'a' - 10
                              in String.cons (Char.fromCode charCode) ""
                          | otherwise -> ""
        toHex n = if | n < 16 -> "0" ++ toHexDigit n
                     | otherwise ->
                         toHexDigit (n `div` 16) ++ toHexDigit (n `mod` 16)
    in case result of
         RGB r g b -> "#" ++ toHex r ++ toHex g ++ toHex b
         NoResult  -> ""

toRGB : Result -> String
toRGB result =
    case result of
      RGB r g b -> join " " ["rgb", show r, show g, show b]
      NoResult  -> ""

toHSL : Result -> String
toHSL result =
    let format = String.left 4 . show
        {hue, saturation, lightness} = toHsl <| toColor result
    in case result of
         RGB r g b ->
             join " " ["hsl", format hue, format saturation, format lightness]
         NoResult  -> ""
