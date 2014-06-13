import Parse.Parse ( parse )
import Parse.Type ( Parser, pure )
import Parse.Combinators ( optional )
import Parse.Builtin as P
import Parse.Infix (..)

import Char
import Graphics.Input ( Input, Handle, input )
import Graphics.Input.Field as Field
import Graphics.Input.Field ( Content, noContent )
import String
import Window

data Result = RGB Int Int Int
            | NoResult

-- View
main = display <~ Window.dimensions ~ contents ~ results

display : (Int,Int) -> Contents -> Result -> Element
display (w,h) contents result =
    let fieldWidth = toFloat w * 0.7
        frame height elem =
            elem |> size (truncate fieldWidth) (truncate <| fieldWidth/7)
                 |> container w h (middleAt (relative 0.5) (relative height))
        col = case result of
                RGB r g b -> rgb r g b
                NoResult  -> white
    in color col <| layers [
          frame (1/6) <| size (w `div` 2) 200 <| hexField contents.hex result
        , frame (2/6) <| rgbField contents.rgb result
        , frame (3/6) <| hslField contents.hsl result
           ]

hexField : Content -> Result -> Element
hexField content result  =
    let toHexDigit n = if | n < 0  -> ""
                          | n < 10 -> show n
                          | n < 16 ->
                              let charCode = n + Char.toCode 'a' - 10
                              in String.cons (Char.fromCode charCode) ""
                          | otherwise -> ""
        toHex n = if | n < 16 -> "0" ++ toHexDigit n
                     | otherwise ->
                         toHexDigit (n `div` 16) ++ toHexDigit (n `mod` 16)
        content' = case (result, content) of
                     (RGB r g b, noContent) ->
                         { noContent | string <- "#" ++ toHex r ++ toHex g
                                                     ++ toHex b }
                     otherwise -> content
    in Field.field Field.defaultStyle hexContent.handle id "hex" content'

rgbField : Content -> Result -> Element
rgbField content result =
    let content' = case (result, content) of
                     (RGB r g b, noContent) ->
                         {noContent | string <- "rbg(" ++ show r ++
                                                   "," ++ show g ++
                                                   "," ++ show b ++ ")"}
                     otherwise -> content
    in Field.field Field.defaultStyle rgbContent.handle id "rgb" content

hslField : Content -> Result -> Element
hslField content result =
    Field.field Field.defaultStyle hslContent.handle id "hsl" content

--
results : Signal Result
results =
    let convert maybeRes = maybe NoResult id maybeRes
    in merges [
           (convert . parse rgbParse . .string) <~ rgbContent.signal
          ]


-- Parsers
rgbParse : Parser Result
rgbParse = optional (P.string "rgb") *> optional (P.char '(' )
             *> pure RGB <*> P.integer
                         <*> (P.char ',' *> P.integer)
                         <*> (P.char ',' *> P.integer) <* optional (P.char ')')

hslParse : Parser Result
hslParse = optional (P.string "rgb") *> optional (P.char '(' )
             *> pure RGB <*> P.integer
                         <*> (P.char ',' *> P.integer)
                         <*> (P.char ',' *> P.integer) <* optional (P.char ')')

-- Inputs
hexContent : Input Content
hexContent = input noContent

rgbContent = input noContent

hslContent = input noContent

type Contents = { hex:Field.Content, rgb:Field.Content, hsl:Field.Content }

contents : Signal Contents
contents = Contents <~ hexContent.signal ~ rgbContent.signal
                     ~ hslContent.signal
