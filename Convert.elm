
import Result
import Result ( Result, RGB, NoResult )
import Parsers ( parse, hexParse, rgbParse, hslParse  )

import Char
import Graphics.Input ( Input, Handle, input )
import Graphics.Input.Field as Field
import Graphics.Input.Field ( Content, noContent )
import Keyboard
import Window

-- Inputs
hexContent : Input Content
hexContent = input noContent
rgbContent = input noContent
hslContent = input noContent

entered = keepIf id True Keyboard.enter

type Contents = { hex:Field.Content, rgb:Field.Content, hsl:Field.Content }

noContents : Contents
noContents = Contents noContent noContent noContent

-- Signals
contents : Signal Contents
contents = merges  [ Contents <~ hexContent.signal ~ rgbContent.signal
                               ~ hslContent.signal
                   , sampleOn entered (constant noContents)
                   ]

results : Signal Result
results =
    let convert maybeRes = maybe NoResult id maybeRes
    in merges [
            (convert . parse hexParse . .string) <~ hexContent.signal
          , (convert . parse rgbParse . .string) <~ rgbContent.signal
          , (convert . parse hslParse . .string) <~ hslContent.signal
          ]

-- view
main = display <~ Window.dimensions ~ contents ~ results

display : (Int,Int) -> Contents -> Result -> Element
display (w,h) contents result =
    let fieldWidth = toFloat w * 0.7
        frame height elem =
            elem |> size (truncate fieldWidth) (truncate <| fieldWidth/7)
                 |> container w h (middleAt (relative 0.5) (relative height))
        col = Result.toColor result
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
        content' = case (result, content.string) of
                     (RGB r g b, "") ->
                         { noContent | string <- "#" ++ toHex r ++ toHex g
                                                     ++ toHex b }
                     otherwise -> content
    in Field.field Field.defaultStyle hexContent.handle id "hex" content'

rgbField : Content -> Result -> Element
rgbField content result =
    let content' = case (result, content.string) of
                     (RGB r g b, "") ->
                         {noContent | string <- "rgb(" ++ show r ++
                                                   "," ++ show g ++
                                                   "," ++ show b ++ ")"}
                     otherwise -> content
    in Field.field Field.defaultStyle rgbContent.handle id "rgb" content'

hslField : Content -> Result -> Element
hslField content result =
    let format f = show <| round <| f * 100
        content' =
            case (result, content.string) of
              (RGB r g b, "") ->
                  let hsl = toHsl <| Result.toColor result
                  in {noContent | string <-
                      "hsl(" ++ format (hsl.hue/turns 1) ++
                         "," ++ format hsl.saturation ++ "%" ++
                         "," ++ format hsl.lightness ++ "%)"}
              otherwise -> content
    in Field.field Field.defaultStyle hslContent.handle id "hsl" content'


