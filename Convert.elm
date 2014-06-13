import Parse.Parse ( parse )
import Parse.Type ( Parser, pure )
import Parse.Combinators ( optional )
import Parse.Builtin as P
import Parse.Infix (..)

import Char
import Graphics.Input ( Input, Handle, input )
import Graphics.Input.Field as Field
import Graphics.Input.Field ( Content, noContent )
import Keyboard
import String
import Window

data Result = RGB Int Int Int
            | NoResult

makeColor r g b = RGB (clamp 0 255 r) (clamp 0 255 g) (clamp 0 255 b)

toColor : Result -> Color
toColor result =case result of
                  RGB r g b -> rgb r g b
                  NoResult  -> white

type Contents = { hex:Field.Content, rgb:Field.Content, hsl:Field.Content }

noContents : Contents
noContents = Contents noContent noContent noContent

-- view
main = display <~ Window.dimensions ~ contents ~ results

display : (Int,Int) -> Contents -> Result -> Element
display (w,h) contents result =
    let fieldWidth = toFloat w * 0.7
        frame height elem =
            elem |> size (truncate fieldWidth) (truncate <| fieldWidth/7)
                 |> container w h (middleAt (relative 0.5) (relative height))
        col = toColor result
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
                  let hsl = toHsl <| toColor result
                  in {noContent | string <-
                      "hsl(" ++ format (hsl.hue/turns 1) ++
                         "," ++ format hsl.saturation ++ "%" ++
                         "," ++ format hsl.lightness ++ "%)"}
              otherwise -> content
    in Field.field Field.defaultStyle hslContent.handle id "hsl" content'

--
results : Signal Result
results =
    let convert maybeRes = maybe NoResult id maybeRes
    in merges [
           (convert . parse rgbParse . .string) <~ rgbContent.signal
          ]


-- Parsers
rgbParse : Parser Result
rgbParse =
    let int = P.spaces *> optional (P.char ',') *> P.spaces *>
              P.integer  <* P.spaces
    in optional (P.string "rgb") *> P.spaces *> optional (P.char '(' )
             *> pure makeColor <*> int <*> int <*> int
             <* optional (P.char ')')

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

contents : Signal Contents
contents = merges  [ Contents <~ hexContent.signal ~ rgbContent.signal
                               ~ hslContent.signal
                   , sampleOn entered (constant noContents)
                   ]

entered = keepIf id True Keyboard.enter