import Parse.Parse ( parse )
import Parse.Type ( Parser, pure )
import Parse.Combinators ( optional )
import Parse.Builtin as P
import Parse.Infix (..)

import Graphics.Input ( Input, Handle, input )
import Graphics.Input.Field as Field
import Graphics.Input.Field ( Content, noContent )
import Window

data Result = RGB Float Float Float
            | NoResult

-- View
main = display <~ Window.dimensions ~ contents ~ results

display : (Int,Int) -> Contents -> Result -> Element
display (w,h) contents result =
    let frame height elem =
            container w h (middleAt (relative 0.5) (relative height)) elem
    in color lightGrey <| layers [
          frame (1/6) <| hexField contents.hex result
        , frame (2/6) <| rgbField contents.rgb result
        , frame (3/6) <| hslField contents.hsl result
           ]

hexField : Content -> Result -> Element
hexField content result  =
    Field.field Field.defaultStyle hexContent.handle id "hex" content

rgbField : Content -> Result -> Element
rgbField content result =
    Field.field Field.defaultStyle rgbContent.handle id "rgb" content

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
             *> pure NoResult

-- Inputs
hexContent : Input Content
hexContent = input noContent

rgbContent = input noContent

hslContent = input noContent

type Contents = { hex:Field.Content, rgb:Field.Content, hsl:Field.Content }

contents : Signal Contents
contents = Contents <~ hexContent.signal ~ rgbContent.signal
                     ~ hslContent.signal
