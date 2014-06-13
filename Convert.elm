import Parse.Parse
import Parse.Builtin
import Parse.Infix (..)

import Graphics.Input ( Input, Handle, input )
import Graphics.Input.Field as Field
import Graphics.Input.Field ( Content, noContent )
import Window

data Result = RGB Float Float Float
            | NoResult

-- View
main = display <~ Window.dimensions ~ contents

display : (Int,Int) -> Contents -> Element
display (w,h) contents =
    let frame height elem =
            container w h (middleAt (relative 0.5) (relative height)) elem
    in color lightGrey <| layers [
          frame (1/6) <| hexField contents.hex
        , frame (2/6) <| rgbField contents.rgb
        , frame (3/6) <| hslField contents.hsl
           ]

hexField : Content -> Element
hexField content =
    Field.field Field.defaultStyle hexContent.handle id "hex" content

rgbField : Content -> Element
rgbField content =
    Field.field Field.defaultStyle rgbContent.handle id "rgb" content

hslField : Content -> Element
hslField content =
    Field.field Field.defaultStyle hslContent.handle id "hsl" content

-- Inputs
hexContent : Input Content
hexContent = input noContent

rgbContent = input noContent

hslContent = input noContent

type Contents = { hex:Field.Content, rgb:Field.Content, hsl:Field.Content }

contents : Signal Contents
contents = Contents <~ hexContent.signal ~ rgbContent.signal
                     ~ hslContent.signal
