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
          frame (1/6) <| field contents.hex
        , frame (2/6) <| field contents.rgb
        , frame (3/6) <| field contents.hsl
           ]

field : Content -> Element
field content = asText "hi"

-- Inputs
hexContent : Input Content
hexContent = input noContent

rgbContent = input noContent

hslContent = input noContent

type Contents = { hex:Field.Content, rgb:Field.Content, hsl:Field.Content }

contents : Signal Contents
contents = Contents <~ hexContent.signal ~ rgbContent.signal
                     ~ hslContent.signal