
import Result
import Result ( Result, RGB, NoResult )
import Parsers ( parse, hexParse, rgbParse, hslParse  )

import Graphics.Input ( Input, Handle, input )
import Graphics.Input.Field as Field
import Graphics.Input.Field ( Content, noContent, defaultStyle, uniformly )
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
contents =
    let updates = merges [
                   (\hex _ -> {noContents | hex <- hex}) <~ hexContent.signal
                 , (\rgb _ -> {noContents | rgb <- rgb}) <~ rgbContent.signal
                 , (\hsl _ -> {noContents | hsl <- hsl}) <~ hslContent.signal
                 , (\_ _ -> noContents) <~ entered
                            ]
    in foldp (<|) noContents updates

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
        fld = field <| fieldWidth/15
        col = Result.toColor result
    in color col <| layers [
          frame (1/6) <| fld "hex" Result.toHex hexContent.handle
                               contents.hex result
        , frame (2/6) <| fld "rgb" Result.toRGB rgbContent.handle
                               contents.rgb result
        , frame (3/6) <| fld "hsl" Result.toHSL hslContent.handle
                               contents.hsl result
           ]

field : Float -> String -> (Result -> String) -> Handle Content -> Content
              -> Result -> Element
field textHeight label printer handle content result =
    let content' = if | String.isEmpty content.string ->
                         { noContent | string <- printer result }
                      | otherwise -> content
    in Field.field (fieldStyle textHeight) handle id label content'

textStyle = Text.defaultStyle
fieldStyle h = { defaultStyle |
     style <- {textStyle | height <- Just h } }
