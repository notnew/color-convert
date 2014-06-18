
import Result
import Result ( Result, RGB, NoResult )
import Parsers ( parse, hexParse, rgbParse, hslParse  )

import Graphics.Input ( Input, Handle, input )
import Graphics.Input.Field as Field
import Graphics.Input.Field ( Content, noContent, defaultStyle, uniformly )
import Keyboard
import Window

{- Record of settings and data needed to make an input field -}
type FieldInfo =
    { label:String, printer:Result -> String, handle:Handle Content
    , content:Content, result:Result, textHeight:Float }

hexInfo : FieldInfo
hexInfo = FieldInfo "hex" Result.toHex hexContent.handle noContent NoResult 40
rgbInfo = FieldInfo "rgb" Result.toRGB rgbContent.handle noContent NoResult 40
hslInfo = FieldInfo "hsl" Result.toHSL hslContent.handle noContent NoResult 40

-- Inputs
hexContent : Input Content
hexContent = input noContent
rgbContent = input noContent
hslContent = input noContent

entered = keepIf id True Keyboard.enter

-- Signals
type Contents = { hex:Field.Content, rgb:Field.Content, hsl:Field.Content }

noContents : Contents
noContents = Contents noContent noContent noContent

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
    let fieldWidth = truncate <| toFloat w * 0.85
        fieldHeight = 80
        frame height elem =
            elem |> size fieldWidth fieldHeight
                 |> container w h (middleAt (relative 0.5)
                                            (absolute height))
        fld fieldInfo = field {fieldInfo | result <- result
                              , textHeight <- toFloat fieldHeight/2}
        col = Result.toColor result
    in color col <| layers [
          frame (240) <| fld { hexInfo | content <- contents.hex }
        , frame (360) <| fld { rgbInfo | content <- contents.rgb }
        , frame (480) <| fld { hslInfo | content <- contents.hsl }
           ]

{- make an input field using data stored in a FieldInfo -}
field : FieldInfo -> Element
field {textHeight, label, printer, handle, content, result} =
    let content' = if | String.isEmpty content.string ->
                         { noContent | string <- printer result }
                      | otherwise -> content
    in Field.field (fieldStyle textHeight) handle id label content'

textStyle = Text.defaultStyle
fieldStyle h = { defaultStyle |
     style <- {textStyle | height <- Just h, bold <- True} }
