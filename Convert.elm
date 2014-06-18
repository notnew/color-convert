
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
    , content:Content, result:Result, width:Int, height:Int, hint:String }

hexInfo : FieldInfo
hexInfo = FieldInfo "hex" Result.toHex hexContent.handle noContent NoResult
                    100 40 "Enter six hexadecimal digits"
rgbInfo = FieldInfo "rgb" Result.toRGB rgbContent.handle noContent NoResult
                    100 40
                    "Enter decimal components, separated by a comma or space"
hslInfo =
    let hint =
        "Separate components by a comma or space\n" ++
        "hue is in radians between 0 and 6.283185\n" ++
        "lightness and saturation are fractions between 0 and 1"
    in FieldInfo "hsl" Result.toHSL hslContent.handle noContent NoResult
                 100 40 hint

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
    let frame height elem =
            elem |> container w h (midTopAt (relative 0.5)
                                            (absolute height))
        fld fieldInfo = field {fieldInfo | result <- result
                              , height <- 80
                              , width <- truncate <| toFloat w * 0.85 }
        col = Result.toColor result
    in color col <| layers [
          frame (200) <| fld { hexInfo | content <- contents.hex }
        , frame (320) <| fld { rgbInfo | content <- contents.rgb }
        , frame (440) <| fld { hslInfo | content <- contents.hsl }
           ]

{- make an input field using data stored in a FieldInfo -}
field : FieldInfo -> Element
field {width, height, label, printer, handle, content, result, hint} =
    let content' = if | String.isEmpty content.string ->
                         { noContent | string <- printer result }
                      | otherwise -> content
        style = fieldStyle <| toFloat height/2
        field = size  width height
                  <| Field.field style handle id label content'
        hintElem = if result == NoResult && not (String.isEmpty content.string)
                   then leftAligned <| toText hint
                   else empty
    in above field hintElem

textStyle = Text.defaultStyle
fieldStyle h = { defaultStyle |
     style <- {textStyle | height <- Just h, bold <- True}
   , outline <- {radius=6, color=grey, width=Field.uniformly 4}
   , padding <- Field.uniformly <| round (h/2) }
