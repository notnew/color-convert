module Result where

data Result = RGB Int Int Int
            | NoResult

make r g b = RGB (clamp 0 255 r) (clamp 0 255 g) (clamp 0 255 b)

toColor : Result -> Color
toColor result =case result of
                  RGB r g b -> rgb r g b
                  NoResult  -> white

