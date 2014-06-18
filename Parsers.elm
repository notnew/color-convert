module Parsers where

import Result
import Result (Result)

import Parse.Parse
import Parse.Type ( Parser, pure )
import Parse.Combinators ( optional )
import Parse.Builtin as P
import Parse.Infix (..)

import Char

parse = Parse.Parse.parse

-- Parsers
hexParse : Parser Result
hexParse =
    let hexDigit = (pure digitToInt <*> P.digit)
              <|>  (pure hexToInt <*> P.hexDigit)
        digitToInt d = Char.toCode d - Char.toCode '0'
        hexToInt d = Char.toCode d - Char.toCode 'a' + 10
        sixHexes =
            let byte = pure (\a b -> a*16 + b) <*>
                         (P.spaces *> hexDigit) <*> hexDigit
            in pure Result.make <*> byte <*> byte <*> byte
    in optional (P.char '#') *> sixHexes

rgbParse : Parser Result
rgbParse =
    let int = P.spaces *> optional (P.char ',') *> P.spaces *>
              P.integer  <* P.spaces
    in optional (P.string "rgb") *> P.spaces *> optional (P.char '(' )
             *> pure Result.make <*> int <*> int <*> int
                <* optional (P.char ')')

hslParse : Parser Result
hslParse =
    let float = P.spaces *> optional (P.char ',') *> P.spaces *>
                P.float  <* P.spaces
        make h s l = let {red,green,blue} = toRgb <| hsl h s l
                     in Result.make red green blue
    in optional (P.string "hsl") *> optional (P.char '(' )
             *> pure make <*> float <*> float <*> float
                <* optional (P.char ')')

