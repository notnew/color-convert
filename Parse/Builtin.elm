module Parse.Builtin where

import Parse.Type (Parser, pure, bind, fromMaybe, fail)
import Parse.Infix (..)
import Parse.Combinators ( guard, tryModify, lookAhead, many, many1)

import Char ( isDigit )
import String as S

eof : Parser ()
eof s = if String.isEmpty s then Just ((), "") else Nothing

anyChar : Parser Char
anyChar = S.uncons

char : Char -> Parser Char
char c = guard ((==) c) anyChar

string : String -> Parser String
string s = \s' -> let len = S.length s in
         if | S.left len s' == s -> Just (s, S.dropLeft len s')
            | otherwise          -> Nothing

oneOf : String -> Parser Char
oneOf s = guard (\c -> String.any ((==) c) s) anyChar

noneOf : String -> Parser Char
noneOf s = guard (\c -> not <| String.any ((==) c) s) anyChar

space : Parser Char
space = oneOf " \n\t\r"

spaces : Parser ()
spaces = () <$ many space

digit : Parser Char
digit = guard isDigit anyChar

integer : Parser Int
integer = let num = many1 digit `bind` (fromMaybe . S.toInt . S.fromList)
              sign = (-1 <$ char '-') <|> pure 1
          in (*) <$> sign <*> num

float : Parser Float
float = let num = str `bind` (fromMaybe . S.toFloat)
            str =  S.fromList <$> (whole <|> decimal)
            whole = (++) <$> (many1 digit) <*> maybeDecimal
            maybeDecimal = decimal <|> ([] <$ char '.') <|> pure []
            decimal = (::) <$> (char '.') <*> many1 digit
            sign = (-1 <$ char '-') <|> pure 1
        in (*) <$> sign <*> num
