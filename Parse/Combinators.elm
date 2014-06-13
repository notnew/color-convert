module Parse.Combinators ( guard, lookAhead, many, many1
                         , optional, manyTill, count, sepBy, sepBy1 )
    where

import Parse.Type ( Parser, map, into, ap, pure, saveLeft, saveRight, bind
                  , alt, fail)
import Parse.Infix (..)

guard : (a -> Bool) -> Parser a -> Parser a
guard pred parser =
    let guard' x = if pred x then pure x else fail
    in parser `bind` guard'

lookAhead : Parser a -> Parser a
lookAhead p = \s -> case p s of
                      Just (r, _) -> Just (r, s)
                      Nothing     -> Nothing

many : Parser a -> Parser [a]
many p = (::) <$> p <*> (\s -> many p s)
           <|> pure []

many1 : Parser a -> Parser [a]
many1 p = (::) <$> p <*> many p

optional : Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

manyTill : Parser a -> Parser end -> Parser [a]
manyTill x end = ([] <$ lookAhead end) <|>
                 ((::) <$> x <*> \s -> manyTill x end s)

count : Int -> Parser a -> Parser [a]
count n x = if | n == 0    -> pure []
               | otherwise -> (::) <$> x <*> (\s -> count (n-1) x s)

sepBy : Parser a -> Parser sep -> Parser [a]
sepBy x sep = sepBy1 x sep <|> pure []

sepBy1 : Parser a -> Parser sep -> Parser [a]
sepBy1 x sep = let join x _ xs = x::xs
                   makeList x = [x]
               in (join <$> x <*> sep <*> (\s -> sepBy1 x sep s))
                  <|> (makeList <$> x)

