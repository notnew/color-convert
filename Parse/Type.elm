module Parse.Type (map, into, ap, pure, saveLeft, saveRight, alt, fail, bind
                  , fromMaybe)
    where

{- Type and typeclass-esque stuff -}

type Parser result = String -> Maybe (result, String)

-- Functor
map : (a -> b ) -> Parser a -> Parser b
map f p = rmap f . p


into : a -> Parser b -> Parser a
into = map . always

-- Applicative
ap : Parser (a -> b) -> Parser a -> Parser b
pf `ap` px = \s -> case pf s of
                     Just (f, s') -> rmap f <| px s'
                     Nothing      -> Nothing

pure : a -> Parser a
pure x = \s -> Just (x, s)

saveLeft : Parser a -> Parser b -> Parser a
saveLeft px py = map always px `ap` py

saveRight : Parser a -> Parser b -> Parser b
saveRight px py = map (flip always) px `ap` py

-- Monad
bind : Parser a -> (a -> Parser b) -> Parser b
px `bind` k = \s -> case px s of
                      Just (x, s') -> (k x) s'
                      Nothing      -> Nothing

-- Alternative
alt : Parser a -> Parser a -> Parser a
px `alt` py = \s -> case px s of
                      Nothing -> py s
                      r       -> r

-- Fail
fail : Parser a
fail = always Nothing

-- Other constructors, conversions
fromMaybe : Maybe a -> Parser a
fromMaybe = maybe fail pure


-- Helpers
{- result map -}
rmap : (a -> b ) -> Maybe (a, String) -> Maybe (b, String)
rmap f r = case r of
             Just (x, s) -> Just (f x, s)
             Nothing     -> Nothing

