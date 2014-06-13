module Parse.Parse where

import Parse.Type(Parser)

-- Run
parse : Parser a -> String -> Maybe a
parse p s = case p s of
              Just (result, _) -> Just result
              _                -> Nothing

