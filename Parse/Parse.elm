module Parse.Parse where

import Parse.Type(Parser)

-- Run
parse : String -> Parser a -> Maybe a
parse s p = case p s of
              Just (result, _) -> Just result
              _                -> Nothing

