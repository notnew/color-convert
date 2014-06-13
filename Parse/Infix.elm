module Parse.Infix where

import Parse.Type as P

(<$) = P.into
(<$>) = P.map
(<*>) = P.ap
(<*) = P.saveLeft
(*>) = P.saveRight
(<|>) = P.alt
