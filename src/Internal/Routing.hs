module Internal.Routing (

    addRoute,
    runRoutes,

    Routes,

    -- reexport parsing bits of interest
    -- as they are handy in creating routes:
    (<+>),
    var,
    static

) where

import Internal.Parsing
import qualified Data.Text            as T
import qualified Control.Monad.Writer as W

type Routes res = W.Writer [T.Text -> Maybe res] ()

-- construct a route by providing parsers, and a function to operate
-- on the result of those parsers. parsers adhere to WrapParser
-- interface so we can dynamically convert things that arent parsers
-- into the right type first. Especially useful when only providng a
-- single parser for instance, to ensure correct types.
addRoute :: WrapParser ps => ps -> ParserVarFn (ResParserTy ps) out -> Routes out
addRoute parsers fn = W.writer ((), [\text -> runParsers (wrap parsers) text fn])

-- take a list of routes and some input state
-- and return the first non-Nothing result we
-- get back.
runRoutes :: Routes r -> T.Text -> Maybe r
runRoutes routes text = matchRoute (W.execWriter routes)
    where matchRoute [] = Nothing
          matchRoute (r:rs) = case r text of
              Nothing  -> matchRoute rs
              Just res -> Just res