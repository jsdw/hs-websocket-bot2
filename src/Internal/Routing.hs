module Internal.Routing (

    addRoute,
    addMaybeRoute,
    runRoutes,

    Routes,
    mkRoutesInput,

    -- reexport parsing bits of interest
    -- as they are handy in creating routes:
    (<+>),
    var,
    static

) where

import Internal.Parsing
import qualified Data.Text            as T
import qualified Control.Monad.Writer as W
import qualified Data.Time            as Time
import           System.Random        (newStdGen, randomRs)
import           Control.Monad.Trans  (liftIO, MonadIO)


type Routes res = W.Writer [RoutesInput -> Maybe res] ()

-- data provided to routes, not exposed.
-- use a constructor to create:
data RoutesInput = RoutesInput
    { routesMessage :: T.Text
    , routesRandom  :: [Double]
    }

mkRoutesInput :: MonadIO m => T.Text -> m RoutesInput
mkRoutesInput msg = do
    randomGen <- liftIO $ newStdGen
    return RoutesInput
        { routesMessage = msg
        , routesRandom  = randomRs (0,1) randomGen
        }

-- construct a route by providing parsers, and a function to operate
-- on the result of those parsers. parsers adhere to WrapParser
-- interface so we can dynamically convert things that arent parsers
-- into the right type first. Especially useful when only providng a
-- single parser for instance, to ensure correct types.
addRoute :: WrapParser RoutesInput ps
         => ps -> ParserVarFn (WrapParserTy ps) out -> Routes out
addRoute parsers fn = addMaybeRoute 1 parsers fn

addMaybeRoute :: WrapParser RoutesInput ps
              => Double -> ps -> ParserVarFn (WrapParserTy ps) out -> Routes out
addMaybeRoute n parsers fn = W.writer ((), [input])
  where
    input = \r@RoutesInput{..} -> if head routesRandom < n
                                then runParsers (wrapParser parsers) r routesMessage fn
                                else Nothing

-- take a list of routes and some input state
-- and return the first non-Nothing result we
-- get back. Each route consumes one random value.
runRoutes :: Routes r -> RoutesInput -> Maybe r
runRoutes routes routesInput = matchRoute (W.execWriter routes) routesInput
    where matchRoute []     ri = Nothing
          matchRoute (r:rs) ri = case r ri of
              Nothing  -> matchRoute rs ri { routesRandom = tail (routesRandom ri) }
              Just res -> Just res















