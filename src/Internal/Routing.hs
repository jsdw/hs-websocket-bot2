{-# LANGUAGE 
    DataKinds,
    TypeFamilies,
    KindSignatures,
    TypeOperators,
    PolyKinds,
    UndecidableInstances,
    GADTs #-}

module Internal.Routing (

    -- create parsers with these:
    (<+>),
    var,
    static,

    -- create routes with these:
    addRoute,
    runRoutes,

    InputState(..),
    Routes

) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text            as T
import qualified Control.Monad.Writer as W

-- label the different kinds of parser
-- depending on which data constructor they are
-- built with:
data ParserTy a = PV a
                | PS a
                | PC (ParserTy a) (ParserTy a)

-- combine parsers using lifted array to store
-- list of parser output types (Var or Static):
data Parsers (ty :: ParserTy *) where
    PVar    :: P.Parser ty  -> Parsers (PV ty)
    PStatic :: P.Parser ty  -> Parsers (PS ty)
    PCons   :: Parsers tys1 -> Parsers tys2 -> Parsers (PC tys1 tys2)

-- convert parser var types to function signature:
type family ParserVarFn (a :: ParserTy *) out where
    ParserVarFn (PC as bs) out = ParserVarFn as (ParserVarFn bs out)
    ParserVarFn (PV a)     out = a -> out
    ParserVarFn (PS a)     out = out

-- run all of our parsers sequentially on some Text. pass the outputs to
-- all of these to some function and return the output of that or Nothing
-- if any parsers fail or text is remaining after parsers applied.
runParsers :: Parsers p -> T.Text -> ParserVarFn p out -> Maybe out
runParsers p text fn = case run p (Just (text,fn)) of
    Just (rem,out) -> if T.length rem > 0
                      then Nothing 
                      else Just out
    Nothing -> Nothing
  where
    run :: Parsers p -> Maybe (T.Text, ParserVarFn p out) -> Maybe (T.Text, out) 
    run _ Nothing = Nothing
    run (PCons as bs) mTxtAndFn = run bs (run as mTxtAndFn)
    run (PStatic a) (Just (txt,fn)) = case P.parse a txt of
        P.Done nextText res -> Just (nextText, fn)
        _ -> Nothing
    run (PVar a) (Just (txt,fn)) = case P.parse a txt of
        P.Done nextText res -> Just (nextText, fn res)
        _ -> Nothing

-- wrap converts a raw (P.Parser a) into a
-- wrapped (Parsers (PS a), but leaves an
-- already wrapped parser alone.
class WrapParser a where
    type ResParserTy a :: ParserTy *
    wrap :: a -> Parsers (ResParserTy a)

instance WrapParser (Parsers p) where
    type ResParserTy (Parsers p) = p
    wrap = id

instance WrapParser (P.Parser p) where
    type ResParserTy (P.Parser p) = PS p
    wrap p = PStatic p

instance WrapParser String where
    type ResParserTy String = PS T.Text
    wrap str = PStatic (P.string $ T.pack str)


-- create a nice syntax for defining Parsers,
-- basically a wrapper for PCons/PStatic/PVar
-- which autowraps into PStatic if not wrapped.
infixl 5 <+>
(<+>) :: (WrapParser p1, WrapParser p2) =>
         p1 -> p2 -> Parsers (PC (ResParserTy p1) (ResParserTy p2))
(<+>) p1 p2 = PCons (wrap p1) (wrap p2)

var = PVar
static = PStatic

-- this state is passed to each route/thing we add to the
-- list. Thus, we can extend it with more data and have
-- more fancy route handlers later on.
data InputState = InputState { inputMessage :: T.Text }

type Routes res = W.Writer [InputState -> Maybe res] ()

-- construct a route by providing parsers, and a function to operate
-- on the result of those parsers. parsers adhere to WrapParser
-- interface so we can dynamically convert things that arent parsers
-- into the right type first. Especially useful when only providng a
-- single parser for instance, to ensure correct types.
addRoute :: WrapParser ps => ps -> ParserVarFn (ResParserTy ps) out -> Routes out
addRoute parsers fn = W.writer ((), [\s -> runParsers (wrap parsers) (inputMessage s) fn])

-- take a list of routes and some input state
-- and return the first non-Nothing result we
-- get back.
runRoutes :: Routes r -> InputState -> Maybe r
runRoutes routes state = matchRoute (W.execWriter routes)
    where matchRoute [] = Nothing
          matchRoute (r:rs) = case r state of
              Nothing  -> matchRoute rs
              Just res -> Just res










