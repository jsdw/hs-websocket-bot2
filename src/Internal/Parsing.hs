{-# LANGUAGE
    DataKinds,
    TypeFamilies,
    KindSignatures,
    TypeOperators,
    PolyKinds,
    UndecidableInstances,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    ConstraintKinds,
    GADTs #-}

module Internal.Parsing (

    -- create parsers with these:
    (<+>),
    var,
    static,
    runParsers,

    -- useful types:
    ParserVarFn,
    WrapParser(..),
    ParserReadTy

) where

import           GHC.Exts             (Constraint)

import qualified Data.Attoparsec.Text as P
import qualified Data.Text            as T

-- label the different kinds of parser
-- depending on which data constructor they are
-- built with:
data ParserTy a = PV a
                | PS a
                | PC (ParserTy a) (ParserTy a)

-- combine parsers using lifted array to store
-- list of parser output types (Var or Static):
data Parsers read (tys :: ParserTy *) where
    PVar    :: (read -> P.Parser ptype)     -> Parsers read (PV ptype)
    PStatic :: (read -> P.Parser ptype)     -> Parsers read (PS ptype)
    PCons   :: Parsers read tys1 -> Parsers read tys2 -> Parsers read (PC tys1 tys2)

-- convert parser var types to function signature:
type family ParserVarFn (a :: ParserTy *) out where
    ParserVarFn (PC as bs) out = ParserVarFn as (ParserVarFn bs out)
    ParserVarFn (PV a)     out = a -> out
    ParserVarFn (PS a)     out = out

-- run all of our parsers sequentially on some Text. pass the outputs to
-- all of these to some function and return the output of that or Nothing
-- if any parsers fail or text is remaining after parsers applied.
runParsers :: forall r p out. Parsers r p -> r -> T.Text -> ParserVarFn p out -> Maybe out
runParsers p readable text fn = case run p (Just (text,fn)) of
    Just (rem,out) -> if T.length rem > 0
                      then Nothing
                      else Just out
    Nothing -> Nothing
  where
    run :: Parsers r p2 -> Maybe (T.Text, ParserVarFn p2 out2) -> Maybe (T.Text, out2)
    run _ Nothing = Nothing
    run (PCons as bs) mTxtAndFn = run bs (run as mTxtAndFn)
    run (PStatic a) (Just (txt,fn)) = noBind fn $ P.parse (a readable) txt
    run (PVar a) (Just (txt,fn)) = bind fn $ P.parse (a readable) txt

    noBind fn r = case r of
        P.Done nextText res -> Just (nextText, fn)
        P.Partial res -> noBind fn (res "")
        _ -> Nothing
    bind fn r = case r of
        P.Done nextText res -> Just (nextText, fn res)
        P.Partial res -> bind fn (res "")
        _ -> Nothing

--
-- A constraint that everything is inhabitant of:
--
class Anything a where
instance Anything a where


-- Given a parser-like thing, This constraint
-- locks down the type of thing that can
-- optionally be fed to it:
type family ParserReadTy p r :: Constraint where
    ParserReadTy (P.Parser a)         r = Anything r
    ParserReadTy (read -> P.Parser a) r = read ~ r
    ParserReadTy (Parsers read ty)    r = read ~ r


-- In order to make variable/static parsers on demand,
-- we need to partially wrap parsers with state but then
-- stop there.
--
-- Desired transformations:
--
-- P.Parser a         => read -> P.Parser a
-- read -> P.Parser a => read -> P.Parser a
--
class ProvideState p where
    type ParserOutTy p
    provideState :: p -> (ParserReadTy p b => b -> P.Parser (ParserOutTy p))

instance ProvideState (P.Parser a) where
    type ParserOutTy (P.Parser a) = a
    provideState parser = \_ -> parser

instance ProvideState (read -> P.Parser a) where
    type ParserOutTy (read -> P.Parser a) = a
    provideState = id

var p = PVar $ provideState p
static p = PStatic $ provideState p


-- Wraps a parser into a standard form. This could use the
-- above to remove an instance but simpler to duplicate.
--
-- Desired transformations:
--
-- P.Parser a         => read -> P.Parser a
-- read -> P.Parser a => Parsers read (PS a)
-- Parsers read ty    => Parsers read ty
--
class WrapParser p where
    type WrapParserTy p :: ParserTy *
    wrapParser :: ParserReadTy p r => p -> Parsers r (WrapParserTy p)

instance WrapParser (P.Parser a) where
    type WrapParserTy (P.Parser a) = PS a
    wrapParser p = PStatic (\_ -> p)

instance WrapParser (read -> P.Parser a) where
    type WrapParserTy (read -> P.Parser a) = PS a
    wrapParser p = PStatic p

instance WrapParser (Parsers read ty) where
    type WrapParserTy (Parsers read ty) = ty
    wrapParser p = p


-- create a nice syntax for defining Parsers,
-- basically a wrapper for PCons/PStatic/PVar
-- which autowraps into PStatic if not wrapped.
infixl 5 <+>
(<+>) :: (WrapParser p1, WrapParser p2, ParserReadTy p1 read, ParserReadTy p2 read)
      => p1 -> p2 -> Parsers read (PC (WrapParserTy p1) (WrapParserTy p2))
(<+>) p1 p2 = PCons (wrapParser p1) (wrapParser p2)













