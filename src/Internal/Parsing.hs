{-# LANGUAGE
    DataKinds,
    TypeFamilies,
    KindSignatures,
    TypeOperators,
    PolyKinds,
    UndecidableInstances,
    MultiParamTypeClasses,
    ScopedTypeVariables,
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
    WrapParserTy

) where

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

 --Desired transformations:

 --var           PARSER     => PVar    (\_ -> PARSER)
 --var (\read -> PARSER)    => PVar    (\read -> PARSER)
 --static PARSER            => PStatic (\_ -> PARSER)
 --       PARSER            => PStatic (\_ -> PARSER)
 --static (\read -> PARSER) => PStatic (\read -> PARSER)
 --       (\read -> PARSER) => PStatic (\read -> PARSER)

-- ProvideState
-- PARSER            => (\_ -> PARSER)
-- (\read -> PARSER) => (\read -> PARSER)
--
-- More formally:
--
-- P.Parser a         => read -> P.Parser a
-- read -> P.Parser a => read -> P.Parser a

type family ProvideStateTy p where
    ProvideStateTy (P.Parser a) = a
    ProvideStateTy (read -> P.Parser a) = a

class ProvideState (read :: *) p where
    provideState :: p -> (read -> P.Parser (ProvideStateTy p))

instance ProvideState read (P.Parser a) where
    provideState parser = \_ -> parser

instance ProvideState read (read -> P.Parser a) where
    provideState = id

-- WrapParser
-- var (\read -> PARSER) => var    (\read -> PARSER)
--     (\read -> PARSER) => static (\read -> PARSER)
--
-- More formally:
--
-- Parsers read tys => Parsers read tys
-- P.Parser a       => Parsers read (PS a)

type family WrapParserTy p where
    WrapParserTy (Parsers read ty) = ty
    WrapParserTy p = PS (ProvideStateTy p)

class WrapParser (read :: *) parserLike where
    wrapParser :: parserLike -> Parsers read (WrapParserTy parserLike)

instance (PS (ProvideStateTy p) ~ WrapParserTy p, ProvideState read p) => WrapParser read p where
    wrapParser p = PStatic $ provideState p

instance {-# OVERLAPPING #-} WrapParser read (Parsers read ty) where
    wrapParser = id

-- create a nice syntax for defining Parsers,
-- basically a wrapper for PCons/PStatic/PVar
-- which autowraps into PStatic if not wrapped.
infixl 5 <+>
(<+>) :: (WrapParser read p1, WrapParser read p2)
      => p1 -> p2 -> Parsers read (PC (WrapParserTy p1) (WrapParserTy p2))
(<+>) p1 p2 = PCons (wrapParser p1) (wrapParser p2)

var p    = PVar (provideState p)
static p = PStatic (provideState p)


---- wrap converts a raw (P.Parser a) into a
---- wrapped (Parsers (PS a), but leaves an
---- already wrapped parser alone.
--class WrapParser read a where
--    type ResParserTy a :: ParserTy *
--    wrap :: a -> Parsers read (ResParserTy a)

--instance WrapParser read (Parsers read p) where
--    type ResParserTy (Parsers read p) = p
--    wrap = id

--instance WrapParser read (read -> P.Parser p) where
--    wrap pfn = PStatic pfn

--instance WrapParser read (P.Parser p) where
--    type ResParserTy (P.Parser p) = PS p
--    wrap p = wrap (\_ -> p)

--instance WrapParser read String where
--    type ResParserTy String = PS T.Text
--    wrap str = PStatic (\_ -> (P.string $ T.pack str))

--instance WrapParser read Char where
--    type ResParserTy Char = PS Char
--    wrap c = PStatic (\_ -> (P.char c))

-- create a nice syntax for defining Parsers,
-- basically a wrapper for PCons/PStatic/PVar
-- which autowraps into PStatic if not wrapped.
--infixl 5 <+>
--(<+>) :: (WrapParser p1, WrapParser p2) =>
--         p1 -> p2 -> Parsers (PC (ResParserTy p1) (ResParserTy p2))
--(<+>) p1 p2 = PCons (wrap p1) (wrap p2)

--var = PVar
--static = PStatic










