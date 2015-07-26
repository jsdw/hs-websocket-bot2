{-# LANGUAGE ViewPatterns #-}

module Parsers (
    --export everything visible in this module as-is:
    module Parsers
) where

import           Prelude              hiding (take)
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import           Control.Applicative  ((<|>))
import           Data.Monoid          ((<>))
import           Control.Monad        (void)
import           Data.Char            (toLower,toUpper)
--
-- This module contains all of the parsers we'll use in routes
--

-- parse botname, optional @ followe by case insensitive name
pBotName :: Parser T.Text
pBotName = do
    let pN = pIS "JamesBot"
    (char '@' >> pN) <|> pN

-- consume text until the parser provided matches, and return
-- a tuple of text consumed and matched result
pUntil :: Parser a -> Parser (T.Text,a)
pUntil parser = loop parser ""
  where
    loop parser out = do
        (parser >>= \r -> return (out,r)) <|> do
            c <- take 1
            loop parser (out<>c)

-- consume the rest of the input
pRest :: Parser T.Text
pRest = fmap T.pack (many' anyChar)

-- parse a relative time into miliseconds
pFromNow :: Parser Int
pFromNow = do
    num <- double
    skipMany space
    word <- fmap T.pack (many' letter)

    let miliseconds = 1
        seconds     = 1000
        minutes     = 60 * seconds
        hours       = 60 * minutes
        days        = 24 * hours
        weeks       = 7  * days
        months      = 4.33 * weeks
        years       = 365 * days
        decades     = 10 * years
        eons        = 1000 * years

    let mult = case T.toLower word of
          (i ["ms", "milisecond", "miliseconds"] -> True) -> miliseconds
          (i ["s", "second", "seconds"]          -> True) -> seconds
          (i ["m", "minute", "minutes"]          -> True) -> minutes
          (i ["h", "hour", "hours"]              -> True) -> hours
          (i ["d", "day", "days"]                -> True) -> days
          (i ["w", "week", "weeks"]              -> True) -> weeks
          (i ["month", "months"]                 -> True) -> months
          (i ["y", "year", "years"]              -> True) -> years
          (i ["decade", "decades"]               -> True) -> decades
          (i ["eon", "eons"]                     -> True) -> eons
          _                                               -> minutes
          where i (a:as) w = if a == word then True else i as w
                i []     w = False

    return $ round (num * mult)

-- parse string, case insensitive
pIS :: T.Text -> Parser T.Text
pIS t = loop t "" 
  where
    loop ""  out = return out
    loop cur out = do
        let ll = toLower $ T.head cur
            ul = toUpper ll
        o <- char ll <|> char ul
        loop (T.tail cur) (out<> T.pack [o])

-- parse string
pS :: T.Text -> Parser T.Text
pS = string