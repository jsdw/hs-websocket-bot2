{-# LANGUAGE ViewPatterns #-}

module Parsers (
    --export everything visible in this module as-is:
    module Parsers
) where

import Internal.Routing

import           Prelude              hiding (take)
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import qualified Data.List            as L
import           Data.Time            as Time
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

-- try provided parser, return Just type if it succeeds else nothing
pMaybe :: Parser a -> Parser (Maybe a)
pMaybe parser = (parser >>= return . Just) <|> return Nothing

--if provided parser succeeds return True else False
pBool :: Parser a -> Parser Bool
pBool parser = (parser >> return True) <|> return False

-- capture one word (letters)
pWord :: Parser T.Text
pWord = many1 letter >>= return . T.pack

-- consume the rest of the input
pRest :: Parser T.Text
pRest = takeText

pDecimal :: Integral a => Parser a
pDecimal = decimal

-- parse a date, supported formats:
-- DD/MM/YYYY
-- YYYY/MM/DD
-- Dec[ember] DD[th/rd/nd] YYYY
-- DD[th/rd/nd] Dec[ember] YYYY
pDate :: Parser Time.Day
pDate = do
    (y,m,d) <- ymd <|> ddmmmmyyyy <|> mmmmddyyyy
    return $ Time.fromGregorian y m d
  where
    ymd :: Parser (Integer,Int,Int)
    ymd = do
        first <- decimal
        dateSep
        second <- decimal
        dateSep
        third <- decimal

        if second > 12 then fail "pDate: month greater than 12" else return ()

        return $ if first > 31
            then (first,fromIntegral second,fromIntegral third)
            else (third,fromIntegral second,fromIntegral first)

    mmmmddyyyy :: Parser (Integer,Int,Int)
    mmmmddyyyy = do
        month <- fromMonthString
        spaceLike
        day <- dayOrMonth
        skipWhile $ inClass "a-zA-Z"
        spaceLike
        year <- decimal
        return (year,month,day)

    ddmmmmyyyy :: Parser (Integer,Int,Int)
    ddmmmmyyyy = do
        day <- dayOrMonth
        skipWhile $ inClass "a-zA-Z"
        spaceLike
        month <- fromMonthString
        spaceLike
        year <- decimal
        return (year,month,day)

    fromMonthString :: Parser Int
    fromMonthString = do
        let months =
                [ ("jan", 1)
                , ("feb", 2)
                , ("mar", 3)
                , ("apr", 4)
                , ("may", 5)
                , ("jun", 6)
                , ("jul", 7)
                , ("aug", 8)
                , ("sep", 9)
                , ("oct", 10)
                , ("nov", 11)
                , ("dec", 12)
                ]

        let tryMonths = choice $ flip fmap months $ \(mstr,mnum) -> do
                asciiCI mstr
                skipWhile $ inClass "a-zA-Z"
                return mnum

        tryMonths <?> "Couldn't parse month."

    dayOrMonth :: Parser Int
    dayOrMonth = numOfLength 2 <|> numOfLength 1

    dateSep :: Parser ()
    dateSep = skip (inClass "-:/ ")

    spaceLike :: Parser ()
    spaceLike = skipWhile $ inClass ", "

    numOfLength :: (Read a, Integral a) => Int -> Parser a
    numOfLength l = fmap read $ count l digit

-- parse a relative time into (years,months,days,ms) eg
-- "4 minutes and 3 seconds"
data RelativeTime = RelativeTime
    { relYears  :: Integer
    , relMonths :: Integer
    , relDays   :: Integer
    , relMs     :: Integer
    }
pRelativeTime :: Parser RelativeTime
pRelativeTime = do
    (y,m,d,ms) <- loop (0,0,0,0)
    return $ RelativeTime y m d ms
  where
    loop :: (Integer,Integer,Integer,Integer) -> Parser (Integer,Integer,Integer,Integer)
    loop (y,m,d,ms) = do
        (y',m',d',ms') <- parseRel
        skipSeperators
        let res = (y+y', m+m', d+d', ms+ms')
        loop res <|> return res
    parseRel :: Parser (Integer,Integer,Integer,Integer)
    parseRel = do
        num <- double
        skipMany space
        word <- fmap T.pack (many' letter)

            --relative to ms
        let seconds     = 1000
            minutes     = 60 * seconds
            hours       = 60 * minutes
            --relative to days
            weeks       = 7
            --relative to years
            decades     = 10
            eons        = 1000

        return $ case T.toLower word of
              --relative to ms
              (i ["ms", "milisecond", "miliseconds"] -> True) -> (0,0,0, round $ num         )
              (i ["s", "second", "seconds"]          -> True) -> (0,0,0, round $ num*seconds )
              (i ["m", "minute", "minutes"]          -> True) -> (0,0,0, round $ num*minutes )
              (i ["h", "hour", "hours"]              -> True) -> (0,0,0, round $ num*hours   )
              --relative to days
              (i ["d", "day", "days"]                -> True) -> (0,0, round $ num,       0)
              (i ["w", "week", "weeks"]              -> True) -> (0,0, round $ num*weeks, 0)
              --relative to months
              (i ["month", "months"]                 -> True) -> (0, round $ num, 0,0)
              --relative to years
              (i ["y", "year", "years"]              -> True) -> (round $ num,         0,0,0)
              (i ["decade", "decades"]               -> True) -> (round $ num*decades, 0,0,0)
              (i ["eon", "eons"]                     -> True) -> (round $ num*eons,    0,0,0)
              _                                               -> (round $ num*minutes, 0,0,0)
              where i (a:as) w = if a == w then True else i as w
                    i []     w = False
    skipSeperators :: Parser ()
    skipSeperators =
        let spaces = skipWhile $ inClass " \n\t,+"
        in spaces >> (skipMany $ choice [string "and"]) >> spaces

-- parse common greetings
pGreetings :: Parser T.Text
pGreetings = pIS "hello"
         <|> pIS "hi"
         <|> pIS "hiya"
         <|> pIS "hey"
         <|> pIS "howdy"
         <|> pIS "good morning"
         <|> pIS "good moro"
         <|> pIS "good day"
         <|> pIS "greetings"
         <|> pIS "what ho"
         <|> pIS "yo"
         <|> pIS "wassup"
         <|> pIS "whats up"

-- parse string, case insensitive for ascii.
pIS :: T.Text -> Parser T.Text
pIS = asciiCI

-- parse string
pS :: T.Text -> Parser T.Text
pS = string

-- auto-add optional spaces between entries.
infixl 6 <..>
(<..>) p1 p2 = p1 <+> many' space <+> p2