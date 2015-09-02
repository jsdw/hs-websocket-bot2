{-# LANGUAGE ViewPatterns #-}

module Parsers (
    --export everything visible in this module as-is:
    module Parsers
) where

import Internal.Routing
import qualified Parsers.DateTime     as DT

import           Prelude              hiding (take)
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import qualified Data.List            as L
import qualified Data.Time            as Time
import           Data.Time            as Time
import           Control.Applicative
import           Control.Monad
import           Data.Monoid          ((<>))
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

--
-- Parse some relative or absolute time from now and give it back
-- as an absolute UTC time. This does all the hard work not only
-- of parsing time but converting from relative to absolute and
-- handling local to UTC conversion.
--
pTime :: RoutesInput -> Parser Time.UTCTime
pTime RoutesInput{ routesTime = t, routesTimeZone = tz } = do
    (Time.ZonedTime newLocalTime _) <- DT.parseTime $ Time.ZonedTime (Time.utcToLocalTime tz t) tz
    return $ Time.localTimeToUTC tz newLocalTime

pIntervalTime :: RoutesInput -> Parser (DT.Interval, Time.UTCTime)
pIntervalTime RoutesInput{ routesTime = t, routesTimeZone = tz } = do
    (interval, Time.ZonedTime newLocalTime _) <- DT.intervalTime $ Time.ZonedTime (Time.utcToLocalTime tz t) tz
    return $ (interval, Time.localTimeToUTC tz newLocalTime)

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

-- parse single name
pName :: Parser T.Text
pName = mappend <$> (string "@" <|> return "") <*> (fmap T.pack $ many1 letter)

-- auto-add optional spaces between entries.
infixl 6 <..>
(<..>) p1 p2 = p1 <+> many' space <+> p2
