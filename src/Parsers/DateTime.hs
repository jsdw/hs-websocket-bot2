module Parsers.DateTime (
    parseTime
) where
--
-- Basic parsing of arbitrary english datetime strings
-- ===================================================
--
-- DAYNAME   = Monday(s) | Tuesday(s) | Wednesday(s) | Mon(s) | Tue(s) | Wed(s) ..
-- MONTHNAME = January | Febuary | March | ..
-- TIME      = 10 TIMESUFFIX | 10:53 (TIMESUFFIX) | 10:52:12 (TIMESUFFIX)
-- MONTHDAY  = 1st | 2nd | 3rd | ..
-- YEAR      = 2015 | '24
-- YMD       = 2015/12/14 | 2015-12-14
-- DMY       = 12/04/2015 | 12-04-2015
-- RELATIVE  = RELVALUE minute(s) | RELVALUE hour(s) | RELVALUE week(s) | RELVALUE DAYNAME | RELVALUE MONTHNAME
-- INTERVAL  = daily | weekly | monthly | yearly
--
-- TIMESUFFIX = am | pm | oclock | o'clock
-- FILLERS    = the | of | on | at | in | and | ,
-- RELVALUE   = next | 1 | 2 | 3 | ..
--
-- examples:
-- =========
--
-- Thursday the 5th of June, 10:33
-- (evaluate in order, so 5th June would override "Thursday")
--
-- next weds at 9pm
-- (move day forward to wednesday *next week*, 9pm, leave everything else)
--
-- 1st July
-- (set monthday and then month, rest unchanged)
--
-- 2016
-- (set year, rest unchanged)
--
-- 3 weeks on tuesday at 10pm
-- (move forward by 3 weeks, then move to the next tuesday, then set time to 10pm)
--
import qualified Data.Time                   as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Data.Text                   as T
import qualified Data.List                   as L
import           Data.Attoparsec.Text
import           Control.Applicative
import           Control.Monad

--
-- Glue it all together here in our single exposed function
--

parseTime :: Time.ZonedTime -> Parser Time.ZonedTime
parseTime zonedTime = loop zonedTime
  where
    parsers = [yymmdd, relTime, year, hms, monthDay]
    loop time = do
        timeSep
        newtime <- (choice $ fmap (\p -> p time) parsers) <?> "Could not parse any time"
        timeSep
        loop newtime <|> return newtime

--
-- Auxiliary helpers:
--

fillers = ["the","of","on","at","in","and",",","+"]

numbers =
    [ ("one",   1)
    , ("two",   2)
    , ("three", 3)
    , ("four",  4)
    , ("five",  5)
    , ("six",   6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine",  9)
    , ("ten",  10)
    ]

numberMultipliers =
    [ ("hundred",       100)
    , ("thousand", thousand)
    , ("million",   million)
    , ("billion",   billion)
    , ("trillion", trillion)
    ]
  where
    thousand = 1000
    million  = thousand * thousand
    billion  = million  * thousand
    trillion = billion  * thousand

days =
    [ (["mon", "monday", "mondays"],                      1)
    , (["tue", "tues", "tuesday", "tuesdays"],            2)
    , (["wed", "weds", "wednesday", "wednesdays"],        3)
    , (["thu", "thur", "thurs", "thursday", "thursdays"], 4)
    , (["fri", "fris", "friday", "fridays"],              5)
    , (["sat", "sats", "saturday", "saturdays"],          6)
    , (["sun", "suns", "sunday", "sundays"],              7)
    ]

months =
    [ (["jan", "january", "januaries"],                           1)
    , (["feb", "febuary", "february", "februaries", "febuaries"], 2)
    , (["mar", "march", "marchs", "marches"],                     3)
    , (["apr", "april", "aprils"],                                4)
    , (["may", "mays"],                                           5)
    , (["jun", "june", "junes"],                                  6)
    , (["jul", "july", "julys", "julies"],                        7)
    , (["aug", "august", "augusts"],                              8)
    , (["sep", "sept", "september", "septembers"],                9)
    , (["oct", "october", "octobers"],                           10)
    , (["nov", "november", "novembers"],                         11)
    , (["dec", "december", "decembers"],                         12)
    ]

data RelTime = RelTime { relYears::Integer,relMonths::Integer,relDays::Integer,relMs::Integer }
toRelTime num =
    --relative to ms
    [ (["ms", "milisecond", "miliseconds"]       , RelTime 0 0 0 (num        ) )
    , (["s", "secs", "second", "seconds"]        , RelTime 0 0 0 (num*seconds) )
    , (["h", "hour", "hours"]                    , RelTime 0 0 0 (num*hours  ) )
    , (["m", "min", "minute", "mins", "minutes"] , RelTime 0 0 0 (num*minutes) )
    --relative to days
    , (["d", "day", "days"]   , RelTime 0 0  (num      ) 0 )
    , (["w", "week", "weeks"] , RelTime 0 0  (num*weeks) 0 )
    --relative to months
    , (["month", "months"]    , RelTime 0 (num) 0 0 )
    --relative to years
    , (["y", "year", "years"] , RelTime (num        ) 0 0 0 )
    , (["decade", "decades"]  , RelTime (num*decades) 0 0 0 )
    , (["eon", "eons"]        , RelTime (num*eons   ) 0 0 0 )
    ]
  where
    seconds     = 1000
    minutes     = 60 * seconds
    hours       = 60 * minutes
    --relative to days
    weeks       = 7
    --relative to years
    decades     = 10
    eons        = 1000

timeSuffix =
    [ (["pm"], 12)
    , (["am", "oclock", "o'clock"], 0)
    ]

timeSep :: Parser ()
timeSep = skipSpace >> ( (choice (fmap string fillers) >> skipSpace) <|> return () )

yymmdd :: Time.ZonedTime -> Parser Time.ZonedTime
yymmdd (ZonedTime (YMD y m d) tod tz) = do
    (y',m',d') <- ymd
    return $ ZonedTime (toYMD y' m' d') tod tz
  where
    sep = char '/' <|> char '-'
    add2k y = if y < 100 then y + 2000 else y
    ymd :: Parser (Integer,Int,Int)
    ymd = do
        (first,second,third) <- (,,) <$> decimal <*> (sep >> decimal) <*> (sep >> decimal)
        if second > 12 then fail "pDate: month greater than 12" else return ()
        return $ if first > 31
            then (add2k first,fromIntegral second,fromIntegral third)
            else (add2k first,fromIntegral second,fromIntegral first)

relTime :: Time.ZonedTime -> Parser Time.ZonedTime
relTime z@(ZonedTime ymd@(YMD y m d) tod tz) = do
    (num :: Integer) <- decimal
       <|> basicNumber
       <|> (string "next" >> return 1)
       <|> (string "this" >> return 0)
       <|> return 0
    skipSpace
    suffix <- fmap T.pack (many1 letter)

    case suffix of
        (getFrom months          -> Just val) -> return $ relMonth   num val
        (getFrom days            -> Just val) -> return $ relWeekDay num val
        (getFrom (toRelTime num) -> Just val) -> return $ relTime    val
        _                                     -> fail "Could not parse relative time"
  where
    relTime v = addToZonedTime z v
    relWeekDay num v =
        let wd = toInteger w where (_,_,w) = Time.toWeekDate ymd
            plusDays = case num of
                0 -> if v < wd then 7 - (wd - v) else (v - wd)
                _ -> (7 - wd) + (num * v)
        in addToZonedTime z (RelTime 0 0 plusDays 0)
    relMonth num v =
        let m' = toInteger m
            plusMonths = case num of
                0 -> if v < m' then 12 - (m' - v) else (v - m')
                _ -> (12 - m') + (num * 12)
        in addToZonedTime z (RelTime 0 plusMonths 0 0)
    basicNumber =
        let pick from = choice $ fmap (\(s,v) -> string s >> return v) from
            loop n = skipSpace >> ((fmap (n*) (pick numberMultipliers) >>= loop) <|> return n)
        in pick numbers >>= loop

year :: Time.ZonedTime -> Parser Time.ZonedTime
year (ZonedTime (YMD y m d) tod tz) = do
    year <- decimal <|> yearlike
    if year < 1000
        then fail "doesn't look yearish"
        else return $ (ZonedTime (toYMD year m d) tod tz)
  where
    twoDigits = fmap read $ count 2 digit
    yearlike = do
        char '\''
        n <- twoDigits
        return $ 2000 + n

hms :: Time.ZonedTime -> Parser Time.ZonedTime
hms (ZonedTime day tod tz) = hoursonly <|> all
  where
    hoursonly = do
        h <- decimal
        skipSpace
        plusHours <- suffix
        out h 0 0 plusHours
    all = do
        h <- decimal
        m <- (sep >> decimal)
        s <- (sep >> decimal) <|> return 0
        skipSpace
        plusHours <- suffix <|> return 0
        out h m s plusHours
    out h' m s plus = do
        let h = if h' < 12 then h' + plus else h'
        guard $ m >= 0 && m < 60 && s >= 0 && s < 60 && h >= 0 && h < 24
        return $ ZonedTime day (ToD h m (fromIntegral s)) tz
    suffix = do
        word <- many1 letter
        case getFrom timeSuffix (T.pack word) of
            Just val -> return val
            Nothing  -> fail "not a valid time suffix"
    sep = char ':' <|> char '.'

monthDay :: Time.ZonedTime -> Parser Time.ZonedTime
monthDay (ZonedTime (YMD y m d) tod tz) = do
    day <- decimal
    skipSpace
    suffix <- many1 letter

    let f = fail "not a month day :("
        newzt d = (ZonedTime (toYMD y m d) tod tz)
        suf days val day =
            let m = L.any (== day) days
            in if m then (if suffix == val then Just (return $ newzt day) else Just f)
                    else Nothing

    case day of
        (suf [1,21,31] "st" -> Just o) -> o
        (suf [2,22]    "nd" -> Just o) -> o
        (suf [3,23]    "rd" -> Just o) -> o
        (suf [1..31]   "th" -> Just o) -> o
        _                              -> f

getFrom :: [([T.Text],a)] -> T.Text -> Maybe a
getFrom vals word =
    let get ((ds,val):rest) w = if L.any (== w) ds then Just val else get rest w
        get [] w = Nothing
    in get vals (T.toLower word)

addToZonedTime :: Time.ZonedTime -> RelTime -> Time.ZonedTime
addToZonedTime (ZonedTime day tod tz) RelTime{..} =
    let d' = Time.addDays                relDays
           . Time.addGregorianMonthsClip relMonths
           . Time.addGregorianYearsClip  relYears
           $ day
        utc = Time.localTimeToUTC tz (Time.LocalTime d' tod)
    in Time.utcToZonedTime tz $ (fromIntegral relMs/1000) `Time.addUTCTime` utc

-- make it a little easier to speak times (to's for consistency):
pattern ZonedTime ymd tod tz = Time.ZonedTime (Time.LocalTime ymd tod) tz
toZonedTime ymd tod = Time.ZonedTime (Time.LocalTime ymd tod)
pattern ToD h m s = Time.TimeOfDay h m s
toToD = Time.TimeOfDay
pattern YMD y m d <- (Time.toGregorian -> (y,m,d))
toYMD = Time.fromGregorian