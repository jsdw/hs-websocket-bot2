module Parsers.DateTime (

) where
--
-- Basic parsing of arbitrary english datetime strings
-- ===================================================
--
-- DAYNAME   = Monday | Tuesday | Wednesday | Mon | Tue(s) | Wed(s) ..
-- MONTHNAME = January | Febuary | March | ..
-- TIME      = 10pm | 10:53 | 10:53pm | 10:52:12 | 10:52:12pm
-- MONTHDAY  = 1st | 2nd | 3rd | ..
-- YEAR      = 2015
-- YMD       = 2015/12/14 | 2015-12-14
-- DMY       = 12/04/2015 | 12-04-2015
-- RELATIVE  = RELVALUE minutes | RELVALUE hours | RELVALUE weeks
-- RELDAY    = next DAYNAME
-- INTERVAL  = daily | weekly | monthly | yearly
--
-- FILLERS   = the | of | on | at | in | and | ,
-- RELVALUE  = next | 1 | 2 | 3 | ..
--
-- examples:
-- =========
--
-- Thursday the 5th of June, 10:33
-- (evaluate in order, so 5th June would override "Thursday")
--
-- next weds at 9pm
-- (move day forward to next wednesday (RELVERB), 9pm, leave everything else)
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
import qualified Data.Time            as Time
import qualified Data.Text            as T
import qualified Data.List            as L
import           Data.Attoparsec.Text
import           Control.Applicative
import           Control.Monad



parseTime :: Time.UTCTime -> Parser Time.UTCTime
parseTime time = do


    return time


--
-- Auxiliary helpers:
--

days =
    [ (["mon", "monday"], 1)
    , (["tue", "tues", "tuesday"], 2)
    , (["wed", "weds", "wednesday"], 3)
    , (["thu", "thur", "thurs", "thursday"], 4)
    , (["fri", "friday"], 5)
    , (["sat", "saturday"], 6)
    , (["sun", "sunday"], 7)
    ]

months =
    [ (["jan", "january"], 1)
    , (["feb", "febuary"], 2)
    , (["mar", "march"], 3)
    , (["apr", "april"], 4)
    , (["may"], 5)
    , (["jun", "june"], 6)
    , (["jul", "july"], 7)
    , (["aug", "august"], 8)
    , (["sep", "sept", "september"], 9)
    , (["oct", "october"], 10)
    , (["nov", "november"], 11)
    , (["dec", "december"], 12)
    ]

relSuffixMultiplier num =
    --relative to ms
    [ (["ms", "milisecond", "miliseconds"] , (0,0,0, round $ num         ) )
    , (["s", "secs", "second", "seconds"]  , (0,0,0, round $ num*seconds ) )
    , (["h", "hour", "hours"]              , (0,0,0, round $ num*hours   ) )
    , (["m", "min", "mins", "minutes"]     , (0,0,0, round $ num*minutes ) )
    --relative to days
    , (["d", "day", "days"]                , (0,0, round $ num,       0) )
    , (["w", "week", "weeks"]              , (0,0, round $ num*weeks, 0) )
    --relative to months
    , (["month", "months"]                 , (0, round $ num, 0,0) )
    --relative to years
    , (["y", "year", "years"]              , (round $ num,         0,0,0) )
    , (["decade", "decades"]               , (round $ num*decades, 0,0,0) )
    , (["eon", "eons"]                     , (round $ num*eons,    0,0,0) )
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

relTimeAsTuple :: Parser (Integral,Integral,Integral,Integral)
relTimeAsTuple = do
    num <- double
    let (y,m,d,ms) = getFrom (relSuffixMultiplier num)
    return (y+y',m+m',d+d',ms+ms')

dayNameAsInt :: Parser Int
dayNameAsInt = do
    word <- many1 letter
    case getFrom days (T.pack word) of
        Just n  -> return n
        Nothing -> fail "Could not match day :("

monthNameAsInt :: Parser Int
monthNameAsInt = do
    word <- many1 letter
    case getFrom months (T.pack word) of
        Just n  -> return n
        Nothing -> fail "Could not match month :("

getFrom :: [([T.Text],a)] -> T.Text -> Maybe a
getFrom vals word =
    let get ((ds,val):rest) w = if L.any (== w) ds then Just val else get rest w
        get [] w = Nothing
    in get vals (T.toLower word)


