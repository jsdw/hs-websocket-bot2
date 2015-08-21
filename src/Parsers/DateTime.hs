module Parsers.DateTime (

) where
--
-- Basic parsing of arbitrary english datetime strings
-- ===================================================
--
-- DAYNAME   = Monday(s) | Tuesday(s) | Wednesday(s) | Mon(s) | Tue(s) | Wed(s) ..
-- MONTHNAME = January | Febuary | March | ..
-- TIME      = 10 TIMESUFFIX | 10:53 (TIMESUFFIX) | 10:52:12 (TIMESUFFIX)
-- MONTHDAY  = 1st | 2nd | 3rd | ..
-- YEAR      = 2015
-- YMD       = 2015/12/14 | 2015-12-14
-- DMY       = 12/04/2015 | 12-04-2015
-- RELATIVE  = RELVALUE minute(s) | RELVALUE hour(s) | RELVALUE week(s) | RELVALUE DAYNAME
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
import           Data.Time            as Time
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import           Control.Applicative
import           Control.Monad