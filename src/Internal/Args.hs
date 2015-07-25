module Internal.Args (

    parse,
    parseKeys,
    ignoreKeys

) where

import qualified Data.Map                  as M

--very simple, just split args into keyed and "other", maintaining order of other.
--keyed args begin with - or --. no protection against supplying args we dont want.

parseKeys :: [String] -> M.Map String String
parseKeys args = fst $ parse args

ignoreKeys :: [String] -> [String]
ignoreKeys args = snd $ parse args

parse :: [String] -> (M.Map String String, [String])
parse args = parseArgs args M.empty []
  where
    parseArgs [] keyed other = (keyed, other)
    parseArgs (('-':'-':key):val:rest) keyed other = parseArgs rest (M.insert key val keyed) other
    parseArgs (('-':key):val:rest) keyed other = parseArgs rest (M.insert key val keyed) other
    parseArgs (val:rest) keyed other = parseArgs rest keyed (other ++ [val])

