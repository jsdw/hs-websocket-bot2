module Parsers (
    --export everything visible in this module as-is:
    module Parsers
) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text            as T

--
-- This module contains all of the parsers we'll use in routes
--