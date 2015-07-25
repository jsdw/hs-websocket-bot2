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

--
-- This module contains all of the parsers we'll use in routes
--

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


pS :: T.Text -> Parser T.Text
pS = string