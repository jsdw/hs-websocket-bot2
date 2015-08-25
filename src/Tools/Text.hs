module Tools.Text (

    swapFirstAndSecondPerson

) where

import qualified Data.Attoparsec.Text       as P
import qualified Data.Text                  as T
import qualified Data.List                  as L
import           Control.Applicative
import           Data.Foldable
import           Data.Monoid

-- swap out first and second person text for eachother. very basic,
-- misses loads of cases and makes some silly mistakes as well!
--
-- my   <=> your
-- me/I <=> you
--

swapFirstAndSecondPerson :: T.Text -> T.Text
swapFirstAndSecondPerson txt = finalize $ foldl' folder "" (tokenize txt)
  where
    swaps :: [(T.Text,T.Text)]
    swaps = fTos ++ fmap (\(f,s) -> (s,f)) fTos
      where
        fTos =
            [ ("my","your")
            , ("his","your")
            , ("her","your")
            , ("'m","'re")
            , ("i","you")
            , ("me","you")
            , ("we","you")
            , ("am","are")
            , ("im","youre")
            ]

    folder :: T.Text -> T.Text -> T.Text
    folder acc word = case L.lookup w swaps of
        Just v -> acc <> case c of
            CapFirst -> T.toTitle v
            CapAll   -> T.toUpper v
            _        -> v
        Nothing     -> acc <> word
      where
        w = T.toLower word
        c = case L.lookup w [("i",CapNone),("you",CapAll)] of
            Just cs -> cs
            Nothing -> capState word

    tokenize :: T.Text -> [T.Text]
    tokenize txt = case P.parseOnly (P.many' (pword <|> psuffix <|> psep)) txt of
        Right arr -> arr
        Left _ -> []
      where
        pword = P.takeWhile1 (P.inClass "a-zA-Z")
        psuffix = P.char '\'' >> pword >>= return . ("'" <>)
        psep = P.takeWhile1 (not . P.inClass "a-zA-Z")

    finalize :: T.Text -> T.Text
    finalize txt =
        if T.takeEnd 1 txt == "I"
            then T.dropEnd 1 txt <> "me"
            else txt

--
-- Determine roughly what capitalism state a word is in:
--
data CapState = CapFirst | CapAll | CapNone deriving Show

capState :: T.Text -> CapState
capState word
    | T.toTitle word == word = CapFirst
    | T.toUpper word == word = CapAll
    | otherwise              = CapNone