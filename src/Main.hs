{-# LANGUAGE
    OverloadedStrings,
    DeriveFunctor #-}

import Internal.Routing
import Internal.Commands
import Internal.WebSocket

import qualified Data.Text          as T
import           Data.Monoid        ((<>))

--
-- Interpreters; to be moved to other file.
--

-- print commands to string:
toText :: Show n => Command n -> T.Text
toText (Pure res) = "Return: " <> (showT res) <> "\n"
toText (Impure m) = t m
  where
    t (Message txt next) = "Message: " <> txt <> "\n" <> (toText next)
    t (Sleep int next) = "Sleep for " <> (showT int) <> "ms\n" <> (toText next)
    t _ = "Unknown command :("


toString cmd = T.unpack (toText cmd)

showT :: Show a => a -> T.Text
showT = T.pack . show