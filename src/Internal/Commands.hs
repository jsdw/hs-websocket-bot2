{-# LANGUAGE
    PatternSynonyms, 
    DeriveFunctor #-}

module Internal.Commands (

    -- types
    Command,
    Cmd(..),

    -- allow matching on free monad pieces
    -- without having to expose them.
    pattern Pure,
    pattern Impure,

    -- commands available to use:
    message,
    sleep

) where

import qualified Control.Monad.Free as F
import qualified Data.Text          as T
import           Data.Monoid        ((<>))

pattern Pure a = F.Pure a
pattern Impure a = F.Impure a

--
-- First, add commands to command type.
--
data Cmd next
    = Message T.Text next
    | Sleep Int next
    | RandomInt (Int -> next)
    | End
    deriving Functor

type Command = F.Free Cmd

--
-- Next, add a simpler interface to create them
-- prewrapped into a free monad:
--

message txt = liftF $ Message txt ()
sleep val = liftF $ Sleep val ()


-- Utility to wrap commands into free monad.
liftF :: Cmd next -> Command next
liftF cmd = F.Impure $ fmap F.Pure cmd