module Commands (
    -- re-export it all:
    module Commands
) where

import Internal.Routing
import Internal.Types
import Tools.Reminders

import           Control.Applicative
import           Control.Monad              (mzero, guard, void)
import           Control.Monad.Trans        (lift, MonadIO, liftIO)
import           Control.Concurrent         (threadDelay)
import qualified Control.Monad.State        as S
import qualified Data.Text                  as T
import           Data.Aeson                 (ToJSON)
import           Data.Default               (def)
import qualified Data.Time                  as Time
import           Data.Time.Format           (defaultTimeLocale)
import           System.Random              (Random, randomRIO, randomIO)

exit :: RouteStateIO ()
exit = lift mzero

respond :: T.Text -> RouteStateIO ()
respond out = do
    state <- S.get
    liftIO $ rsReplyFn state $ def
        { resMessage = out
        , resRoom = rsRoom state
        }

respondWithColour :: MessageColour -> T.Text -> RouteStateIO ()
respondWithColour col out = do
    state <- S.get
    liftIO $ rsReplyFn state $ def
        { resMessage = out
        , resColour = Just col
        , resRoom = rsRoom state
        }

respondSlowly :: T.Text -> RouteStateIO ()
respondSlowly t = do
    sleepMs $ (T.length t) * 110
    respond t

respondSlowlyWithColour :: MessageColour -> T.Text -> RouteStateIO ()
respondSlowlyWithColour col t = do
    sleepMs $ (T.length t) * 110
    respondWithColour col t

-- get reminders object:
askReminders :: RouteStateIO (Reminders MessageResponse)
askReminders = rsReminders <$> S.get

-- get the full message:
askMessage :: RouteStateIO T.Text
askMessage = rsMessage <$> S.get

-- get the username of the sender:
askName :: RouteStateIO T.Text
askName = rsName <$> S.get

-- get the room of the sender:
askRoom :: RouteStateIO (Maybe T.Text)
askRoom = rsRoom <$> S.get


-- sleep for some number of ms:
sleepMs :: MonadIO m => Int -> m ()
sleepMs num = liftIO $ threadDelay (num*1000)

random :: (MonadIO m, Random a) => m a
random = liftIO $ randomIO

randomRange :: (MonadIO m, Random a) => (a,a) -> m a
randomRange r = liftIO $ randomRIO r
