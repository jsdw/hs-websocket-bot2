module Commands (
    -- re-export it all:
    module Commands
) where

import Internal.Routing
import Internal.Types
import Parsers          (RelativeTime(..))
import Tools.Reminders

import           Control.Applicative
import           Control.Monad              (mzero, guard, void)
import           Control.Monad.Trans        (lift, MonadIO, liftIO)
import           Control.Concurrent         (threadDelay)
import qualified Control.Monad.State        as S
import qualified Data.Text                  as T
import           Data.Aeson                 (ToJSON)
import           Data.Default
import qualified Data.Time                  as Time
import           Data.Time.Format           (defaultTimeLocale)
import           System.Random              (Random, randomRIO, randomIO)

exit :: RouteStateIO ()
exit = lift mzero

data ResponseOpts = ResponseOpts
    { roRoom   :: Maybe T.Text
    , roColour :: Maybe MessageColour
    , roSlow   :: Bool
    }
instance Default ResponseOpts where
    def = ResponseOpts def def False

respondWith :: ResponseOpts -> T.Text -> RouteStateIO ()
respondWith ResponseOpts{..} txt = do
    state <- S.get
    if roSlow then sleepMs $ (T.length txt) * 110 else return ()
    liftIO $ rsReplyFn state $ def
        { resMessage = txt
        , resRoom = roRoom <|> rsRoom state
        , resColour = roColour
        }

respond :: T.Text -> RouteStateIO ()
respond out = do
    state <- S.get
    liftIO $ rsReplyFn state $ def
        { resMessage = out
        , resRoom = rsRoom state
        }

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

addRelativeToUTC :: RelativeTime -> Time.UTCTime -> Time.UTCTime
addRelativeToUTC RelativeTime{..} Time.UTCTime{..} =
    let d' = Time.addDays                relDays
           . Time.addGregorianMonthsClip relMonths
           . Time.addGregorianYearsClip  relYears
           $ utctDay
    in (fromIntegral relMs/1000) `Time.addUTCTime` (Time.UTCTime d' utctDayTime)
