module Commands (
    -- re-export it all:
    module Commands
) where

import Internal.Routing
import Internal.Types

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

-- get the full message:
getMessage :: RouteStateIO T.Text
getMessage = do
    state <- S.get
    return $ rsMessage state

-- get the username of the sender:
getName :: RouteStateIO T.Text
getName = do
    state <- S.get
    return $ rsName state

-- sleep for some number of ms:
sleepMs :: MonadIO m => Int -> m ()
sleepMs num = liftIO $ threadDelay (num*1000)

getTime :: MonadIO m => m Time.ZonedTime
getTime = liftIO $ Time.getZonedTime

addMs :: Integral n => Time.ZonedTime -> n -> Time.ZonedTime
addMs time ms = Time.utcToZonedTime tz newUtcTime
  where utcTime = Time.zonedTimeToUTC time
        tz = Time.zonedTimeZone time
        newUtcTime = Time.addUTCTime ((fromIntegral ms) / 1000) utcTime

formatTime :: Time.FormatTime t => String -> t -> T.Text
formatTime s t = T.pack $ Time.formatTime defaultTimeLocale s t

random :: (MonadIO m, Random a) => m a
random = liftIO $ randomIO

randomRange :: (MonadIO m, Random a) => (a,a) -> m a
randomRange r = liftIO $ randomRIO r