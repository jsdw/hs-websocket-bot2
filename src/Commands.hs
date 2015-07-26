module Commands (
    -- re-export it all:
    module Commands
) where

import Internal.Routing
import Internal.Types

import           Control.Monad              (mzero, guard)
import           Control.Monad.Trans        (lift)
import           Control.Monad.State        (liftIO, void)
import           Control.Concurrent         (threadDelay)
import qualified Control.Monad.State        as S
import qualified Data.Text                  as T
import           Data.Aeson                 (ToJSON)
import           Data.Default               (def)
import qualified Data.Time                  as Time
import           Data.Time.Format           (defaultTimeLocale)

-- stop computation:
exit :: RouteStateIO ()
exit = lift mzero

-- respond with a message:
respond :: T.Text -> RouteStateIO ()
respond out = do
    state <- S.get
    liftIO $ rsReplyFn state $ def
        { resMessage = out 
        , resRoom = rsRoom state
        }

-- respond with a coloured message:
respondWithColour :: MessageColour -> T.Text -> RouteStateIO ()
respondWithColour col out = do
    state <- S.get
    liftIO $ rsReplyFn state $ def
        { resMessage = out
        , resColour = Just col
        , resRoom = rsRoom state
        }

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
sleepMs :: Int -> RouteStateIO ()
sleepMs num = liftIO $ threadDelay (num*1000)

getTime :: RouteStateIO Time.ZonedTime
getTime = liftIO $ Time.getZonedTime

addMs :: Integral n => Time.ZonedTime -> n -> Time.ZonedTime
addMs time ms = Time.utcToZonedTime tz newUtcTime
  where utcTime = Time.zonedTimeToUTC time
        tz = Time.zonedTimeZone time
        newUtcTime = Time.addUTCTime ((fromIntegral ms) / 1000) utcTime

formatTime :: Time.FormatTime t => String -> t -> T.Text
formatTime s t = T.pack $ Time.formatTime defaultTimeLocale s t