module Internal.WebSocket (

    ServerSettings(..),
    ServerError(..),
    startServer

) where

import           Internal.Channel

import           Data.Aeson                 (FromJSON,ToJSON)
import qualified Data.Aeson                 as JSON
import qualified Network.WebSockets         as WS
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding         as T
import           Data.Default               (def,Default)
import           Data.Monoid                ((<>))
import           Control.Monad.Trans        (liftIO, MonadIO)
import qualified System.IO                  as IO
import qualified Control.Exception          as E
import           Control.Monad
import           Control.Concurrent

data ServerSettings = ServerSettings
    { sAddress :: T.Text
    , sPort :: Int
    , sError :: ServerError -> IO ()
    , sLogger :: T.Text -> IO ()
    }

data ServerError = ParseFailureError BL.ByteString

instance Default ServerSettings where
    def = ServerSettings
        { sAddress = "0.0.0.0"
        , sPort = 9090
        , sError = defaultErrorHandler
        , sLogger = defaultLogHandler
        }


-- this is the main export from here. provide it some
-- settings and it'll run a server and fire the relevant
-- callback with pre-decoded json where necessary, or
-- error cb if something goes wrong.
startServer :: (ToJSON b, FromJSON a)
            => ServerSettings
            -> (IO a -> (b -> IO ()) -> IO ()) -- callback takes thing to read and thing to write to
            -> IO ()
startServer ServerSettings{..} callback = do

    sLogger $ "Starting server"
    sLogger $ "==============="
    sLogger $ "Port:    " <> T.pack (show sPort)
    sLogger $ "Address: " <> sAddress

    WS.runServer (T.unpack sAddress) sPort $ application callback sError


-- this is run whenever a new connection is established.
-- when it finishes, the new connection is closed. We rely
-- on the user supplied callback to block if it wants to keep
-- open.
application :: (FromJSON a, ToJSON b)
            => (IO a -> (b -> IO ()) -> IO ())  -- our callback which will read/write messages
            -> (ServerError -> IO ())           -- an error callback
            -> WS.ServerApp                     -- the server app constructed
application callback err pending = do

    (in_read, in_write) <- makeChan
    (out_read, out_write) <- makeChan

    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    tid1 <- toClient conn in_read
    tid2 <- fromClient conn out_write

    callback out_read in_write `E.finally` do
        killThread tid1
        killThread tid2

  where
    -- read messages from the callback and send them
    -- to the connected client
    toClient conn read = forkIO $ forever $ do
        msg <- read
        WS.sendTextData conn $ JSON.encode msg
    -- receive messages from the connected client and
    -- send them on to the callback
    fromClient conn write = do
        tid0 <- myThreadId
        forkIO $ flip E.finally (E.throwTo tid0 E.ThreadKilled) $ forever $ do
            msg <- WS.receiveData conn :: IO BL.ByteString
            case JSON.decode msg of
                Nothing -> err (ParseFailureError msg)
                Just res -> write res

-- The default error handler logs to stderr
-- the default logger logs to stdout
-- the default callback does nothing
defaultErrorHandler :: ServerError -> IO ()
defaultErrorHandler (ParseFailureError bs) = T.hPutStrLn IO.stderr $ "Error parsing from JSON: "<>T.decodeUtf8 (BL.toStrict bs)

defaultLogHandler :: T.Text -> IO ()
defaultLogHandler str = T.hPutStrLn IO.stdout str
