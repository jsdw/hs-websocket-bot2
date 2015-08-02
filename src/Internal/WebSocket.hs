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
startServer :: (ToJSON b, FromJSON a) => ServerSettings -> IO (IO a, b -> IO ())
startServer ServerSettings{..} = do

    sLogger $ "Starting server"
    sLogger $ "==============="
    sLogger $ "Port:    " <> T.pack (show sPort)
    sLogger $ "Address: " <> sAddress

    (in_read, in_write) <- makeChan
    (out_read, out_write) <- makeChan

    forkIO $ WS.runServer (T.unpack sAddress) sPort $ application (in_read, out_write) sError
    return (out_read, in_write)

application :: (FromJSON b, ToJSON a)
            => (IO a, b -> IO ())
            -> (ServerError -> IO ())           -- an error callback
            -> WS.ServerApp                     -- the server app constructed
application (read,write) err pending = do

    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    forkIO $ forever $ do
        msg <- read
        WS.sendTextData conn $ JSON.encode msg

    forever $ do
        msg <- WS.receiveData conn :: IO BL.ByteString
        let mRes = JSON.decode msg

        case mRes of
            Nothing -> err (ParseFailureError msg)
            Just res -> write res

-- The default error handler logs to stderr
-- the default logger logs to stdout
-- the default callback does nothing
defaultErrorHandler :: ServerError -> IO ()
defaultErrorHandler (ParseFailureError bs) = T.hPutStrLn IO.stderr $ "Error parsing from JSON: "<>T.decodeUtf8 (BL.toStrict bs)

defaultLogHandler :: T.Text -> IO ()
defaultLogHandler str = T.hPutStrLn IO.stdout str
