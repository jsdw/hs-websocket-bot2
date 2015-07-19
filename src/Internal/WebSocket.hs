{-# LANGUAGE 
    RecordWildCards #-}

module Internal.WebSocket (


) where 

import           Data.Aeson                 ((.=),FromJSON,ToJSON,parseJSON,toJSON,Object)
import           Data.Aeson                 as JSON
import qualified Network.WebSockets         as WS
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.Encoding         as T
import           Data.Default               (def,Default)
import           Data.Monoid                ((<>))
import           Control.Monad.Trans        (liftIO, MonadIO)



--import           Prelude                    hiding (print)
--import           Data.Text                  (Text)
--import qualified Data.Text                  as T
--import qualified Data.Text.IO               as T
--import qualified Data.Text.Encoding         as T
--import qualified Data.ByteString.Lazy.Char8 as B
--import           Control.Concurrent
--import           Control.Monad.Trans        (liftIO)
--import           Control.Monad              (forever,mzero)
--import           Control.Applicative        ((<|>),(<$>),(<*>))
--import qualified Control.Exception          as E
--import           Control.Lens
--import           System.Environment         (getArgs)
--import qualified Data.Map                   as M            
--import           Text.Read                  (readMaybe)

--import           Data.Default               (def,Default)
--import qualified Data.Attoparsec.Text       as P


--
-- Socket server needs:
--
--   address  (String) eg "127.0.0.1"
--     address server will listen on
--   port     (Int)    eg 9090
--     port server will listen on
--   callback (FromJson a => a -> IO b)
--     what to do when a (valid) message is received.
--   onError (ServerError -> IO a)
--     run this func if an error occurs, passing message.
--     use ServerError type to encode different error types.
--
-- We want to keep this general, but we should be able
-- to auto-parse from JSON depending on input type of
-- callback, and then not fire callback if parsing
-- fails (fallback to logging error somewhere).
--

data ServerSettings = ServerSettings 
    { sAddress :: String
    , sPort :: Int
    , sCallback :: forall a m. (MonadIO m, FromJSON a) => a -> m ()
    , sError :: forall m. MonadIO m => ServerError -> m ()
    , sLogger :: forall m. MonadIO m => String -> m ()
    }

data ServerError = ParseFailureError B.ByteString

instance Default ServerSettings where
    def = ServerSettings
        { sAddress = "0.0.0.0"
        , sPort = 9090
        , sCallback = \a -> return ()
        , sError = defaultErrorHandler
        , sLogger = defaultLogHandler
        }


startServer :: ServerSettings -> IO ()
startServer ServerSettings{..} = do

    sLogger $ "Starting server"
    sLogger $ "==============="
    sLogger $ "Port:    " <> (show sPort)
    sLogger $ "Address: " <> sAddress

    --WS.runServer sAddress sPort $ application sCallback sError

    return ()

application cb err = undefined

-- The default error handler logs to stderr
-- the default logger logs to stdout.
defaultErrorHandler :: MonadIO m => ServerError -> m ()
defaultErrorHandler (ParseFailureError bs) = undefined

defaultLogHandler :: MonadIO m => String -> m ()
defaultLogHandler str = undefined



----
---- The retrieve/reply loop. generate a response given the
---- current bot state and brain and some message and connection
---- to send responses to
----
-- application botState botBrain pending = flip E.finally disconnect $ do

--    liftIO $ putStrLn "Connection established"
--    conn <- WS.acceptRequest pending
--    WS.forkPingThread conn 30

--    forever $ do 
--        msg <- WS.receiveData conn

--        let m = decode msg :: Maybe ClientMessage
--        case m of
--            Just message -> do

--                printLn "message from {} ({}): {}" (message^.cName, message^.cRoom, message^.cMessage)

--                --fork a new thread so that we don't block
--                --this loop if we want timers etc
--                liftIO $ forkIO $ generateResponse message botState botBrain conn
--                return ()

--            Nothing -> printLn "bad input: {}" (Only msg)

--  where 
--    disconnect = printLn "Closing connection" ()

----
---- Our entry point
----
-- main = do

--    argMap <- fmap parseKeys getArgs

--    --bot state lives here
--    state <- newMVar def :: IO BotState
    
--    --bot brain built up here
--    let brain = runBrainBuilder buildRules

--    --parse port number from args
--    let (Just port) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 9090
--          where maybeP = M.lookup "port" argMap <|> M.lookup "p" argMap

--    --parse address from args
--    let (Just address) = M.lookup "address" argMap <|> M.lookup "a" argMap <|> Just "0.0.0.0"

--    printLn "Starting server" ()
--    printLn "===============" ()
--    printLn "Port:    {}" (Only port)
--    printLn "Address: {}" (Only address)

--    withSocketsDo $ WS.runServer address port $ application state brain
