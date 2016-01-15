import qualified Data.Text as Text
import qualified Data.Map  as Map
import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<|>))
import Control.Concurrent  (MVar,putMVar,newEmptyMVar,takeMVar,forkIO)
import System.Environment  (getArgs)
import Data.Monoid         ((<>))
import Text.Read           (readMaybe)
import Data.Default        (def)
import Data.Aeson          (Value)

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Internal.WebSocket
import Internal.Args
import Internal.Types

main :: IO ()
main = do

    args <- fmap parseKeys getArgs

    -- parse port number and address from args:
    let (Just address) = Map.lookup "address" args <|> Map.lookup "a" args <|> Just "0.0.0.0"
        (Just httpPort) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 8080
           where maybeP = Map.lookup "http" args <|> Map.lookup "h" args
        (Just port) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 9090
           where maybeP = Map.lookup "port" args <|> Map.lookup "p" args

    -- begin socket server with these settings:
    let socketSettings = def
          { sAddress = Text.pack address
          , sPort = port
          , sLogger = \_ -> return ()
          }

    -- print welcome message:
    putStrLn   "==================="
    putStrLn   "|     PostBot     |"
    putStrLn   "==================="
    putStrLn $ "Starting on: " <> address
    putStrLn $ "Http port:   " <> show httpPort
    putStrLn $ "Socket port: " <> show port

    -- share this between http and socket server
    -- to send messages between them.
    msgVar <- newEmptyMVar

    --kick off our http server:
    forkIO $ run httpPort (httpApp msgVar)

    -- each time someone connects we run this:
    startServer socketSettings (callback msgVar)

--
-- This ignores things sent to it from hipchat and just
-- relays messages off as they are posted here.
--
callback :: MVar MessageResponse -> IO Value -> (MessageResponse -> IO ()) -> IO ()
callback msgVar _ write = forever $ takeMVar msgVar >>= write

--
-- Pick up message sposted to me and respond. Mostly boilerplate.
-- the type describes what we expect in and what we hand back.
--
type PostMessage = ReqBody '[JSON] MessageResponse :> Post '[JSON] ()

postMessageApi :: Proxy PostMessage
postMessageApi = Proxy

httpServer :: MVar MessageResponse -> Server PostMessage
httpServer msgVar postBody = liftIO $ putStrLn (show postBody) >> putMVar msgVar postBody

httpApp :: MVar MessageResponse -> Application
httpApp msgVar = serve postMessageApi (httpServer msgVar)


