import Internal.Routing
import Internal.WebSocket
import Internal.Args
import Internal.Types

import Commands
import Parsers

import           System.Environment  (getArgs)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Data.Default
import           Data.Aeson
import qualified Data.Map            as M
import           Text.Read           (readMaybe)
import           Control.Monad       (mzero,guard)
import qualified Control.Monad.State as S
import           Control.Applicative ((<$>),(<*>),(<|>))

--
-- For each message that is received, attempt to
-- parse it against each of these routes from top
-- to bottom, and run the corresponding thing if
-- parsing is successful.
--
routes :: Routes (RouteStateIO ())
routes = do

    -- BOTNAME remind me REMINDER in TIME_FROM_NOW
    addRoute 
      ( pBotName <+> pS " remind me " <+> var (pUntil (pS " in ")) <+> var pFromNow <+> pRest )
      $ \(reminder,_) msFromNow -> do

        --calculate date in msFromNow
        time <- getTime
        let futureTime = time `addMs` msFromNow

        name <- getName
        respond $ "I will remind you " <> reminder <> " at " <> formatTime "%c" futureTime
        sleepMs msFromNow
        respondWithColour Red $ name <> " remember " <> reminder

--
-- Run a message received through the routes.
-- matching route will return an IO () which
-- we then run to perform some action.
--
callback :: MessageReceived -> SocketReplyFn -> IO ()
callback MessageReceived{..} replyFn = do

    let rs = RouteState
           { rsMessage = rMessage
           , rsName    = rName
           , rsReplyFn = replyFn
           , rsRoom    = rRoom
           }

    case runRoutes routes rMessage of
        Just m -> doWithRouteState m rs
        Nothing -> return () 

--
-- Kick off a socket server using our wrapper. pass
-- in the above callback which handles what to do
-- when a valid message is received
--
main :: IO ()
main = do 

    args <- fmap parseKeys getArgs

    --parse port number and address from args:
    let (Just address) = M.lookup "address" args <|> M.lookup "a" args <|> Just "0.0.0.0"
        (Just port) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 9090
           where maybeP = M.lookup "port" args <|> M.lookup "p" args

    --begin socket server with these settings:
    let socketSettings = (def :: ServerSettings MessageReceived)
          { sAddress = T.pack address
          , sPort = port
          , sCallback = callback
          }

    startServer socketSettings 


