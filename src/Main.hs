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
        name <- getName
        time <- getTime

        let futureTime = time `addMs` msFromNow
        respond $ "I will remind you " <> reminder <> " at " <> formatTime "%c" futureTime

        --ignore times > 1 year
        guard (msFromNow < (1000 * 60 * 60 * 24 * 365))

        sleepMs msFromNow
        respondWithColour Red $ name <> " remember " <> reminder

    -- a reminders reminder
    addRoute
      ( pBotName <+> pS " remind" <+> pRest )
      $ respond "Want a reminder? remind me REMINDER in NUMBER UNIT"

    -- greetings!
    addRoute
      ( var pGreetings <+> pUntil pBotName <+> pRest )
      $ \greeting -> do

        name <- getName

        let res = case T.toLower greeting of
              "wassup"   -> name <> " wa fizzle my dizzle?"
              "good day" -> "A fine day to you kind " <> name
              _          -> "Hello there " <> name

        respondSlowly res

    --
    -- Random responses to messages containing BOTNAME:
    --

    addMaybeRoute (1/100) (pUntil pBotName <+> pRest)
      $ do
        name <- getName
        respondSlowly $ name <> " that accent isn't even slightly convincing."

--
-- Run a message received through the routes.
-- matching route will return an IO () which
-- we then run to perform some action.
--
callback :: MessageReceived -> SocketReplyFn -> IO ()
callback MessageReceived{..} replyFn = do

    routesInput <- mkRoutesInput rMessage

    let rs = RouteState
           { rsMessage = rMessage
           , rsName    = rName
           , rsReplyFn = replyFn
           , rsRoom    = rRoom
           }

    case runRoutes routes routesInput of
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


