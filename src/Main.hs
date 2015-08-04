import Internal.Routing
import Internal.WebSocket
import Internal.Args
import Internal.Types

import Tools.Persist
import Tools.Reminders

import Commands
import Parsers

import           System.Environment  (getArgs)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Data.Default
import           Data.Aeson
import qualified Data.Map            as M
import           Text.Read           (readMaybe)
import           Control.Monad
import           Control.Monad.Trans
import qualified Control.Monad.State as S
import           Control.Applicative ((<$>),(<*>),(<|>))
import           Data.Time
import           Data.Foldable

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
      ( pBotName <..> pS "remind me" <..> var (pUntil (pS " in")) <..> var pFromNow <+> pRest )
      $ \(reminder,_) msFromNow -> do

        time <- liftIO $ getCurrentTime
        let futureTime = (fromIntegral msFromNow/1000) `addUTCTime` time

        name      <- askName
        room      <- askRoom
        reminders <- askReminders

        let resp = def
              { resColour = Just Red
              , resRoom = room
              , resMessage = reminder
              }

        respond $ name <> " reminder set."
        addReminder reminders name resp Once futureTime


    addRoute
      ( pBotName <..> pS "show reminders" <+> pRest )
      $ do

        name        <- askName
        reminders   <- askReminders
        myReminders <- getReminders reminders name

        let reminderStr = foldl' foldfn "" (zip [1..] myReminders)
            foldfn txt (i, Reminder{ reminderText = MessageResponse{..} }) =
                txt <> "(" <> (T.pack (show i)) <> ") remember " <> resMessage <> "\r\n"

        respond reminderStr

    addRoute
      ( pBotName <..> pS "remove reminder" <..> var pDecimal )
      $ \n -> do

        name        <- askName
        reminders   <- askReminders

        removeReminder reminders name n
        respond $ name <> " reminder " <> (T.pack $ show n) <> " removed."

    -- a reminders reminder
    addRoute
      ( pBotName <..> pS "remind" <+> pRest )
      $ respond "Want a reminder? remind me REMINDER in NUMBER UNIT"


    -- greetings!
    addRoute
      ( var pGreetings <..> pUntil pBotName <+> pRest )
      $ \greeting -> do

        name <- askName

        let res = case T.toLower greeting of
              "wassup"   -> name <> " wa fizzle my dizzle?"
              "good day" -> "A fine day to you kind " <> name
              _          -> "Hello there " <> name

        respondSlowly res

    --
    -- Random responses to messages containing BOTNAME:
    --

    addMaybeRoute (1/100) (pUntil pBotName <..> pRest)
      $ do
        name <- askName
        respondSlowly $ name <> " that accent isn't even slightly convincing."


--
-- Kick off a socket server using our wrapper. pass
-- in the above callback which handles what to do
-- when a valid message is received
--
main :: IO ()
main = do

    args <- fmap parseKeys getArgs

    -- parse port number and address from args:
    let (Just address) = M.lookup "address" args <|> M.lookup "a" args <|> Just "0.0.0.0"
        (Just port) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 9090
           where maybeP = M.lookup "port" args <|> M.lookup "p" args

    -- begin socket server with these settings:
    let socketSettings = def
          { sAddress = T.pack address
          , sPort = port
          }

    -- open our websocket connection up
    (read,write) <- startServer socketSettings

    -- load in our reminders system and attach to write.
    reminders <- loadReminders (mkDefaultReminderOpts "jamesbot-reminders.json")
    handleReminders reminders write

    -- run any received messages against the
    -- routes, performing the associated action
    -- if a matching one is found
    forever $ do

        MessageReceived{..} <- read
        routesInput <- mkRoutesInput rMessage

        let rs = RouteState
               { rsMessage   = rMessage
               , rsName      = rName
               , rsReplyFn   = write
               , rsRoom      = rRoom
               , rsReminders = reminders
               }

        case runRoutes routes routesInput of
            Just m -> doWithRouteState m rs
            Nothing -> return ()


-- take some reminders and a function to write them
-- out and hook it together.
handleReminders :: Reminders MessageResponse -> (MessageResponse -> IO ()) -> IO ()
handleReminders reminders write = onReminder reminders $
    \name MessageResponse{..} -> write def
        { resMessage = name <> " remember " <> resMessage
        , resRoom = resRoom
        }
