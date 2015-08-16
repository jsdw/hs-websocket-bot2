import Internal.Routing
import Internal.WebSocket
import Internal.Args
import Internal.Types

import Tools.Persist
import Tools.Reminders
import Tools.Text

import Commands
import Parsers

import           System.Environment  (getArgs)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Monoid         ((<>))
import           Data.Default
import           Data.Aeson
import qualified Data.Map            as M
import           Text.Read           (readMaybe)
import           Control.Monad
import           Control.Monad.Trans
import qualified Control.Monad.State as S
import           Control.Applicative ((<$>),(<*>),(<|>))
import qualified Control.Exception   as E
import           Data.Time
import           Data.Foldable
import           Numeric             (showHex)

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
      ( pBotName <..> pS "remind me" <..> var (pUntil (pS " in")) <..> var pRelativeTime <+> pRest )
      $ \(reminder,_) relativeTime -> do

        time <- liftIO $ getCurrentTime
        let futureTime = relativeTime `addRelativeToUTC` time

        name      <- askName
        room      <- askRoom
        reminders <- askReminders

        let resp = def
              { resColour = Just Red
              , resRoom = room
              , resMessage = swapFirstAndSecondPerson reminder
              }

        respond $ name <> " reminder set."
        addReminder reminders name resp Once futureTime

    addRoute
      ( pBotName <..> pS "show reminders" <+> pRest )
      $ do

        name        <- askName
        reminders   <- askReminders
        myReminders <- getReminders reminders name
        tz          <- liftIO $ getCurrentTimeZone

        let formattedTime t = T.pack $ formatTime defaultTimeLocale "%H:%M %d/%m/%Y" (utcToLocalTime tz t)
            reminderStr = foldl' foldfn "/quote " (zip [1..] myReminders)
            foldfn txt (i, Reminder{ reminderText = MessageResponse{..}, reminderTimes = (t:_) }) =
                txt <> (T.pack (show i)) <> ". remember " <> resMessage
                    <> " (next is " <> formattedTime t <> ")\r\n"

        if length myReminders == 0
            then respond $ name <> " you have no reminders"
            else respond reminderStr

    addRoute
      ( pBotName <..> pS "remove reminder" <..> var pDecimal )
      $ \n -> do

        name        <- askName
        reminders   <- askReminders

        let toHex n = T.pack $ "0x" ++ showHex (n*24000) ""

        bRemoved <- removeReminder reminders name n
        respond $ if bRemoved
            then name <> " reminder " <> (T.pack $ show n) <> " removed."
            else name <> " SEGMENTATION FAULT. Memory core location "<> toHex n <>" erased."

    -- a reminders reminder
    addRoute
      ( pBotName <..> pS "remind" <+> pRest )
      $ respond "Want a reminder? remind me REMINDER in NUMBER UNIT"

    -- say something!
    addRoute
        ( pBotName <..> var (pBool $ pS "slowly") <..> pS "say" <..> var (pMaybe $ pS "(" *> pWord <* pS ")" ) <..> var pRest )
        $ \beSlow mRoom message -> do
            name <- askName
            guard $ name == "@james"
            respondWith def{ roSlow = beSlow, roRoom = mRoom } message

    -- greetings!
    addRoute
      ( var pGreetings <..> pUntil pBotName <+> pRest )
      $ \greeting -> do

        name <- askName

        let res = case T.toLower greeting of
              "wassup"   -> name <> " wa fizzle my dizzle?"
              "good day" -> "A fine day to you kind " <> name
              _          -> "Hello there " <> name

        respondWith def{roSlow = True} res

    --
    -- Random responses to messages containing BOTNAME:
    --

    addMaybeRoute (1/100) (pUntil pBotName <..> pRest)
      $ do
        name <- askName
        respondWith def{roSlow = True} $ name <> " you have something on your face."


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

    -- load in our reminders system and attach to write.
    reminders <- loadReminders (mkDefaultReminderOpts "jamesbot-reminders.json")

    -- each time someone connects we run this:
    startServer socketSettings (callback reminders)


callback :: Reminders MessageResponse -> IO MessageReceived -> (MessageResponse -> IO ()) -> IO ()
callback reminders read write = do
    offReminders <- handleReminders reminders write
    loop `E.finally` putStrLn "running cleanup" >> offReminders
  where
    -- run any received messages against the
    -- routes, performing the associated action
    -- if a matching one is found
    loop = forever $ do

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
handleReminders :: Reminders MessageResponse -> (MessageResponse -> IO ()) -> IO (IO ())
handleReminders reminders write = onReminder reminders $
    \name r@MessageResponse{..} ->
        write r{ resMessage = "BZZT! " <> name <> " remember " <> resMessage }
