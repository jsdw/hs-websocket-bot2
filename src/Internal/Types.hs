module Internal.Types (

    --message handed to the bot from outside
    MessageReceived(..),
    --message sent from the bot
    MessageResponse(..),
    MessageColour(..),

    RouteState(..),
    RouteStateIO,
    doWithRouteState

) where

import           Tools.Reminders            (Reminders)

import qualified Data.Text                  as T
import           Data.Aeson
import           Data.Default
import           Control.Monad              (mzero, void)
import           Control.Concurrent         (threadDelay)
import qualified Control.Monad.Trans.Maybe  as M
import qualified Control.Monad.State        as S

--
-- this is what we expect incoming messages to look like.
-- they will be auto-parsed into this structure, and if
-- parsing fails we won't try to respond
--
data MessageReceived = MessageReceived
    { rName :: T.Text
    , rMessage :: T.Text
    , rRoom :: Maybe T.Text
    } deriving (Show,Eq)

instance FromJSON MessageReceived where
    parseJSON (Object m) = MessageReceived
                       <$> m .: "name"
                       <*> m .: "message"
                       <*> m .: "room"
    parseJSON _  = mzero

--
-- this is what outgoing messages will look like.
-- our response commands will generate this.
--
data MessageResponse = MessageResponse
    { resMessage :: T.Text
    , resColour :: Maybe MessageColour
    , resRoom :: Maybe T.Text
    } deriving (Show,Eq)

instance Default MessageResponse where
    def = MessageResponse
        { resMessage = ""
        , resColour = Nothing
        , resRoom = Nothing
        }

instance ToJSON MessageResponse where
    toJSON (MessageResponse m c r) = object
        [ "message" .= m
        , "colour"  .= c
        , "room"    .= r
        ]

instance FromJSON MessageResponse where
    parseJSON (Object m) = MessageResponse
                       <$> m .: "message"
                       <*> m .: "colour"
                       <*> m .: "room"
    parseJSON _  = mzero

data MessageColour = Yellow | Green | Red | Purple | Grey | Random
    deriving (Show,Eq)

instance ToJSON MessageColour where
    toJSON Yellow = String "yellow"
    toJSON Green  = String "green"
    toJSON Red    = String "red"
    toJSON Purple = String "purple"
    toJSON Grey   = String "grey"
    toJSON Random = String "random"

instance FromJSON MessageColour where
    parseJSON (String c) = return $ col c
      where
        col "yellow" = Yellow
        col "green"  = Green
        col "red"    = Red
        col "purple" = Purple
        col "grey"   = Grey
        col _        = Random

--
-- lock down the route output to this, so that we
-- know what commands we'll be able to run in that
-- context. Stack a monad transformer.
--
data RouteState = RouteState
    { rsMessage :: T.Text
    , rsName    :: T.Text
    , rsRoom    :: Maybe T.Text
    , rsReplyFn :: (MessageResponse -> IO ())
    , rsReminders :: Reminders MessageResponse
    }

type RouteStateIO = S.StateT RouteState (M.MaybeT IO)

doWithRouteState :: RouteStateIO a -> RouteState -> IO ()
doWithRouteState m rs = void $ M.runMaybeT (S.execStateT m rs)

