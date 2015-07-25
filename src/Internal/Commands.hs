module Internal.Commands (

    -- locked down route state we'll use:
    RouteState(..),
    RouteStateIO,
    doWithRouteState,

    -- commands to work in route bits:
    exit,
    respond,
    getMessage,
    getName,
    sleepMs

) where

import Internal.WebSocket (SocketReplyFn)
import Internal.Routing

import           Control.Monad              (mzero, guard)
import           Control.Monad.Trans        (lift)
import           Control.Monad.State        (liftIO, void)
import           Control.Concurrent         (threadDelay)
import qualified Control.Monad.Trans.Maybe  as M
import qualified Control.Monad.State        as S
import qualified Data.Text                  as T
import           Data.Aeson                 (ToJSON)

--
-- lock down the route output to this, so that we
-- know what commands we'll be able to run in that
-- context
--
data RouteState = RouteState
    { rsMessage :: T.Text
    , rsName    :: T.Text
    , rsReplyFn :: SocketReplyFn
    }

type RouteStateIO = S.StateT RouteState (M.MaybeT IO)

doWithRouteState :: RouteStateIO a -> RouteState -> IO ()
doWithRouteState m rs = void $ M.runMaybeT (S.execStateT m rs)

-- stop computation:
exit :: RouteStateIO ()
exit = lift mzero

-- respond to the socket:
respond :: ToJSON out => out -> RouteStateIO ()
respond out = do
    state <- S.get
    liftIO $ rsReplyFn state out

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