module Internal.Channel (
    makeChan,
    makeExpiringChan
) where

import Control.Concurrent
import Data.Functor            (void)
import Control.Monad.Trans     (MonadIO(..))

--
-- make a channel that doesnt block on write but does on read.
-- if the reader is severed some messages are discarded.
--
makeChan :: MonadIO m => m (IO a, a -> IO ())
makeChan = liftIO $ do
    mv <- newEmptyMVar
    let reader = takeMVar mv
        writer a = isEmptyMVar mv >>= \b -> if b then putMVar mv a else modifyMVar_ mv (\_ -> return a)
    return (reader, writer)

makeExpiringChan :: MonadIO m => Int -> m (IO a, a -> IO ())
makeExpiringChan timeout = liftIO $ do

    (reader, writer) <- makeChan
    tid <- newEmptyMVar

    let killTimer = do
            bEmpty <- isEmptyMVar tid
            if not bEmpty
                then takeMVar tid >>= killThread
                else return ()

        doTimer = do
            killTimer
            ntid <- forkIO $ void $ threadDelay timeout >> reader
            putMVar tid ntid

        writer' a = writer a >> doTimer
        reader' = killTimer >> reader

    return (reader', writer')