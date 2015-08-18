module Internal.Channel (
    makeChan
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
        writer a = isEmptyMVar mv >>= \b ->
            if b then putMVar mv a
                 else modifyMVar_ mv (\_ -> return a)
    return (reader, writer)
