module Tools.Persist (
    PersistOpts(..),
    mkDefaultPersistOpts,
    persistMVar
) where

import           Data.Default
import           Control.Concurrent
import           Data.Aeson                 (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified System.IO                  as IO
import           Control.Monad              (forever, void)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Control.Exception
import           System.IO.Error            (isDoesNotExistError)

data PersistOpts = PersistOpts
    { poOnClose    :: Bool
    , poOnStartup  :: Bool       -- load in persisted file contents on start?
    , poOnInterval :: Maybe Int -- seconds
    , poFilename   :: FilePath
    }

mkDefaultPersistOpts :: FilePath -> PersistOpts
mkDefaultPersistOpts name = PersistOpts
    { poOnClose    = True
    , poOnStartup  = True
    , poOnInterval = Just 1
    , poFilename   = name
    }

persistMVar :: (MonadIO m, Eq v, ToJSON v, FromJSON v) => PersistOpts -> MVar v -> m ()
persistMVar PersistOpts{..} mv = liftIO $ do

    -- load in file contents initially if asked to (and possible to):
    let doesNotExist e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

    if poOnStartup
        then tryInitialRead poFilename mv `catch` doesNotExist
        else return ()

    -- write back to file if asked to on interval:
    case poOnInterval of
        Just i  -> void $ forkIO $ writeLoop poFilename (i * 1000000) mv
        Nothing -> return ()

  where
    tryInitialRead name mv = do
        file <- BL.readFile name
        case decode file of
            Just c  -> void $ swapMVar mv c
            Nothing -> return ()

writeLoop :: (MonadIO m, ToJSON v, Eq v) => FilePath -> Int -> MVar v -> m ()
writeLoop name i mv = liftIO $ do
    curVal <- readMVar mv
    loop mv curVal
  where
    loop mv curVal = do
      newVal <- readMVar mv
      if newVal /= curVal
          then BL.writeFile name (encode newVal)
          else return ()
      threadDelay i
      loop mv newVal
