module Tools.Persist (
    PersistOpts(..),
    mkDefaultPersistOpts,
    persistMVar,
    stopPersistance
) where

import           Control.Concurrent
import           Data.Aeson                 (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified System.IO                  as IO
import           Control.Monad              (forever, void)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Control.Exception
import           System.IO.Error            (isDoesNotExistError)

data PersistOpts = PersistOpts
    { poOnStartup  :: Bool       -- load in persisted file contents on start?
    , poOnInterval :: Maybe Int  -- seconds
    , poFilename   :: FilePath
    }

mkDefaultPersistOpts :: FilePath -> PersistOpts
mkDefaultPersistOpts name = PersistOpts
    { poOnStartup  = True
    , poOnInterval = Just 1
    , poFilename   = name
    }

persistMVar :: (MonadIO m, Eq v, ToJSON v, FromJSON v) => PersistOpts -> MVar v -> m Persist
persistMVar PersistOpts{..} mv = liftIO $ do

    -- initialise persist object to control this later.
    isStopped <- newMVar False

    let controls = Persist
          { pIsStopped = isStopped
          }

    -- load in file contents initially if asked to (and possible to):
    if poOnStartup
        then tryInitialRead poFilename mv `catch` doesNotExist
        else return ()

    -- write back to file if asked to on interval:
    case poOnInterval of
        Just i  -> void $ forkIO $ writeLoop controls poFilename (i * 1000000) mv
        Nothing -> return ()

    return controls

  where
    doesNotExist e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
    tryInitialRead name mv = do
        file <- BL.readFile name
        case decode file of
            Just c  -> void $ swapMVar mv c
            Nothing -> return ()

writeLoop :: (MonadIO m, ToJSON v, Eq v) => Persist -> FilePath -> Int -> MVar v -> m ()
writeLoop p name i mv = liftIO $ do
    curVal <- readMVar mv
    loop p mv curVal
  where
    loop p@Persist{..} mv curVal = do
      newVal <- readMVar mv
      if newVal /= curVal
          then BL.writeFile name (encode newVal)
          else return ()
      threadDelay i
      bStopped <- readMVar pIsStopped
      if not bStopped
          then loop p mv newVal
          else return ()

--
-- A persisted object. use this to alter/stop persistance.
-- don't expose this externally; no pattern matching on it.
--
data Persist = Persist
    { pIsStopped :: MVar Bool
    }

--
-- functions to operate on our persist object. expose these.
--
stopPersistance :: MonadIO m => Persist -> m ()
stopPersistance Persist{..} = liftIO $ void $ swapMVar pIsStopped True