{-# LANGUAGE DeriveGeneric #-}

module Tools.Reminders (

    ReminderInterval(..),
    Reminder(..),
    ReminderOpts(..),
    Reminders,

    mkDefaultReminderOpts,
    loadReminders,

    addReminder,
    getReminders,
    removeReminder,
    onReminder

) where

-- for auto JSON handling:
import           GHC.Generics

import           Tools.Persist
import           Control.Concurrent
import           Control.Applicative
import           Data.Foldable
import           Data.Time
import qualified Data.Text                  as T
import qualified Data.List                  as L
import           Data.Aeson                 (FromJSON, ToJSON, encode, decode)
import           Control.Monad              (forever, void)
import           Control.Monad.Trans        (MonadIO, liftIO)
import qualified Data.Map                   as M

type ReminderPerson = T.Text

data ReminderInterval
    = Once
    | Daily
    | Weekly
    | Monthly
    | Yearly
    deriving (Ord, Eq, Show, Generic)

instance ToJSON ReminderInterval
instance FromJSON ReminderInterval

data Reminder rem = Reminder
    { reminderText :: rem
    , reminderInterval :: ReminderInterval
    , reminderTimes :: [UTCTime]
    } deriving (Show, Eq, Generic)

instance ToJSON rem => ToJSON (Reminder rem)
instance FromJSON rem => FromJSON (Reminder rem)

instance Eq rem => Ord (Reminder rem) where
    compare Reminder{ reminderTimes = (t1:_) } Reminder{ reminderTimes = (t2:_) } = compare t1 t2

data ReminderOpts = ReminderOpts
    { roFilename :: Maybe String -- persist, and if so, what file?
    }

mkDefaultReminderOpts :: String -> ReminderOpts
mkDefaultReminderOpts name = ReminderOpts
    { roFilename = Just name
    }

--
-- This is how we obtain a reminders map to work with. it handles
-- the persistance and handles firing and removing reminders as
-- they occur
--
loadReminders :: (Eq rem, ToJSON rem, FromJSON rem) => MonadIO m => ReminderOpts -> m (Reminders rem)
loadReminders ReminderOpts{..} = liftIO $ do

    -- create reminders:
    rs@Reminders{..} <- Reminders
                 <$> newMVar M.empty
                 <*> newMVar []

    -- persist them:
    case roFilename of
        Nothing -> return ()
        Just n -> void $ persistMVar (mkDefaultPersistOpts n) reminders

    -- kick off loop to fire them off as necessary:
    forkIO $ forever $ manager rs

    return rs

  where
    -- spins up threads for any reminders approaching
    -- being done (or done already!) which sleep the remainder
    -- of the time and then call the subscribed things
    manager :: Reminders rem -> IO ()
    manager Reminders{..} = do

        callbacks <- readMVar subscribed
        tNow <- getCurrentTime

        modifyMVar_ reminders $ \rmap -> do
            let (msgts, newmap) = updateReminderMap tNow rmap
            fireCallbacks callbacks msgts
            return newmap

        threadDelay 1000000
        return ()

    -- update reminder map
    updateReminderMap :: UTCTime ->
                         M.Map ReminderPerson [Reminder rem] ->
                         ([(ReminderPerson, rem, NominalDiffTime)], M.Map ReminderPerson [Reminder rem])
    updateReminderMap tNow rmap = M.foldlWithKey' fn ([], M.empty) rmap
      where
        fn (outinfo,rmap) name rs =
          let (newoutinfo,newrs) = updateReminders tNow name rs
          in (newoutinfo, M.insert name newrs rmap)

    -- update list of reminders, accumulating (name,text,time) along the way
    updateReminders :: UTCTime ->
                       ReminderPerson ->
                       [Reminder rem] ->
                       ([(ReminderPerson, rem, NominalDiffTime)], [Reminder rem])
    updateReminders tNow name rs = foldr fn ([],[]) rs
      where
        fn r (outinfo,outrs) = (newoutinfo, newoutrs)
          where
            (mT, mR) = updateReminder tNow r
            newoutinfo = case mT of
                Nothing -> outinfo
                Just t -> (name, reminderText r, t):outinfo
            newoutrs = case mR of
                Nothing -> outrs
                Just rem -> rem:outrs

    -- remove closest time if necessaary
    updateReminder :: UTCTime -> Reminder rem -> (Maybe NominalDiffTime, Maybe (Reminder rem))
    updateReminder now r@Reminder{ reminderTimes = (t:ts) } =
        let dt = diffUTCTime t now in
        if dt < 5
            then (Just dt, if ts == [] then Nothing else Just r{ reminderTimes = ts })
            else (Nothing, Just r)

    -- create an IO action which fires all callbacks:
    fireCallbacks :: [(Int,ReminderPerson -> rem -> IO ())] -> [(ReminderPerson, rem, NominalDiffTime)] -> IO ()
    fireCallbacks cbs ms = sequence_ $ foldl' resolvecbs [] ms
        where
          resolvecbs as (name,txt,dt) =
              let a = forkIO $ do
                      threadDelay $ floor $ dt * 1000000
                      sequence_ $ fmap (\(_,fn) -> fn name txt) cbs
              in a:as



--
-- Add a reminder to the list with a given time, interval, message and
-- person to remind. sorted by next reminder time, nearer = first
--
addReminder :: (Eq rem, MonadIO m) =>
               Reminders rem       -> -- opaque reminders object (rem is reminder data)
               ReminderPerson      -> -- person to remind
               rem                 -> -- reminder text (or other data)
               ReminderInterval    -> -- how often to remind
               UTCTime             -> -- when to remind
               m ()
addReminder Reminders{..} name txt i time = liftIO $ modifyMVar_ reminders $ \rmap ->
    let rs = M.findWithDefault [] name rmap
        newrem = Reminder
            { reminderText = txt
            , reminderInterval = i
            , reminderTimes = addTimes i time
            }
        newmap = M.insert name (L.sort (newrem:rs)) rmap
    in return newmap
  where
    addTimes Once    t = [t]
    addTimes Daily   t = let times t' = t' : (times $ plusDays   1  t') in times t
    addTimes Weekly  t = let times t' = t' : (times $ plusDays   7  t') in times t
    addTimes Monthly t = let times t' = t' : (times $ plusMonths 1  t') in times t
    addTimes Yearly  t = let times t' = t' : (times $ plusMonths 12 t') in times t
    plusDays   n t = UTCTime (addDays n (utctDay t))                (utctDayTime t)
    plusMonths n t = UTCTime (addGregorianMonthsClip n (utctDay t)) (utctDayTime t)

--
-- Get reminders for someone as a list. empty if no reminders exist. sorted
-- by time, nearest first.
--
getReminders :: MonadIO m => Reminders rem -> ReminderPerson -> m [Reminder rem]
getReminders Reminders{..} name = liftIO $ M.findWithDefault [] name <$> readMVar reminders

--
-- Remove a reminder for someone based on its position in the list.
--
removeReminder :: MonadIO m => Reminders rem -> ReminderPerson -> Integer -> m Bool
removeReminder Reminders{..} name n = liftIO $ modifyMVar reminders $ \rmap ->
    let rs = M.findWithDefault [] name rmap
        (newrs,bRemoved) = foldr (\(n',r) (arr,b) -> if n' /= n then (r:arr,b) else (arr,True)) ([],False) (L.zip [1..] rs)
        newmap = M.insert name newrs rmap
    in return (newmap,bRemoved)

--
-- Subscribe to being handed reminders when they occur
--
onReminder :: MonadIO m => Reminders rem -> (ReminderPerson -> rem -> IO ()) -> m (IO ())
onReminder Reminders{..} fn = liftIO $ modifyMVar subscribed $ \cs ->
    let newId = foldl' (\newId (id,_) -> if id >= newId then id+1 else newId) 0 cs
        offFn = modifyMVar_ subscribed (return . filter (\(id,_) -> id /= newId))
    in return (((newId,fn):cs),offFn)



-- =======================================
-- | Don't export constructors for this. |
-- =======================================
--
-- Provide funcs to create/work with it above.
--

data Reminders rem = Reminders
    { reminders :: MVar (M.Map ReminderPerson [Reminder rem]) -- map of name to reminder list
    , subscribed :: MVar [(Int, ReminderPerson -> rem -> IO ())]
    }
