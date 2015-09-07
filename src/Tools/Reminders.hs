{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Tools.Reminders (

    DT.Interval(..),
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

-- for our Interval type:
import           Parsers.DateTime            as DT

import           Tools.Persist
import           Control.Concurrent
import           Control.Applicative
import           Data.Foldable
import           Data.Time
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Maybe
import qualified Data.Text                   as T
import qualified Data.List                   as L
import           Data.Aeson
import           Control.Monad
import           Control.Monad.Trans         (MonadIO, liftIO)
import qualified Data.Map                    as M

type ReminderPerson = T.Text
type ReminderMap rem = M.Map ReminderPerson [Reminder rem]

data Reminder rem = Reminder
    { reminderText :: rem
    , reminderInterval :: DT.Interval
    , reminderTime :: UTCTime
    } deriving (Show, Eq, Generic)

deriving instance Generic DT.Interval
deriving instance Generic DT.Weekday

instance ToJSON rem => ToJSON (Reminder rem)
instance FromJSON rem => FromJSON (Reminder rem)

instance ToJSON DT.Weekday
instance FromJSON DT.Weekday

instance ToJSON DT.Interval
instance FromJSON DT.Interval

instance Eq rem => Ord (Reminder rem) where
    compare Reminder{ reminderTime = t1 } Reminder{ reminderTime = t2 } = compare t1 t2

data ReminderOpts = ReminderOpts
    { roFilename :: Maybe String -- persist, and if so, what file?
    }

mkDefaultReminderOpts :: String -> ReminderOpts
mkDefaultReminderOpts name = ReminderOpts
    { roFilename = Just name
    }

--
-- Add a reminder to the list with a given time, interval, message and
-- person to remind. sorted by next reminder time, nearer = first
--
addReminder :: (Eq rem, MonadIO m) =>
               Reminders rem       -> -- opaque reminders object (rem is reminder data)
               ReminderPerson      -> -- person to remind
               rem                 -> -- reminder text (or other data)
               DT.Interval         -> -- how often to remind
               UTCTime             -> -- when to remind
               m ()
addReminder Reminders{..} name txt i time = liftIO $ modifyMVar_ reminders $ \rmap ->
    let rs = M.findWithDefault [] name rmap
        newrem = Reminder
            { reminderText = txt
            , reminderInterval = i
            , reminderTime = time
            }
        newmap = M.insert name (newrem:rs) rmap
    in return newmap

--
-- Get reminders for someone as a list. empty if no reminders exist. sorted
-- by time, nearest first.
--
getReminders :: (Eq rem, MonadIO m) => Reminders rem -> ReminderPerson -> m [Reminder rem]
getReminders Reminders{..} name = liftIO $ do
    rmap <- readMVar reminders
    return $ L.sort $ M.findWithDefault [] name rmap

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


--
-- This is how we obtain a reminders map to work with. it handles
-- the persistance and handles firing and removing reminders as
-- they occur
--
loadReminders :: forall m rem. (Eq rem, ToJSON rem, FromJSON rem) => MonadIO m => ReminderOpts -> m (Reminders rem)
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
            let (msgts, newmap) = updateReminders tNow rmap
            fireCallbacks callbacks msgts
            return newmap
        threadDelay 1000000
        return ()

    -- create an IO action which fires all callbacks:
    fireCallbacks :: [(Int,ReminderPerson -> rem -> IO ())] -> [(ReminderPerson, rem, NominalDiffTime)] -> IO ()
    fireCallbacks cbs ms = mapM_ resolvecbs ms
      where
        resolvecbs (name,txt,dt) = forkIO $ do
            threadDelay $ floor $ dt * 1000000
            mapM_ (\(_,fn) -> fn name txt) cbs

    -- update the reminder map
    updateReminders :: UTCTime -> ReminderMap rem -> ([(ReminderPerson, rem, NominalDiffTime)], ReminderMap rem)
    updateReminders time rmap = M.foldlWithKey' fn ([], M.empty) rmap
      where
        fn (outinfo,rmap) name rs =
            let (nextoutinfo,newrs) = getUpcomingReminders time rs
                addName = fmap (\(dt,rem) -> (name,dt,rem))
            in (outinfo ++ addName nextoutinfo, M.insert name newrs rmap)

    -- given some list of reminders, get nearby data and filtered new rem list
    getUpcomingReminders :: UTCTime -> [Reminder rem] -> ([(rem,NominalDiffTime)],[Reminder rem])
    getUpcomingReminders time rs = (msgs, catMaybes newrs)
      where
        (msgs,newrs) = foldr fn ([],[]) rs
        fn r@Reminder{..} (outinfo,outrs) = let dt = secondsToReminder time r in if dt < 5
            then ((reminderText,dt):outinfo, getNextReminder r:outrs)
            else (outinfo, Just r:outrs)

    -- how long until a reminder given time t?
    secondsToReminder :: UTCTime -> Reminder rem -> NominalDiffTime
    secondsToReminder t Reminder{..} = diffUTCTime reminderTime t

    -- generate next reminder (may be Nothing if interval is once)
    getNextReminder :: Reminder rem -> Maybe (Reminder rem)
    getNextReminder r@Reminder{..} = case nextTime reminderInterval reminderTime of
        Nothing -> Nothing
        Just nt -> Just r{ reminderTime = nt }

    -- Given a time and some interval, give back the next time
    nextTime :: DT.Interval -> UTCTime -> Maybe UTCTime
    nextTime i t = case i of
        Once    -> Nothing
        Days [] -> Nothing
        Days ds -> Just $ nextDay    ds t
        Daily   -> Just $ plusDays   1  t
        Weekly  -> Just $ plusDays   7  t
        Monthly -> Just $ plusMonths 1  t
        Yearly  -> Just $ plusMonths 12 t
      where
        plusDays   n t = UTCTime (addDays                n (utctDay t)) (utctDayTime t)
        plusMonths n t = UTCTime (addGregorianMonthsClip n (utctDay t)) (utctDayTime t)
        nextDay   ds t = plusDays (L.minimum distances) t
          where
            distances = fmap (fromIntegral . distance weekDay . fromEnum) ds
            distance from to
                | from > to  = distance (from - 7) to
                | from == to = 7
                | otherwise  = to - from
            weekDay = let (_,_,wd) = toWeekDate (utctDay t) in wd

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
