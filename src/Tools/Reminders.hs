{-# LANGUAGE DeriveGeneric #-}

module Tools.Reminders (

    ReminderInterval(..),
    Reminder(..),
    ReminderOpts(..),

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

data Reminder = Reminder
    { reminderText :: T.Text
    , reminderInterval :: ReminderInterval
    , reminderTimes :: [UTCTime]
    } deriving (Show, Eq, Generic)

instance ToJSON Reminder
instance FromJSON Reminder

instance Ord Reminder where
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
loadReminders :: MonadIO m => ReminderOpts -> m Reminders
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
    manager :: Reminders -> IO ()
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
                         M.Map ReminderPerson [Reminder] ->
                         ([(ReminderPerson, T.Text, NominalDiffTime)], M.Map ReminderPerson [Reminder])
    updateReminderMap tNow rmap = M.foldlWithKey' fn ([], M.empty) rmap
      where
        fn (outinfo,rmap) name rs =
          let (newoutinfo,newrs) = updateReminders tNow name rs
          in (newoutinfo, M.insert name newrs rmap)

    -- update list of reminders, accumulating (name,text,time) along the way
    updateReminders :: UTCTime ->
                       ReminderPerson ->
                       [Reminder] ->
                       ([(ReminderPerson, T.Text, NominalDiffTime)], [Reminder])
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
    updateReminder :: UTCTime -> Reminder -> (Maybe NominalDiffTime, Maybe Reminder)
    updateReminder now r@Reminder{ reminderTimes = (t:ts) } =
        let dt = diffUTCTime t now in
        if dt < 5
            then (Just dt, if ts == [] then Nothing else Just r{ reminderTimes = ts })
            else (Nothing, Just r)

    -- create an IO action which fires all callbacks:
    fireCallbacks :: [ReminderPerson -> T.Text -> IO ()] -> [(ReminderPerson, T.Text, NominalDiffTime)] -> IO ()
    fireCallbacks cbs ms = sequence_ $ foldl' resolvecbs [] ms
        where
          resolvecbs as (name,txt,dt) =
              let a = forkIO $ do
                      threadDelay $ floor $ dt * 1000000
                      sequence_ $ fmap (\fn -> fn name txt) cbs
              in a:as



--
-- Add a reminder to the list with a given time, interval, message and
-- person to remind. sorted by next reminder time, nearer = first
--
addReminder :: MonadIO m        =>
               Reminders        -> -- opaque reminders object
               ReminderPerson   -> -- person to remind
               T.Text           -> -- reminder message
               ReminderInterval -> -- how often to remind
               UTCTime          -> -- when to remind
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
getReminders :: MonadIO m => Reminders -> ReminderPerson -> m [Reminder]
getReminders Reminders{..} name = liftIO $ M.findWithDefault [] name <$> readMVar reminders

--
-- Remove a reminder for someone based on its position in the list.
--
removeReminder :: MonadIO m => Reminders -> ReminderPerson -> Integer -> m ()
removeReminder Reminders{..} name n = liftIO $ modifyMVar_ reminders $ \rmap ->
    let rs = M.findWithDefault [] name rmap
        newrs = foldr (\(n',r) b -> if n' /= n then r:b else b) [] (L.zip [1..] rs)
        newmap = M.insert name newrs rmap
    in return newmap

--
-- Subscribe to being handed reminders when they occur
--
onReminder :: MonadIO m => Reminders -> (ReminderPerson -> T.Text -> IO ()) -> m ()
onReminder Reminders{..} fn = liftIO $ modifyMVar_ subscribed $ \cs -> return (fn:cs)



-- =======================
-- | Don't export this. |
-- =======================
--
-- Provide funcs to create/work with it above.
--

data Reminders = Reminders
    { reminders :: MVar (M.Map ReminderPerson [Reminder]) -- map of name to reminder list
    , subscribed :: MVar [ ReminderPerson -> T.Text -> IO () ]
    }
