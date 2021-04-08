module Util where

import Data.Text
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

-- A utility for sending messages. Currently we ignore whether it was successful.
sendMessage :: ChannelId -> Text -> DiscordHandler ()
sendMessage channel m = do
  void . restCall $ R.CreateMessage channel m

readTVarDisc :: TVar a -> DiscordHandler a
readTVarDisc = liftIO . readTVarIO

modifyTVarDisc :: TVar a -> (a -> a)-> DiscordHandler ()
modifyTVarDisc var f = liftIO . atomically $ modifyTVar var f

bst :: TimeZone
bst = TimeZone { timeZoneMinutes = 60
               , timeZoneSummerOnly = True
               , timeZoneName = "BST" }

utcFromHoursMinutesDayOffset :: (Int, Int, Integer) -> IO UTCTime
utcFromHoursMinutesDayOffset (hours, minutes, dayOffset) = do
  currentTime <- getCurrentTime
  if (localTimeOfDay . utcToLocalTime bst) currentTime > TimeOfDay {todHour = hours, todMin = minutes, todSec = 0}
    then return . localTimeToUTC bst $
          LocalTime { localDay = addDays (1 + dayOffset) (utctDay currentTime)
                    , localTimeOfDay = TimeOfDay { todHour = hours
                                                 , todMin = minutes
                                                 , todSec = 0}}
    else return . localTimeToUTC bst $
          LocalTime { localDay = addDays dayOffset $ utctDay currentTime
                    , localTimeOfDay = TimeOfDay { todHour = hours
                                                 , todMin = minutes
                                                 , todSec = 0}}
