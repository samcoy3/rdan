module Util where

import Data.Text

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

-- A utility for sending messages. Currently we ignore whether it was successful.
sendMessage :: DiscordHandle -> ChannelId -> Text -> IO ()
sendMessage disc channel m = do
  _ <- restCall disc $ R.CreateMessage channel m
  return ()

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
