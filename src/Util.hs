module Util where

import Config

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

import Control.Monad.Reader

type BotM = ReaderT Config DiscordHandler

getConfig :: BotM Config
getConfig = ask

-- A utility for sending messages. Currently we ignore whether it was successful.
sendMessage :: ChannelId -> Text -> BotM ()
sendMessage channel m = do
  lift . void . restCall $ R.CreateMessage channel m

readTVarDisc :: TVar a -> BotM a
readTVarDisc = liftIO . readTVarIO

modifyTVarDisc :: TVar a -> (a -> a)-> BotM ()
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
