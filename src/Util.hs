module Util where

import Data.Text

import Discord
import Discord.Types
import qualified Discord.Requests as R

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
