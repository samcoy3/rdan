module Game.Vote where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M

import Discord
import Discord.Types

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime

data EndCondition a =
  AllVoted
  | AllVotedOrTimeUp a
  | TimeUp a
  deriving (Eq, Show)

type VoteId = Int
type Votes = M.Map VoteId Vote
data Vote = Vote { messages :: M.Map MessageId UserId
                 , responses :: M.Map UserId [Text]
                 , purpose :: Text
                 , announceChannel :: ChannelId
                 , endCondition :: EndCondition UTCTime}
            deriving (Show)

describe :: Int -> Vote -> Text
describe playerCount vote =
  "On the subject of **" <>
  purpose vote <>
  "**. So far, out of the " <>
  (T.pack . show $ playerCount) <>
  " players, " <>
  (T.pack . show . M.size . M.filter (/= []) $ responses vote) <>
  " have voted. " <>
  (endConditionDescription . endCondition) vote

endConditionDescription :: EndCondition UTCTime -> Text
endConditionDescription AllVoted =
  "The vote will end when everyone has voted; there is no time limit."
endConditionDescription (AllVotedOrTimeUp t) =
  "The vote will end when everyone has voted, or at " <>
  printTime t <>
  ", whichever comes first."
endConditionDescription (TimeUp t) =
  "The vote will end at " <>
  printTime t <>
  " and not before."

printTime :: UTCTime -> Text
printTime = T.pack .
  (formatTime defaultTimeLocale "%b %d at %R") .
  (utcToLocalTime bst)

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
