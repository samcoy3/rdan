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
import Data.Aeson.Types
import Data.Yaml
import GHC.Generics

data EndCondition a =
  AllVoted
  | AllVotedOrTimeUp a
  | TimeUp a
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (ToJSON a) => ToJSON (EndCondition a)
instance (FromJSON a) => FromJSON (EndCondition a)

type VoteId = Int
type Votes = M.Map VoteId Vote
data Vote = Vote { messages :: M.Map MessageId UserId
                 , responses :: M.Map UserId [Text]
                 , purpose :: Text
                 , announceChannel :: ChannelId
                 , lastChecked :: Maybe UTCTime
                 , endCondition :: EndCondition UTCTime}
            deriving (Show, Generic)

deriving instance ToJSONKey Snowflake
deriving instance FromJSONKey Snowflake
instance ToJSON Vote
instance FromJSON Vote

-- Given a number of players, check if a vote has received that number of votes.
allPlayersHaveVoted :: Int -> Vote -> Bool
allPlayersHaveVoted playerCount vote =
  playerCount == (M.size . M.filter (/= []) $ responses vote)

publicDescription :: Int -> Vote -> Text
publicDescription playerCount vote =
  let voteCountText = if allPlayersHaveVoted playerCount vote
        then ":ballot_box: **All players have voted.** "
        else  (T.pack . show . M.size . M.filter (/= []) $ responses vote) <>
                "/" <>
                (T.pack . show $ playerCount) <>
                " votes. "
  in "On the subject of **" <>
  purpose vote <>
  "**.\n" <>
  voteCountText <>
  (endConditionDescription . endCondition) vote

userDMDescription :: Text -> EndCondition UTCTime -> Text
userDMDescription purpose endCondition =
  "A new vote has begun on the subject of **"
    <> purpose
    <> "**! "
    <> endConditionDescription endCondition
    <> " Please react to this message with a tick or a cross in order to vote:"

endConditionDescription :: EndCondition UTCTime -> Text
endConditionDescription AllVoted =
  "The vote will end when everyone has voted."
endConditionDescription (AllVotedOrTimeUp t) =
  "The vote will end at " <>
  printTime t <>
  ", or when all players have voted."
endConditionDescription (TimeUp t) =
  "The vote will end at " <>
  printTime t <>
  "."

printTime :: UTCTime -> Text
printTime = T.pack .
  (formatTime defaultTimeLocale "%b %d at %R") .
  (utcToLocalTime bst)

bst :: TimeZone
bst = TimeZone { timeZoneMinutes = 60
               , timeZoneSummerOnly = True
               , timeZoneName = "BST" }

getEndTime :: Vote -> Maybe UTCTime
getEndTime v =
  case endCondition v of
    AllVoted -> Nothing
    AllVotedOrTimeUp t -> Just t
    TimeUp t -> Just t

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
