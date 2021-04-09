module Vote where

import Config
import Util

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

--- Vote Poller ---
-- This function frequently checks whether a vote has ended.
votePoller :: TVar Votes -> BotM ()
votePoller votes = do
  currentTime <- liftIO getCurrentTime
  currentVotes <- readTVarDisc votes
  let votesToEnd = M.keys .
                   M.filter (\v -> case endCondition v of
                                     AllVoted -> False
                                     TimeUp t -> currentTime > t
                                     AllVotedOrTimeUp t -> currentTime > t)
                   $ currentVotes
  mapM_ (endVote votes) votesToEnd
  liftIO . threadDelay $ (1000000 * 15 :: Int) -- Sleeps for fiteen seconds
  votePoller votes

endVote :: TVar Votes -> Int -> BotM ()
endVote v voteid = do
  stateOfVotes <- readTVarDisc v
  config <- getConfig
  let particularVote = stateOfVotes M.!? voteid
  case particularVote of
    Nothing -> return ()
    Just vote -> do
      liftIO . atomically $ modifyTVar v (M.delete voteid)
      sendMessage (announceChannel vote) $
        "The vote on **" <>
        purpose vote <>
        "** has concluded. The results are:\n" <>
        T.unlines ["**" <> getPlayerNameFromID config p <> "**: " <> (T.unwords e) | (p,e) <- M.toList (responses vote)]

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
