{-# LANGUAGE OverloadedStrings #-}

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

--- Vote Poller ---
-- This function checks, every minute, whether a vote has ended.
votePoller :: DiscordHandle -> TVar Votes -> IO ()
votePoller disc votes = do
  currentTime <- getCurrentTime
  currentVotes <- readTVarIO votes
  let votesToEnd = M.keys .
                   M.filter (\v -> case endCondition v of
                                     AllVoted -> False
                                     TimeUp t -> currentTime > t
                                     AllVotedOrTimeUp t -> currentTime > t)
                   $ currentVotes
  mapM_ (endVote disc votes) votesToEnd
  threadDelay $ (1000000 * 60 :: Int) -- Sleeps for one minute
  votePoller disc votes

endVote :: DiscordHandle -> TVar Votes -> Int -> IO ()
endVote disc v voteid = do
  stateOfVotes <- readTVarIO v
  let particularVote = stateOfVotes M.!? voteid
  case particularVote of
    Nothing -> return ()
    Just vote -> do
      atomically $ modifyTVar v (M.delete voteid)
      sendMessage disc (announceChannel vote) $
        "The vote on **" <>
        purpose vote <>
        "** has concluded. The results are:\n" <>
        T.unlines ["**" <> T.pack (Config.playerNames M.! p) <> "**: " <> (T.unwords e) | (p,e) <- M.toList (responses vote)]

describe :: Vote -> Text
describe vote =
  "On the subject of **" <>
  purpose vote <>
  "**. So far, out of the " <>
  (T.pack . show . length $ Config.players) <>
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
