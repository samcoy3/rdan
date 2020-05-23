{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Random
import Text.Read

import Vote
import Config
import Util

import Discord
import Discord.Types
import qualified Discord.Requests as R

type ScoreMap = M.Map UserId Int
type GameState = (TVar Votes, TVar ScoreMap)

--- Command type ---

data Retrievable = Rule Int | Motion Int deriving (Show, Eq)
data Command =
  Help (Maybe Text)
  | PrintScores
  | AddToScore UserId Int
  | Find Retrievable
  | FindInline [Retrievable]
  | Roll Int Int
  | NewVote (EndCondition (Either NominalDiffTime (Int, Int))) Text
  | VoteStatus (Maybe [VoteId])
  | EndVote [VoteId]
  deriving (Show, Eq)

--- COMMAND ACTION ---

enactCommand :: DiscordHandle -> GameState -> Message -> Command -> IO ()

enactCommand disc _ m (Help s) = sendMessage disc (messageChannel m)
  (case s of
    Nothing -> "I'm rdan, the robot delightfully aiding Nomic.\n"
               <> "To view help on a specific command, type `!help` followed by the command you want to learn about. For example: `!help scores` to learn more about the `!scores` command.\n"
               <> "The list of commands available are as follows: `help`, `scores`, `addscore`, `rule`, `motion`, `newote`, `roll`, `votestatus`, `endvote`."
    Just "help" -> "To view help on a specific command, type `!help` followed by the command you want to learn about. For example: `!help scores` to learn more about the `!scores` command.\n"
               <> "The list of commands available are as follows: `help`, `scores`, `addscore`, `rule`, `motion`, `newote`, `roll`, `votestatus`, `endvote`."
    Just "scores" -> "Usage: `!scores`\n"
                     <> "Displays the current scores of all the players."
    Just "addscore" -> "Usage: `!addscore <player> <delta>`\n"
                       <> "Changes `<player>`'s score by `<delta>`. You must tag `<player` to use this command."
    Just "rule" -> "Usage: `!rule <rule_number>`\n"
                   <> "Retrieves the rule numbered `<rule_number>`. You can also call this inline with `!r<rule_number>`."
    Just "motion" -> "Usage: `!motion <motion_number>`\n"
                     <> "Retrieves the motion numbered `<motion_number>`. You can also call this inline with `!m<motion_number>`."
    Just "roll" -> "Usage: `!roll <x>d<y>`\n"
                   <> "Rolls a d`<y>`dice `<x>` times and displays the results."
    Just "newvote" -> "Usage: `!newvote <end_condition> <purpose...`\n"
                      <> "Starts a new vote on the subject of `<purpose>`.\n"
                      <> "`end_condition` determines how the vote ends. It can be one of five things:\n"
                      <> "- `all votes`: Ends the vote after everyone has voted.\n"
                      <> "- `after XXdYYhZZm`: Ends the vote after XX days, YY hours, and ZZ minutes (you can omit any of these, e.g. XXhYYm).\n"
                      <> "- `at XX:YY`: Ends the vote at the next XX:YY, where XX:YY is a 24-hour time. For example, `at 13:00` will end the vote at 13:00 today if it's before 13:00 now, otherwise it will end it tomorrow at 13:00.\n"
                      <> "- `all votes or after XXdYYhZZm`: Ends the vote after everyone has voted, or the specified interval, whichever comes first.\n"
                      <> "- `all votes or at XX:YY`: Ends the vote after everyone has voted, or at the specified time, whichever comes first.\n"
    Just "votestatus" -> "Usage: `!votestatus [#XX #YY...`\n"
                         <> "Queries the status of the ongoing votes. Will show how many people have voted, and display the subject of the vote.\n"
                         <> "You can optionally supply one or several vote IDs in order to restrict your query to those votes, e.g. `!votestatus #23`."
    Just "endvote" -> "Usage: `!endvote #XX #YY ...`\n"
                      <> "Prematurely ends the vote(s) with the specified ID(s).\n"
                      <> "Note that a vote will end automatically once every player has voted or the time has been reached anyway; you should use this command only if you want to end the vote prior to everyone having voted."
    )

enactCommand disc (_, s) m PrintScores = do
  currentScores <- readTVarIO s
  let scoreText = "The current scores are:\n"
        `T.append` T.unlines [T.pack $ (Config.playerNames M.! p) ++ ": " ++ show score | (p,score) <- M.toList currentScores]
  sendMessage disc (messageChannel m) scoreText

enactCommand disc (_, s) m (AddToScore user delta) = do
  atomically $ modifyTVar s (M.update (pure . (+delta)) user)
  sendMessage disc (messageChannel m) "Scores updated."

enactCommand disc _ m (Find x) = do
  result <- getRetrieveable disc x
  sendMessage disc (messageChannel m) $
    (case result of
       Left retr -> (T.pack . show $ retr) <> " not found, sorry!"
       Right ruleText -> T.unlines . tail . T.lines $ ruleText
    )

enactCommand disc _ m (FindInline xs) = do
  results <- getRetrieveables disc xs
  sendMessage disc (messageChannel m ) $
    T.unlines $
    map (\r -> case r of
            Left retr -> "Could not find " <> (T.pack . show $ retr) <> "."
            Right ruleText -> ruleText
        ) results
    where
      getRetrieveables _ [] = return []
      getRetrieveables disc' (x:xs') = do
        current <- getRetrieveable disc' x
        futures <- getRetrieveables disc' xs'
        return $ current : futures

enactCommand disc _ m (Roll quant sides) = do
  let bounds = replicate quant (1, sides) :: [(Int, Int)]
  rolls <- mapM randomRIO bounds
  sendMessage disc (messageChannel m) $
    (T.pack . show . sum $ rolls) <>
    " ⟵ " <>
    (T.pack . show $ rolls)

-- TODO: This handles failure to establish message channels very poorly.
-- TODO: Time handling is pretty shocking, but can probably be cleaner.
enactCommand disc (v, _) m (NewVote endCon purpose') = do
  let timeFromHM (h', m') =
        (do
            currentTime <- getCurrentTime
            if (localTimeOfDay . utcToLocalTime bst) currentTime > TimeOfDay {todHour = h', todMin = m', todSec = 0}
              then return . localTimeToUTC bst $
                   LocalTime { localDay = addDays 1 (utctDay currentTime)
                             , localTimeOfDay = TimeOfDay { todHour = h'
                                                          , todMin = m'
                                                          , todSec = 0}}
              else return . localTimeToUTC bst $
                   LocalTime { localDay = utctDay currentTime
                             , localTimeOfDay = TimeOfDay { todHour = h'
                                                          , todMin = m'
                                                          , todSec = 0}})
  currentVotes <- readTVarIO v
  endCon' <- case endCon of
    AllVoted -> return AllVoted
    (AllVotedOrTimeUp (Left diffTime)) -> getCurrentTime
      >>= return . AllVotedOrTimeUp . (addUTCTime diffTime)
    (TimeUp (Left diffTime)) -> getCurrentTime
      >>= return . TimeUp . (addUTCTime diffTime)
    (AllVotedOrTimeUp (Right (h, m))) -> timeFromHM ( fromIntegral h
                                                    , fromIntegral m)
      >>= return . AllVotedOrTimeUp
    (TimeUp (Right (h, m))) -> timeFromHM ( fromIntegral h
                                          , fromIntegral m)
      >>= return . TimeUp

  userHandles <- getDMs disc Config.players
  messageHandles <- mapM ((\c -> restCall disc
                            $ R.CreateMessage c
                            $ "A new vote has begun on the subject of **" <> purpose' <> "**! " <> endConditionDescription endCon' <> " Please react to this message with a tick or a cross in order to vote:") . channelId) userHandles
  let newid = (+1) $ max 0 (if (M.keys currentVotes) == []
                            then 0
                            else maximum . M.keys $ currentVotes)
  sendMessage disc (messageChannel m) $ "A new vote has been started, and players have been notified. The vote ID is #" <> (T.pack . show) newid
  atomically $ modifyTVar v
    (M.insert newid (Vote { messages = M.fromList $ zip (map messageId $ rights messageHandles) Config.players
                          , responses = M.fromList $ zip Config.players $ repeat []
                          , purpose = purpose'
                          , announceChannel = messageChannel m
                          , endCondition = endCon'}))

enactCommand disc (v, _) m (VoteStatus voteids) = do
  currentVotes <- readTVarIO v
  case voteids of
    Nothing -> sendMessage disc (messageChannel m) $
               if M.null currentVotes
               then "There are no votes ongoing."
               else "The current active votes are as follows:\n" <>
                    (T.unlines ["**Vote #" <> (T.pack . show) vid <> "**: " <> describe vote | (vid, vote) <- M.toList currentVotes])
    Just voteids -> sendMessage disc (messageChannel m) $
                    T.unlines $ (flip map voteids)
                    (\v -> if v `elem` (M.keys currentVotes)
                              then "**Vote #" <> (T.pack . show) v <> "**: " <> describe (currentVotes M.! v)
                              else "There is no vote with the vote ID #" <> (T.pack . show) v)

enactCommand disc g@(v, _) m (EndVote (vote:vs)) = do
  currentVotes <- readTVarIO v
  if vote `elem` (M.keys currentVotes)
    then endVote disc v vote
    else sendMessage disc (messageChannel m) $
         "There is no vote with the vote ID #" <> (T.pack . show) vote
  enactCommand disc g m (EndVote vs)
enactCommand _ _ _ (EndVote []) = return ()
>>>>>>> voting-overhaul

--- MISC ---

getRetrieveable :: DiscordHandle -> Retrievable -> IO (Either Retrievable Text)
getRetrieveable disc retr =
  do
    messages <- restCall disc $
                R.GetChannelMessages channel (100, R.LatestMessages)
    let validRules = filter
                     (\t -> T.pack ("**" ++ show retr ++ "**") `T.isPrefixOf` t)
                     (map messageText $ fromRight [] messages)
    return $
      if null validRules
      then Left retr
      else Right (head validRules)
      where
        channel = case retr of
          Rule _ -> Config.rulesChannel
          Motion _ -> Config.motionsChannel

getDMs :: DiscordHandle -> [UserId] -> IO [Channel]
getDMs _ [] = return []
getDMs disc (u:us) = do
  channel <- restCall disc $ R.CreateDM u
  restChannels <- getDMs disc us
  case channel of
    Left _ -> return restChannels
    Right c -> return $ c : restChannels
