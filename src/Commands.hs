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

data AbsoluteTime = AbsTime { hours :: Int
                            , minutes :: Int
                            , dayOffset :: Integer } deriving (Show, Eq)
data Retrievable = Rule Int | Motion Int deriving (Show, Eq)
data Command =
  Help (Maybe Text)
  | PrintScores
  | AddToScore UserId Int
  | Find Retrievable
  | FindInline [Retrievable]
  | Roll Int Int
  | NewVote (EndCondition (Either NominalDiffTime AbsoluteTime)) Text
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
                      <> "`end_condition` determines how the vote ends. It is constructed as follows:\n"
                      <> "- Firstly, if there is a `!` at the beginning of `end_condition` then the vote will not end when all votes are in.\n"
                      <> "- Next, optional time constraints can be specified. To specify a period of time, use the syntax `XXdYYhZZm`, where `XX`, `YY`, and `ZZ` are numbers denoting how many days, minutes, and seconds the vote is to last. Parts of this can be omitted, e.g. `YYhZZm`. Alternatively, you can specify the precise time the vote is to end using `HH:MM+D` syntax, where `HH` is the hours (in the 24 hour clock), `MM` is the minutes, and `DD` is the offset in days from the next instance of that time.\n"
                      <> "\nExamples:\n "
                      <> "- `!newvote Apples` will start a new vote on the subject of Apples. The vote will only end when everyone has voted.\n"
                      <> "- `!newvote 24h Derek` will start a new vote on the subject of Derek. The vote lasts 24 hours, but can end early (if everyone votes before then).\n"
                      <> "- `!newvote !18:00+1 Snakes` will start a new vote on the subject of Snakes. The vote will end at the 18:00 after next (if this vote was proposed on Tuesday at 1700, this would be Wednesday at 1800; if this vote was proposed on Tuesday at 1900, this would be Thursday at 1900). The vote may not end early, even if everyone has voted."
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
  currentVotes <- readTVarIO v
  endCon' <- case endCon of
    AllVoted -> return AllVoted
    (AllVotedOrTimeUp (Left diffTime)) -> getCurrentTime
      >>= return . AllVotedOrTimeUp . (addUTCTime diffTime)
    (TimeUp (Left diffTime)) -> getCurrentTime
      >>= return . TimeUp . (addUTCTime diffTime)
    (AllVotedOrTimeUp (Right abs)) -> utcFromHoursMinutesDayOffset
      (hours abs, minutes abs, dayOffset abs)
      >>= return . AllVotedOrTimeUp
    (TimeUp (Right abs)) -> utcFromHoursMinutesDayOffset
      (hours abs, minutes abs, dayOffset abs)
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
