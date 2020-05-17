{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import System.Random
import Text.Read

import Vote
import Config

import Discord
import Discord.Types
import qualified Discord.Requests as R

type ScoreMap = M.Map UserId Int
type GameState = (TVar Vote, TVar ScoreMap)

--- Command type ---

data Retrievable = Rule Int | Motion Int deriving (Show, Eq)
data Command =
  Help (Maybe Text)
  | PrintScores
  | AddToScore UserId Int
  | Find Retrievable
  | FindInline [Retrievable]
  | Roll Int Int
  | NewVote Text
  | VoteStatus
  | EndVote
  deriving (Show, Eq)

-- A utility for sending messages. Currently we ignore whether it was successful.
sendMessage :: DiscordHandle -> ChannelId -> Text -> IO ()
sendMessage disc channel m = do
  _ <- restCall disc $ R.CreateMessage channel m
  return ()

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
    Just "newvote" -> "Usage: `!newvote <purpose>`\n"
                      <> "Starts a new vote on the subject of `<purpose>`."
    Just "votestatus" -> "Usage: `!votestatus`\n"
                         <> "Queries the status of the ongoing vote. Will show how many people have voted, and display the subject of the vote."
    Just "endvote" -> "Usage: `!endvote`\n"
                      <> "Prematurely ends the current vote. Note that a vote will end automatically once every player has voted anyway; you should use this command only if you want to end the vote prior to everyone having voted."
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

enactCommand disc (v, _) m (NewVote purpose') = do
  currentVote <- readTVarIO v
  case currentVote of
    VoteInProgress {} -> sendMessage
                         disc
                         (messageChannel m)
                         "There is currently a vote in progress; you cannot start a new vote at this time."
    NoVote -> do
      atomically $ writeTVar v VoteInProgress
        { responses = M.empty
        , purpose = purpose'
        , announceChannel = messageChannel m}
      userHandles <- getDMs disc Config.players
      mapM_ ((\c -> sendMessage disc c $ "A new vote has begun! Please write a response to this message to vote on the matter of **" `T.append` purpose' `T.append` "**:") . channelId) userHandles

enactCommand disc (v, _) m (VoteStatus) = do
  currentVote <- readTVarIO v
  sendMessage disc (messageChannel m) $
    case currentVote of
      NoVote -> "There is not currently a vote in progress."
      VoteInProgress r p _ ->
        "There is currently a vote in progress on the matter of **" `T.append`
        p `T.append`
        "**. So far, out of the required " `T.append`
        (T.pack . show . length $ Config.players) `T.append`
        " players, " `T.append`
        (T.pack . show . length . M.keys $ r) `T.append`
        " have voted."

enactCommand disc (v, _) m (EndVote) = do
  stateOfVote <- readTVarIO v
  case stateOfVote of
    NoVote -> sendMessage disc (messageChannel m)
              "There is currently no vote to end."
    VoteInProgress{} -> do
      atomically $ writeTVar v NoVote
      sendMessage disc (announceChannel stateOfVote) $
        "The vote on **" `T.append`
        purpose stateOfVote `T.append`
        "** has been concluded. The results are:\n" `T.append`
        T.unlines ["**" `T.append` T.pack (Config.playerNames M.! p) `T.append` "**: " `T.append` e | (p,e) <- M.toList (responses stateOfVote)]

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
