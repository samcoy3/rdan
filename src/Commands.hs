{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Text.Read

import Vote
import Config

import Discord
import Discord.Types
import qualified Discord.Requests as R

type ScoreMap = M.Map UserId Int
type GameState = (TVar Vote, TVar ScoreMap)
type Command = DiscordHandle -> GameState -> Message -> IO ()

-- A utility for sending messages. Currently we ignore whether it was successful.
sendMessage :: DiscordHandle -> ChannelId -> Text -> IO ()
sendMessage disc channel m = do
  _ <- restCall disc $ R.CreateMessage channel m
  return ()

commandMap :: M.Map Text Command
commandMap = M.fromList
  [ ("!help", printHelp)
  , ("!scores", printScores)
  , ("!addscore", updateScore)
  , ("!rule", findRule)
  , ("!motion", findMotion)
  , ("!newvote", startVote)
  , ("!voteStatus", voteStatus)
  , ("!endvote", endVote)]

--- COMMANDS ---

-- Prints the help text.
printHelp :: Command
printHelp disc _ message = sendMessage disc (messageChannel message) helpText

-- Prints the current scores.
printScores :: Command
printScores disc (_, s) message = do
  currentScores <- readTVarIO s
  let scoreText = "The current scores are:\n"
        `T.append` T.unlines [T.pack $ (Config.playerNames M.! p) ++ ": " ++ show score | (p,score) <- M.toList currentScores]
  sendMessage disc (messageChannel message) scoreText

-- Updates a user's score.
updateScore :: Command
updateScore disc (_, s) message = do
  atomically $ modifyTVar s (M.update (pure . (+amount)) uid)
  sendMessage disc (messageChannel message) "Scores updated."
    where
      amount = read . T.unpack . last . T.words . messageText $ message
      uid = userId . head . messageMentions $ message

-- Finds a specified rule.
findRule :: Command
findRule = findRuleOrMotion "Rule" Config.rulesChannel

-- Finds a specified motion.
findMotion :: Command
findMotion = findRuleOrMotion "Motion" Config.motionsChannel

-- Starts a new vote, or tells the user asking that it is not possible.
startVote :: Command
startVote disc (v, _) message = do
  currentVote <- readTVarIO v
  case currentVote of
    VoteInProgress {} -> sendMessage
                         disc
                         (messageChannel message)
                         "There is currently a vote in progress; you cannot start a new vote at this time."
    NoVote -> do
      let purpose' = T.unwords . tail . T.words $ messageText message
      atomically $ writeTVar v VoteInProgress
        { responses = M.empty
        , purpose = purpose'
        , announceChannel = messageChannel message}
      userHandles <- getDMs disc Config.players
      mapM_ ((\c -> sendMessage disc c $ "A new vote has begun! Please write a response to this message to vote on the matter of **" `T.append` purpose' `T.append` "**:") . channelId) userHandles

-- Prints the status of the current vote.
voteStatus :: Command
voteStatus disc (v, _) message = do
  currentVote <- readTVarIO v
  sendMessage disc (messageChannel message) $
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

-- Ends the current vote. If called as a command, this will be premature.
endVote :: Command
endVote disc (v, _) m = do
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

findRuleOrMotion :: String -> ChannelId -> Command
findRuleOrMotion search channel disc _ message =
  let number = (readMaybe . T.unpack . head . tail . T.words . messageText $ message) :: Maybe Int in
  do
    messages <- restCall disc $
                R.GetChannelMessages channel (100, R.LatestMessages)
    let validRules = filter
                     (\t -> T.pack ("**" ++ search ++ " " ++ show (fromMaybe 0 number) ++ "**") `T.isPrefixOf` t)
                     (map messageText $ fromRight [] messages)
    sendMessage disc (messageChannel message) $
      if null validRules
      then T.pack "Nothing found, sorry!"
      else T.unlines . tail . T.lines . head $ validRules

getDMs :: DiscordHandle -> [UserId] -> IO [Channel]
getDMs _ [] = return []
getDMs disc (u:us) = do
  channel <- restCall disc $ R.CreateDM u
  restChannels <- getDMs disc us
  case channel of
    Left _ -> return restChannels
    Right c -> return $ c : restChannels

helpText :: Text
helpText = T.unlines
  [ "Welcome to rdan, your robot delightfully assisting Nomic."
  , "You can use the following commands to interact with the bot:"
  , "`!rule <rule_number>`: Prints out the text of the relevant rule."
  , "`!motion <motion_number>`: Prints out the text of the relevant motion."
  , "`!scores`: Prints out the current scores for all players."
  , "`!addscore <tag_player> <score_change>`: Changes the score of the player you tag by the number of points you specify."
  , "`!newvote <topic>`: Starts a new vote on the topic you specify, and sends a DM to all players. You vote by replying to the DM."
  , "`!votestatus`: Reminds you of the topic of the current vote, and reveals how many people have voted so far."
  , "`!endvote`: Prematurely ends the vote, revealing all results. Once everyone has voted, this will happen anyway."]
