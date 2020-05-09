{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Vote

import Data.Either
import qualified Data.Text as T
import Data.Text (Text, isPrefixOf)
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import Discord
import Discord.Types
import qualified Discord.Requests as R

type ScoreMap = M.Map UserId Int

startingScores :: ScoreMap
startingScores = M.fromList $ [(p, 0) | p <- Config.players]

sendMessage :: DiscordHandle -> ChannelId -> Text -> IO ()
sendMessage disc channel m = do
  _ <- restCall disc $ R.CreateMessage channel m
  return ()

main :: IO ()
main = do
  currentVote <- newTVarIO NoVote
  currentScores <- newTVarIO startingScores
  userError <- runDiscord $ def
               { discordToken = Config.botToken
               , discordOnEvent = handleEvent currentVote currentScores }
  TIO.putStrLn userError

handleEvent :: TVar Vote -> TVar ScoreMap -> DiscordHandle -> Event -> IO ()
handleEvent vote scores disc event = case event of
  MessageCreate m | userIsBot $ messageAuthor m -> return ()
                  | otherwise -> do
                      channel <- restCall disc $ R.GetChannel (messageChannel m)
                      case channel of
                        Left _ -> return ()
                        Right channel' ->
                          if channelIsInGuild channel'
                          then handleServerMessage vote scores disc event
                          else handleDM vote disc event
  _ -> return ()

handleServerMessage :: TVar Vote -> TVar ScoreMap -> DiscordHandle -> Event -> IO ()
handleServerMessage vote scores disc (MessageCreate m)
  | "!scores" `isPrefixOf` messageText m = do
      currentScores <- getScores scores
      sendMessage disc (messageChannel m) currentScores

  | "!addscore" `isPrefixOf` messageText m = do
      _ <- updateScore
           scores
           (userId . head . messageMentions $ m)
           (read . T.unpack . last . T.words . messageText $ m)
      sendMessage disc (messageChannel m) "Scores updated."

  | "!newvote" `isPrefixOf` messageText m = do
      currentVote <- readTVarIO vote
      case currentVote of
        (VoteInProgress _ _ _) -> sendMessage
                                  disc
                                  (messageChannel m)
                                  "There is currently a vote in progress; you cannot start a new vote at this time."
        NoVote -> do
          _ <- newVote vote (messageChannel m) (T.unwords . tail . T.words $ messageText m)
          vote' <- readTVarIO vote
          userHandles <- getDMs disc Config.players
          mapM_ ((\c -> sendMessage disc c $ "A new vote has begun! Please write a response to this message to vote on the matter of **" `T.append` purpose vote' `T.append` "**:") . channelId) userHandles

  | "!votestatus" `isPrefixOf` messageText m = do
      currentVote <- readTVarIO vote
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

  | "!endvote" `isPrefixOf` messageText m = endVote vote disc

  | "!rule" `isPrefixOf` messageText m = do
      rule <- getRule disc $ read . T.unpack . last . T.words . messageText $ m
      sendMessage disc (messageChannel m) rule

  | "!motion" `isPrefixOf` messageText m = do
      motion <- getMotion disc (read . T.unpack . last . T.words . messageText $ m)
      sendMessage disc (messageChannel m) motion

  | "!help" `isPrefixOf` messageText m =
      sendMessage disc (messageChannel m) $
      T.unlines [ "Welcome to rdan, your robot delightfully assisting Nomic."
                , "You can use the following commands to interact with the bot:"
                , "`!rule <rule_number>`: Prints out the text of the relevant rule."
                , "`!motion <motion_number>`: Prints out the text of the relevant motion."
                , "`!scores`: Prints out the current scores for all players."
                , "`!addscore <tag_player> <score_change>`: Changes the score of the player you tag by the number of points you specify."
                , "`!newvote <topic>`: Starts a new vote on the topic you specify, and sends a DM to all players. You vote by replying to the DM."
                , "`!votestatus`: Reminds you of the topic of the current vote, and reveals how many people have voted so far."
                , "`!endvote`: Prematurely ends the vote, revealing all results. Once everyone has voted, this will happen anyway."]

  | otherwise = return ()

handleServerMessage _ _ _ _ = return ()

--- SERVER HANDLES ---

getScores :: TVar ScoreMap -> IO Text
getScores scoreVar = do
  scores <- readTVarIO scoreVar
  return $ "The current scores are:\n"
    `T.append` (T.unlines [T.pack $ (Config.playerNames M.! p) ++ ": " ++ (show s) |
                   (p,s) <- M.toList scores])

updateScore :: TVar ScoreMap -> UserId -> Int -> IO ()
updateScore score uid amount = atomically $
  modifyTVar score (M.update (pure . (+amount)) uid)

getRuleOrMotion :: String -> ChannelId -> DiscordHandle -> Int -> IO Text
getRuleOrMotion search channel disc number = do
  messages <- restCall disc $
              R.GetChannelMessages channel (100, R.LatestMessages)
  validRules <- return $
    filter (\t -> (T.pack $ "**" ++ search ++ " " ++ show number ++ "**") `T.isPrefixOf` t)
    (map messageText $ fromRight [] messages)
  return $ if null validRules
           then T.pack "Nothing found, sorry!"
           else T.unlines . tail . T.lines . head $ validRules

getRule :: DiscordHandle -> Int -> IO Text
getRule = getRuleOrMotion "Rule" Config.rulesChannel

getMotion :: DiscordHandle -> Int -> IO Text
getMotion = getRuleOrMotion "Motion" Config.motionsChannel

newVote :: TVar Vote -> ChannelId -> Text -> IO ()
newVote vote channel purpose' = atomically $
  writeTVar vote VoteInProgress { responses = M.empty
                                , purpose = purpose'
                                , announceChannel = channel}

endVote :: TVar Vote -> DiscordHandle -> IO ()
endVote vote disc = do
  stateOfVote <- readTVarIO vote
  sendMessage disc (announceChannel stateOfVote) $
    "The vote on **" `T.append`
    purpose stateOfVote `T.append`
    "** has been concluded. The results are:\n" `T.append`
    (T.unlines ["**" `T.append` (T.pack $ Config.playerNames M.! p) `T.append` "**: " `T.append` e | (p,e) <- M.toList (responses stateOfVote)])

--- DMS ---

getDMs :: DiscordHandle -> [UserId] -> IO [Channel]
getDMs _ [] = return []
getDMs disc (u:us) = do
  channel <- restCall disc $ R.CreateDM u
  restChannels <- getDMs disc us
  case channel of
    Left _ -> return restChannels
    Right c -> return $ c : restChannels

handleDM :: TVar Vote -> DiscordHandle -> Event -> IO ()
handleDM vote disc event = case event of
  MessageCreate m -> do
    currentVote <- readTVarIO vote
    (case currentVote of
       NoVote -> return ()
       VoteInProgress r _ _ -> if (user `elem` (M.keys r)) || (not $ user `elem` Config.players)
                               then return ()
                               else do
         sendMessage disc (messageChannel m) "Your response has been recorded"
         atomically $ modifyTVar vote (\v -> v {responses = M.insert user (messageText m) (responses currentVote)})
         newStateOfVote <- readTVarIO vote
         if (length . M.keys . responses $ newStateOfVote) == length Config.players
           then endVote vote disc
           else return ()
           where user = userId . messageAuthor $ m)
  _ -> return ()

