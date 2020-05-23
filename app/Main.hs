{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config
import Vote
import Commands
import CommandParser

import Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TVar

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Attoparsec.Text as A

main :: IO ()
main = do
  currentVotes <- newTVarIO M.empty
  currentScores <- newTVarIO Config.scores
  let gameState = (currentVotes, currentScores)
  userReadableError <- runDiscord $ def
                       { discordToken = Config.botToken
                       , discordOnEvent = handleEvent gameState
                       , discordOnStart = (\disc -> forkIO (votePoller disc currentVotes) >> return ())}
  TIO.putStrLn userReadableError

handleEvent :: GameState -> DiscordHandle -> Event -> IO ()
handleEvent g disc event = case event of
  MessageCreate m | userIsBot $ messageAuthor m -> return ()
                  | otherwise -> do
                      channel <- restCall disc $ R.GetChannel (messageChannel m)
                      case channel of
                        Left _ -> return ()
                        Right channel' ->
                          if channelIsInGuild channel'
                          then handleServerMessage g disc event
                          else return ()
  MessageReactionAdd reactInfo -> addReactToVote disc g reactInfo
  MessageReactionRemove reactInfo -> removeReactFromVote disc g reactInfo
  _ -> return ()

handleServerMessage :: GameState -> DiscordHandle -> Event -> IO ()
handleServerMessage g disc (MessageCreate m) =
  case (A.parseOnly commandParser $ messageText m) of
    Left _ -> return ()
    Right cmd -> enactCommand disc g m cmd

handleServerMessage _ _ _ = return ()

--- DM Voting ---
-- TODO: Small race condition here involving the vote ending. Shouldn't be a problem though.
addReactToVote :: DiscordHandle -> GameState -> ReactionInfo -> IO ()
addReactToVote disc g@(votes, _) reactInfo = do
  currentVotes <- readTVarIO votes
  let correspondingVotes = M.filter
                           (\vote -> (reactionMessageId reactInfo) `elem` (M.keys . messages $ vote))
                           currentVotes
  if M.null correspondingVotes
    then return ()
    else do
    let (voteid, vote) = (head . M.toList) correspondingVotes
    atomically . modifyTVar votes $
      M.adjust
      (\vote -> vote {responses = M.adjust
                       (\t -> t++[emojiName . reactionEmoji $ reactInfo])
                       ((messages vote) M.! (reactionMessageId reactInfo))
                       (responses vote)})
      voteid
    newVotes <- readTVarIO votes
    if (M.size . M.filter (/= []) $ responses (newVotes M.! voteid)) == length Config.players
      then (case endCondition (newVotes M.! voteid) of
              TimeUp _ -> return ()
              _ ->  endVote disc votes voteid)
      else return ()

removeReactFromVote :: DiscordHandle -> GameState -> ReactionInfo -> IO ()
removeReactFromVote disc g@(votes, _) reactInfo = do
  currentVotes <- readTVarIO votes
  let correspondingVotes = M.filter
                           (\vote -> (reactionMessageId reactInfo) `elem` (M.keys . messages $ vote))
                           currentVotes
  if M.null correspondingVotes
    then return ()
    else do
    let (voteid, vote) = (head . M.toList) correspondingVotes
    atomically . modifyTVar votes $
      M.adjust
      (\vote -> vote {responses = M.adjust
                       (\t -> delete (emojiName . reactionEmoji $ reactInfo) t)
                       ((messages vote) M.! (reactionMessageId reactInfo))
                       (responses vote)})
      voteid
