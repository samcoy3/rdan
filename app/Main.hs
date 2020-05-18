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

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Attoparsec.Text as A

main :: IO ()
main = do
  currentVote <- newTVarIO NoVote
  currentScores <- newTVarIO Config.scores
  let gameState = (currentVote, currentScores)
  userReadableError <- runDiscord $ def
                       { discordToken = Config.botToken
                       , discordOnEvent = handleEvent gameState }
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
addReactToVote :: DiscordHandle -> GameState -> ReactionInfo -> IO ()
addReactToVote disc g@(vote, _) reactInfo = do
  currentVote <- readTVarIO vote
  case currentVote of
    NoVote -> return ()
    VoteInProgress m r _ _ ->
      if ((reactionMessageId reactInfo) `notElem` M.keys m)
        then return ()
        else do
        atomically $
          modifyTVar vote
          (\v -> v {responses = M.adjust
                                (\t -> t++[emojiName . reactionEmoji $ reactInfo])
                                (m M.! (reactionMessageId reactInfo))
                                r})
        newVoteState <- readTVarIO vote
        if (M.size . M.filter (/= []) $ responses newVoteState) == length Config.players
          then endVote disc vote
          else return ()

removeReactFromVote :: DiscordHandle -> GameState -> ReactionInfo -> IO ()
removeReactFromVote disc g@(vote, _) reactInfo = do
  currentVote <- readTVarIO vote
  case currentVote of
    NoVote -> return ()
    VoteInProgress m r _ _ -> do
      if ((reactionMessageId reactInfo) `notElem` M.keys m)
        then return ()
        else atomically $
             modifyTVar vote
             (\v -> v {responses = M.adjust
                        (\t -> delete (emojiName . reactionEmoji $ reactInfo) t)
                        (m M.! (reactionMessageId reactInfo))
                        r})
