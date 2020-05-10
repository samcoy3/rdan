{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Vote
import Commands

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import Discord
import Discord.Types
import qualified Discord.Requests as R

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
                          else handleDM g disc event
  _ -> return ()

handleServerMessage :: GameState -> DiscordHandle -> Event -> IO ()
handleServerMessage g disc (MessageCreate m)
  -- Checks if command is in the command map.
  | (head . T.words . messageText $ m) `elem` M.keys Commands.commandMap =
      (Commands.commandMap M.! (head . T.words . messageText $ m)) disc g m
  | otherwise = return ()

handleServerMessage _ _ _ = return ()

--- DMS ---

-- Handles a DM.
-- If there's a vote in progress and the DMing user has not voted, then records the vote.
-- If this vote ends the vote, then ends the vote.
-- If the user isn't eligible or has already voted, then ignores the vote.
handleDM :: GameState -> DiscordHandle -> Event -> IO ()
handleDM g@(vote, _) disc event = case event of
  MessageCreate m -> do
    currentVote <- readTVarIO vote
    case currentVote of
       NoVote -> return ()
       VoteInProgress r _ _ -> if (user `elem` M.keys r) || (user `elem` Config.players)
                               then return ()
                               else do
         Commands.sendMessage disc (messageChannel m) "Your response has been recorded"
         atomically $ modifyTVar vote (\v -> v {responses = M.insert user (messageText m) (responses currentVote)})
         newStateOfVote <- readTVarIO vote
         if (length . M.keys . responses $ newStateOfVote) == length Config.players
           then Commands.endVote disc g m
           else return ()
           where user = userId . messageAuthor $ m
  _ -> return ()

