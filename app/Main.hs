{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config
import Vote
import Commands
import CommandParser
import Util

import Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
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
                       , discordOnStart = void $ ask >>= liftIO . forkIO . runReaderT (votePoller currentVotes)}
  TIO.putStrLn userReadableError

handleEvent :: GameState -> Event -> DiscordHandler ()
handleEvent g event = case event of
  MessageCreate m | userIsBot $ messageAuthor m -> return ()
                  | otherwise -> do
                      channel <- restCall $ R.GetChannel (messageChannel m)
                      case channel of
                        Left _ -> return ()
                        Right channel' ->
                          if channelIsInGuild channel'
                          then handleServerMessage g event
                          else return ()
  MessageReactionAdd reactInfo -> addReactToVote g reactInfo
  MessageReactionRemove reactInfo -> removeReactFromVote g reactInfo
  _ -> return ()

handleServerMessage :: GameState -> Event -> DiscordHandler ()
handleServerMessage g (MessageCreate m) =
  case (A.parseOnly commandParser $ messageText m) of
    Left _ -> return ()
    Right cmd -> enactCommand g m cmd

handleServerMessage _ _ = return ()

--- DM Voting ---
-- TODO: Small race condition here involving the vote ending. Shouldn't be a problem though.
addReactToVote :: GameState -> ReactionInfo -> DiscordHandler ()
addReactToVote g@(votes, _) reactInfo = do
  currentVotes <- readTVarDisc votes
  let correspondingVotes = M.filter
                           (\vote -> (reactionMessageId reactInfo) `elem` (M.keys . messages $ vote))
                           currentVotes
  if M.null correspondingVotes
    then return ()
    else do
    let (voteid, vote) = (head . M.toList) correspondingVotes
    modifyTVarDisc votes $
      M.adjust
      (\vote -> vote {responses = M.adjust
                       (\t -> t++[emojiName . reactionEmoji $ reactInfo])
                       ((messages vote) M.! (reactionMessageId reactInfo))
                       (responses vote)})
      voteid
    newVotes <- readTVarDisc votes
    if (M.size . M.filter (/= []) $ responses (newVotes M.! voteid)) == length Config.players
      then (case endCondition (newVotes M.! voteid) of
              TimeUp _ -> return ()
              _ ->  endVote votes voteid)
      else return ()

removeReactFromVote :: GameState -> ReactionInfo -> DiscordHandler ()
removeReactFromVote g@(votes, _) reactInfo = do
  currentVotes <- readTVarDisc votes
  let correspondingVotes = M.filter
                           (\vote -> (reactionMessageId reactInfo) `elem` (M.keys . messages $ vote))
                           currentVotes
  if M.null correspondingVotes
    then return ()
    else do
    let (voteid, vote) = (head . M.toList) correspondingVotes
    modifyTVarDisc votes $
      M.adjust
      (\vote -> vote {responses = M.adjust
                       (\t -> delete (emojiName . reactionEmoji $ reactInfo) t)
                       ((messages vote) M.! (reactionMessageId reactInfo))
                       (responses vote)})
      voteid
