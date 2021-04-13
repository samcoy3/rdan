module Handlers where

import Commands
import CommandParser
import Game

import Data.Attoparsec.Text as A

import Control.Monad.Reader
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Discord
import Discord.Types
import Discord.Requests as R

handleEvent :: Event -> BotM ()
handleEvent event = case event of
  MessageCreate m | userIsBot $ messageAuthor m -> return ()
                  | otherwise -> do
                      channel <- lift . restCall $ R.GetChannel (messageChannel m)
                      case channel of
                        Left _ -> return ()
                        Right channel' ->
                          if channelIsInGuild channel'
                          then handleServerMessage event
                          else return ()
  MessageReactionAdd reactInfo -> addReactToVote reactInfo
  MessageReactionRemove reactInfo -> removeReactFromVote reactInfo
  _ -> return ()

handleServerMessage :: Event -> BotM ()
handleServerMessage (MessageCreate m) =
  case (A.parseOnly commandParser $ messageText m) of
    Left _ -> return ()
    Right cmd -> enactCommand m cmd

handleServerMessage _ = return ()

--- DM Voting ---
-- TODO: Small race condition here involving the vote ending. Shouldn't be a problem though.
addReactToVote :: ReactionInfo -> BotM ()
addReactToVote reactInfo = do
  playerCount <- length <$> players <$> getConfig
  currentVotes <- votes <$> getGameState
  let correspondingVotes = M.filter
                           (\vote -> (reactionMessageId reactInfo) `elem` (M.keys . messages $ vote))
                           currentVotes
  if M.null correspondingVotes
    then return ()
    else do
    let (voteid, vote) = (head . M.toList) correspondingVotes
    modifyVotes $
      M.adjust
      (\vote -> vote {responses = M.adjust
                       (\t -> t++[emojiName . reactionEmoji $ reactInfo])
                       ((messages vote) M.! (reactionMessageId reactInfo))
                       (responses vote)})
      voteid
    newVotes <- votes <$> getGameState
    if (M.size . M.filter (/= []) $ responses (newVotes M.! voteid)) == playerCount
      then (case endCondition (newVotes M.! voteid) of
              TimeUp _ -> return ()
              _ ->  endVote voteid)
      else return ()

removeReactFromVote :: ReactionInfo -> BotM ()
removeReactFromVote reactInfo = do
  currentVotes <- votes <$> getGameState
  let correspondingVotes = M.filter
                           (\vote -> (reactionMessageId reactInfo) `elem` (M.keys . messages $ vote))
                           currentVotes
  if M.null correspondingVotes
    then return ()
    else do
    let (voteid, vote) = (head . M.toList) correspondingVotes
    modifyVotes $
      M.adjust
      (\vote -> vote {responses = M.adjust
                       (\t -> delete (emojiName . reactionEmoji $ reactInfo) t)
                       ((messages vote) M.! (reactionMessageId reactInfo))
                       (responses vote)})
      voteid
