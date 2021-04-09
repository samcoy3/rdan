{-# LANGUAGE LambdaCase #-}

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

import Data.Yaml (decodeFileEither)

main :: IO ()
main = do
  currentVotes <- newTVarIO M.empty
  decodeFileEither "config.yaml" >>= \case
    Left e -> do
      putStrLn (show e)
      putStrLn "Unable to read config file. Please check that it is formatted correctly."
    Right config -> do
        currentScores <- newTVarIO . M.fromList $ zip (getPlayerIDs config) (repeat 0)
        let gameState = (currentVotes, currentScores)
        TIO.putStrLn (T.pack . show $ config)
        userReadableError <- runDiscord $ def
                        { discordToken = botToken config
                        , discordOnEvent = \event -> runReaderT (handleEvent gameState event) config
                        , discordOnStart = (do
                            discordHandle <- ask
                            liftIO . forkIO $ runReaderT (runReaderT (votePoller currentVotes) config) discordHandle
                            return ())}
        TIO.putStrLn userReadableError

handleEvent :: GameState -> Event -> BotM ()
handleEvent g event = case event of
  MessageCreate m | userIsBot $ messageAuthor m -> return ()
                  | otherwise -> do
                      channel <- lift . restCall $ R.GetChannel (messageChannel m)
                      case channel of
                        Left _ -> return ()
                        Right channel' ->
                          if channelIsInGuild channel'
                          then handleServerMessage g event
                          else return ()
  MessageReactionAdd reactInfo -> addReactToVote g reactInfo
  MessageReactionRemove reactInfo -> removeReactFromVote g reactInfo
  _ -> return ()

handleServerMessage :: GameState -> Event -> BotM ()
handleServerMessage g (MessageCreate m) =
  case (A.parseOnly commandParser $ messageText m) of
    Left _ -> return ()
    Right cmd -> enactCommand g m cmd

handleServerMessage _ _ = return ()

--- DM Voting ---
-- TODO: Small race condition here involving the vote ending. Shouldn't be a problem though.
addReactToVote :: GameState -> ReactionInfo -> BotM ()
addReactToVote g@(votes, _) reactInfo = do
  playerCount <- length <$> players <$> getConfig
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
    if (M.size . M.filter (/= []) $ responses (newVotes M.! voteid)) == playerCount
      then (case endCondition (newVotes M.! voteid) of
              TimeUp _ -> return ()
              _ ->  endVote votes voteid)
      else return ()

removeReactFromVote :: GameState -> ReactionInfo -> BotM ()
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
