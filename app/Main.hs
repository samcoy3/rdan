{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Game
import Commands
import CommandParser

import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Concurrent.STM.TVar

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Attoparsec.Text as A

import Data.Yaml (decodeFileEither, prettyPrintParseException)

main :: IO ()
main = do
  startup <- runExceptT readFiles
  case startup of
    Left e -> TIO.putStrLn e
    Right (config, gameState') -> do
      gameState <- newTVarIO gameState'
      TIO.putStrLn (T.pack . show $ config)
      TIO.putStrLn (T.pack . show $ gameState')
      userReadableError <- runDiscord $ def
                      { discordToken = botToken config
                      , discordOnEvent = \event -> runReaderT (handleEvent event) (config, gameState)
                      , discordOnStart = (do
                          discordHandle <- ask
                          liftIO . forkIO $ runReaderT (runReaderT votePoller (config, gameState)) discordHandle
                          return ())}
      TIO.putStrLn userReadableError

  return ()

readFiles :: ExceptT Text IO (Config, GameState)
readFiles = do
  config <-
    withExceptT (T.pack . prettyPrintParseException) $
      ExceptT (decodeFileEither "config.yaml")
  gameState <- catchError (withExceptT (T.pack . prettyPrintParseException) . ExceptT $ decodeFileEither "game_state.yaml") $
    \_ -> do
      liftIO $ TIO.putStrLn "Couldn't read game state. Would you like to start a new game? (Y/N)"
      response <- liftIO TIO.getLine
      if response == "y" || response == "Y"
        then return $ newGameState config
        else throwError "Aborting..."
  return (config, gameState)

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
