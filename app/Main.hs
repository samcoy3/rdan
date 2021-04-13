{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Game
import Handlers
import Commands
import CommandParser

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Except
import Discord
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO

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
