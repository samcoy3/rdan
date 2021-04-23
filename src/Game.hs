module Game
  ( sendMessage,
    editMessage,
    readTVarDisc,
    modifyTVarDisc,
    getConfig,
    getGameState,
    serialiseGameState,
    modifyScore,
    modifyVotes,
    votePoller,
    endVote,

    Config (..),
    Player (..),
    getPlayerNames,
    getPlayerIDs,
    getPlayerNameFromID,
    getPlayerIDFromName,

    Vote (..),
    VoteId,
    EndCondition (..),
    publicDescription,
    userDMDescription,
    endConditionDescription,
    printTime,
    utcFromHoursMinutesDayOffset,

    GameState (..),
    BotM,
    newGameState
  )
where
import Game.Config
import Game.GameState
import Game.Vote

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Yaml (encodeFile)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Data.Time.Clock
import Control.Monad
import Control.Monad.Reader
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar

type BotM = ReaderT (Config, TVar GameState) DiscordHandler

-------- Utilities --------
sendMessage :: ChannelId -> Text -> BotM ()
sendMessage channel m =
  lift . void . restCall $ R.CreateMessage channel m

editMessage :: ChannelId -> MessageId -> Text -> BotM ()
editMessage channel message newtext =
  lift . void . restCall $ R.EditMessage (channel, message) newtext Nothing

readTVarDisc :: TVar a -> BotM a
readTVarDisc = liftIO . readTVarIO

modifyTVarDisc :: TVar a -> (a -> a)-> BotM ()
modifyTVarDisc var f = liftIO . atomically $ modifyTVar var f

-------- Game State --------
getConfig :: BotM Config
getConfig = asks fst

getGameState :: BotM GameState
getGameState = asks snd >>= readTVarDisc

serialiseGameState :: BotM ()
serialiseGameState = getGameState >>= liftIO . encodeFile "game_state.yaml"

modifyScore :: UserId -> Int -> BotM ()
modifyScore target delta = do
  gameState <- asks snd
  modifyTVarDisc gameState
    (\g -> g {scores = M.update (pure . (+ delta)) target (scores g)})
  serialiseGameState

modifyVotes :: (Map VoteId Vote -> Map VoteId Vote) -> BotM ()
modifyVotes f = do
  gameState <- asks snd
  modifyTVarDisc gameState (\g -> g {votes = f (votes g)})
  serialiseGameState

-------- Vote Polling --------
votePoller :: BotM ()
votePoller = do
  currentTime <- liftIO getCurrentTime
  currentVotes <- votes <$> getGameState
  playerCount <- length . players <$> getConfig
  let votesToEnd = M.keys .
                   M.filter (\v -> case endCondition v of
                                     AllVoted ->
                                       (M.size . M.filter (/= []) $ responses v)
                                       == playerCount
                                     TimeUp t -> currentTime > t
                                     AllVotedOrTimeUp t ->
                                       currentTime > t
                                       ||
                                       (M.size . M.filter (/= []) $ responses v)
                                       == playerCount)
                   $ currentVotes
  mapM_ endVote votesToEnd
  liftIO . threadDelay $ (1000000 * 15 :: Int) -- Sleeps for fiteen seconds
  votePoller

endVote :: Int -> BotM ()
endVote voteid = do
  stateOfVotes <- votes <$> getGameState
  config <- getConfig
  let particularVote = stateOfVotes M.!? voteid
  case particularVote of
    Nothing -> return ()
    Just vote -> do
      modifyVotes (M.delete voteid)
      sendMessage (announceChannel vote) $
        "The vote on **" <>
        purpose vote <>
        "** has concluded. The results are:\n" <>
        T.unlines ["**" <> getPlayerNameFromID config p <> "**: " <> T.unwords e | (p,e) <- M.toList (responses vote)]
