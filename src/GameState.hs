module GameState where

import Types
import Vote
import Config
import Util

import qualified Data.Map as M
import Data.Map (Map)

import Control.Monad.Reader

import Discord.Types

newGameState :: Config -> GameState
newGameState config =
  GameState
    (M.fromList $ zip (getPlayerIDs config) (repeat 0))
    M.empty

getGameState :: BotM GameState
getGameState = asks snd >>= readTVarDisc

serialiseGameState :: BotM ()
serialiseGameState = undefined

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
