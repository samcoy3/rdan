module Game.GameState where

import Game.Config
import Game.Vote

import qualified Data.Map as M
import Data.Map (Map)

import Control.Monad.Reader

import Discord.Types

data GameState = GameState
  { scores :: Map UserId Int,
    votes :: Map VoteId Vote
  } deriving (Show)

newGameState :: Config -> GameState
newGameState config =
  GameState
    (M.fromList $ zip (getPlayerIDs config) (repeat 0))
    M.empty
