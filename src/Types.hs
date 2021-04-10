module Types where

import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Discord
import Discord.Types
import Data.Text

import qualified Data.Map as M
import Data.Map (Map)

-------- CONFIG --------
data Player = Player
  { playerName :: Text,
    playerId :: UserId
  }
  deriving (Eq, Show)

data Config = Config
  { botToken :: Text,
    rulesChannel :: ChannelId,
    motionsChannel :: ChannelId,
    players :: [Player]
  }
  deriving (Eq, Show)

-------- VOTES --------
data EndCondition a =
  AllVoted
  | AllVotedOrTimeUp a
  | TimeUp a
  deriving (Eq, Show)

type VoteId = Int
type Votes = M.Map VoteId Vote
data Vote = Vote { messages :: M.Map MessageId UserId
                 , responses :: M.Map UserId [Text]
                 , purpose :: Text
                 , announceChannel :: ChannelId
                 , endCondition :: EndCondition UTCTime}
            deriving (Show)

-------- Game State --------
data GameState = GameState
  { scores :: Map UserId Int,
    votes :: Map VoteId Vote
  } deriving (Show)

-------- Bot Monad --------
type BotM = ReaderT (Config, TVar GameState) DiscordHandler
