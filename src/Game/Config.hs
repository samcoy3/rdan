module Game.Config where

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Text hiding (find)

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))
import Data.Yaml.Aeson (withScientific)

import Data.Foldable

import Discord.Types

data Player = Player
  { playerName :: Text,
    playerId :: UserId
  }
  deriving (Eq, Show)

data Config = Config
  { botToken :: Text,
    rulesChannel :: ChannelId,
    motionsChannel :: ChannelId,
    votePollFrequency :: Int,
    voteReminders :: Bool,
    voteReminderInterval :: Int,
    defaultReacts :: [Text],
    players :: [Player]
  }
  deriving (Eq, Show)

newtype BetterSnowflake = BetterSnowflake {convert :: Snowflake}

instance FromJSON BetterSnowflake where
  parseJSON = withScientific "Snowflake" (return . BetterSnowflake . Snowflake . floor . realToFrac)

instance FromJSON Player where
  parseJSON (Y.Object c) =
    Player <$>
    c .: "name" <*>
    c .: "id"
  parseJSON _ = fail "Failed to parse player"

instance FromJSON Config where
  parseJSON (Y.Object c) =
    Config <$>
    c .: "bot-token" <*>
    c .: "rules-channel" <*>
    c .: "motions-channel" <*>
    c .:? "vote-poll-frequency" .!= 15 <*>
    c .:? "vote-reminders" .!= False <*>
    c .:? "vote-reminder-interval" .!= 10 <*>
    c .:? "default-voting-reactions" .!= [] <*>
    c .: "players"
  parseJSON _ = fail "Failed to parse config"

getPlayerNames :: Config -> [Text]
getPlayerNames = fmap playerName <$> players

getPlayerIDs :: Config -> [UserId]
getPlayerIDs = fmap playerId <$> players

getPlayerNameFromID :: Config -> UserId -> Text
getPlayerNameFromID config uid =
  maybe "Player not found" playerName $
    find (\p -> playerId p == uid) (players config)

getPlayerIDFromName :: Config -> Text -> Maybe UserId
getPlayerIDFromName config name=
  playerId <$> find (\p -> playerName p == name) (players config)
