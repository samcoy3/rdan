module Config where

import qualified Data.Map as M
import Data.Text hiding (find)

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
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
    (convert <$> c .: "id")
  parseJSON _ = fail "Failed to parse player"

instance FromJSON Config where
  parseJSON (Y.Object c) =
    Config <$>
    c .: "bot-token" <*>
    (convert <$> c .: "rules-channel") <*>
    (convert <$> c .: "motions-channel") <*>
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
