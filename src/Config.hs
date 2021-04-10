module Config where

import Types

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Text hiding (find)

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Data.Yaml.Aeson (withScientific)

import Data.Foldable

import Discord.Types

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

getConfig :: BotM Config
getConfig = asks fst

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
