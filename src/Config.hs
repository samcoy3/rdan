module Config where

import qualified Data.Map as M
import Data.Text

import Discord.Types

-- The Discord bot token
botToken :: Text
botToken = undefined

-- The channel in which the rules are kept
rulesChannel :: ChannelId
rulesChannel = undefined

-- The channel in which the motions are kept
motionsChannel :: ChannelId
motionsChannel = undefined

-- The list of UserIds to be polled for votes
players :: [UserId]
players = undefined

-- The names of the players to be displayed in vote results and scores
playerNames :: M.Map UserId String
playerNames = undefined

-- The scores of the players on bot startup
scores :: M.Map UserId Int
scores = undefined
