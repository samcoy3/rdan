module Vote where

import Data.Text
import qualified Data.Map as M

import Discord.Types

data Vote =
  NoVote |
  VoteInProgress { responses :: M.Map UserId Text
                 , purpose :: Text
                 , announceChannel :: ChannelId}
          deriving (Eq, Show)
