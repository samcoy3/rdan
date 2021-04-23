{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Game.Article where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)

import Discord
import Discord.Types

import Data.Aeson.Types
import Data.Yaml

import GHC.Generics

data ArticleType = Rule | Motion
  deriving (Eq, Ord, Show, Generic)

data Article = Article { body :: Text
                       , message :: MessageId
                       , repealed :: Bool }
               deriving (Show, Generic)

type Articles = Map (ArticleType, Int) Article

instance ToJSONKey ArticleType
instance FromJSONKey ArticleType
instance ToJSON ArticleType
instance FromJSON ArticleType
instance ToJSON Article
instance FromJSON Article

prettyPrint :: ArticleType -> Int -> Text -> Text
prettyPrint atype number text =
  "**"
  <> (T.pack . show) atype
  <> " "
  <> (T.pack . show) number
  <> ":**\n"
  <> text
