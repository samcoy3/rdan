module Game
  ( sendMessage,
    editMessage,
    getConfig,
    getGameState,
    serialiseGameState,
    modifyScore,
    modifyVotes,
    modifyArticles,
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

    Article(..),
    ArticleType (..),
    prettyPrintFromText,
    prettyPrintFromArticle,
    printArticleName,
    getArticleChannel,

    GameState (..),
    BotM,
    newGameState
  )
where
import Game.Article
import Game.Config
import Game.GameState
import Game.Vote

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.List ((\\))
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

modifyArticles :: (Articles -> Articles) -> BotM ()
modifyArticles f = do
  gameState <- asks snd
  modifyTVarDisc gameState (\g -> g {articles = f (articles g)})
  serialiseGameState

-------- Articles --------
getArticleChannel :: ArticleType -> BotM ChannelId
getArticleChannel Motion = motionsChannel <$> getConfig
getArticleChannel Rule = rulesChannel <$> getConfig

-------- Vote Polling --------
votePoller :: BotM ()
votePoller = do
  currentTime <- liftIO getCurrentTime
  currentVotes <- votes <$> getGameState
  playerCount <- length . players <$> getConfig
  pollFrequency <- votePollFrequency <$> getConfig
  endAllAutomatically <- endAllVotesWhenAllVoted <$> getConfig
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
  currentVotes <- votes <$> getGameState
  when (all (allPlayersHaveVoted playerCount) currentVotes && endAllAutomatically)
    $ mapM_ endVote (M.keys currentVotes)
  sendReminders
  modifyVotes (fmap (\v -> v {lastChecked = Just currentTime}))
  liftIO . threadDelay $ (1000000 * pollFrequency :: Int) -- Sleeps for fiteen seconds
  votePoller

sendReminders :: BotM ()
sendReminders = do
  currentVotes <- votes <$> getGameState
  currentTime <- liftIO getCurrentTime
  reminders <- voteReminders <$> getConfig
  reminderInterval <- (* 60) . voteReminderInterval <$> getConfig
  if reminders
    then forM_ currentVotes $ \v ->
      case (lastChecked v, getEndTime v) of
        (Just lastChecked, Just endTime) ->
          if nominalDiffTimeToSeconds (diffUTCTime endTime lastChecked) > fromIntegral reminderInterval
             && nominalDiffTimeToSeconds (diffUTCTime endTime currentTime) < fromIntegral reminderInterval
             then let recipients = M.elems (messages v) \\ M.keys (M.filter (not . null) (responses v)) in
                    forM_ recipients $ \r -> do
                      dmChannel <- lift . restCall $ R.CreateDM r
                      case dmChannel of
                        Left _ -> return ()
                        Right chan -> sendMessage (channelId chan)
                          $ "**Vote reminder!**\n"
                          <> "Remember to vote on the vote concerning **"
                          <> (purpose v)
                          <> "**! "
                          <> "The vote ends in "
                          <> (T.pack . show) (reminderInterval `div` 60)
                          <>" minutes."
             else return ()
        _ -> return ()
    else return ()

endVote :: Int -> BotM ()
endVote voteid = do
  stateOfVotes <- votes <$> getGameState
  config <- getConfig
  let particularVote = stateOfVotes M.!? voteid
  case particularVote of
    Nothing -> return ()
    Just vote -> do
      let announceChannel' = fromMaybe (announceChannel vote) (defaultAnnounceChannel config)
      modifyVotes (M.delete voteid)
      sendMessage (announceChannel') $
        "The vote on **" <>
        purpose vote <>
        "** has concluded. The results are:\n" <>
        T.unlines ["**" <> getPlayerNameFromID config p <> "**: " <> T.unwords e | (p,e) <- M.toList (responses vote)]
