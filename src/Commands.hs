module Commands where

import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.IO.Class
import Control.Concurrent.STM.TVar
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Random
import Text.Read hiding (lift)

import Game

import Discord
import Discord.Types
import qualified Discord.Requests as R

type ScoreMap = M.Map UserId Int

--- Command type ---

data AbsoluteTime = AbsTime { hours :: Int
                            , minutes :: Int
                            , dayOffset :: Integer } deriving (Show, Eq)
data Retrievable = Rule Int | Motion Int deriving (Show, Eq)
data Command =
  Help (Maybe Text)
  | PrintScores
  | AddToScore [(Either UserId Text, Int)]
  | Find Retrievable
  | FindInline [Retrievable]
  | Roll Int Int
  | NewVote (EndCondition (Either NominalDiffTime AbsoluteTime)) Text
  | VoteStatus (Maybe [VoteId])
  | EndVote [VoteId]
  deriving (Show, Eq)

--- COMMAND ACTION ---

enactCommand :: Message -> Command -> BotM ()

enactCommand m (Help s) = sendMessage (messageChannel m)
  (case s of
    Nothing -> "I'm rdan, the robot delightfully aiding Nomic.\n"
               <> "To view help on a specific command, type `!help` followed by the command you want to learn about. For example: `!help scores` to learn more about the `!scores` command.\n"
               <> "The list of commands available are as follows: `help`, `scores`, `addscore`, `rule`, `motion`, `newote`, `roll`, `votestatus`, `endvote`."
    Just "help" -> "To view help on a specific command, type `!help` followed by the command you want to learn about. For example: `!help scores` to learn more about the `!scores` command.\n"
               <> "The list of commands available are as follows: `help`, `scores`, `addscore`, `rule`, `motion`, `newote`, `roll`, `votestatus`, `endvote`."
    Just "scores" -> "Usage: `!scores`\n"
                     <> "Displays the current scores of all the players."
    Just "addscore" -> "Usage: `!addscore <player> <delta> [<player> <delta>...]`\n"
                       <> "Changes `<player>`'s score by `<delta>`. You must tag `<player` or type their exact name to use this command. You can specify multiple players and changes. For example: `!addscore @Alice 3 Bob -2` will increase Alice's score by 3 and decrease Bob's by 2."
    Just "rule" -> "Usage: `!rule <rule_number>`\n"
                   <> "Retrieves the rule numbered `<rule_number>`. You can also call this inline with `!r<rule_number>`."
    Just "motion" -> "Usage: `!motion <motion_number>`\n"
                     <> "Retrieves the motion numbered `<motion_number>`. You can also call this inline with `!m<motion_number>`."
    Just "roll" -> "Usage: `!roll <x>d<y>`\n"
                   <> "Rolls a d`<y>`dice `<x>` times and displays the results."
    Just "newvote" -> "Usage: `!newvote <end_condition> <purpose...`\n"
                      <> "Starts a new vote on the subject of `<purpose>`.\n"
                      <> "`end_condition` determines how the vote ends. It is constructed as follows:\n"
                      <> "- Firstly, if there is a `!` at the beginning of `end_condition` then the vote will not end when all votes are in.\n"
                      <> "- Next, optional time constraints can be specified. To specify a period of time, use the syntax `XXdYYhZZm`, where `XX`, `YY`, and `ZZ` are numbers denoting how many days, minutes, and seconds the vote is to last. Parts of this can be omitted, e.g. `YYhZZm`. Alternatively, you can specify the precise time the vote is to end using `HH:MM+D` syntax, where `HH` is the hours (in the 24 hour clock), `MM` is the minutes, and `DD` is the offset in days from the next instance of that time.\n"
                      <> "\nExamples:\n "
                      <> "- `!newvote Apples` will start a new vote on the subject of Apples. The vote will only end when everyone has voted.\n"
                      <> "- `!newvote 24h Derek` will start a new vote on the subject of Derek. The vote lasts 24 hours, but can end early (if everyone votes before then).\n"
                      <> "- `!newvote !18:00+1 Snakes` will start a new vote on the subject of Snakes. The vote will end at the 18:00 after next (if this vote was proposed on Tuesday at 1700, this would be Wednesday at 1800; if this vote was proposed on Tuesday at 1900, this would be Thursday at 1900). The vote may not end early, even if everyone has voted."
    Just "votestatus" -> "Usage: `!votestatus [#XX #YY...`\n"
                         <> "Queries the status of the ongoing votes. Will show how many people have voted, and display the subject of the vote.\n"
                         <> "You can optionally supply one or several vote IDs in order to restrict your query to those votes, e.g. `!votestatus #23`."
    Just "endvote" -> "Usage: `!endvote #XX #YY ...`\n"
                      <> "Prematurely ends the vote(s) with the specified ID(s).\n"
                      <> "Note that a vote will end automatically once every player has voted or the time has been reached anyway; you should use this command only if you want to end the vote prior to everyone having voted."
    )

enactCommand m PrintScores = printScores m

enactCommand m (AddToScore cs) = do
  forM_ cs $ \(target, delta) -> do
    scores <- scores <$> getGameState
    player <- case target of
      Left uid -> return $ Right uid
      Right name -> do
        config <- getConfig
        case getPlayerIDFromName config name of
          Nothing -> return $ Left name
          Just uid -> return $ Right uid
    case player of
      Left text ->
        sendMessage (messageChannel m) $
        "Could not find player **" <> text <> "**."
      Right uid ->
        modifyScore uid delta
  sendMessage (messageChannel m) "Scores updated."
  printScores m

enactCommand m (Find x) = do
  result <- getRetrieveable x
  sendMessage  (messageChannel m) $
    (case result of
       Left retr -> (T.pack . show $ retr) <> " not found, sorry!"
       Right ruleText -> T.unlines . tail . T.lines $ ruleText
    )

enactCommand m (FindInline xs) = do
  results <- forM xs $ getRetrieveable
  sendMessage  (messageChannel m ) $
    T.unlines $
    map (\r -> case r of
            Left retr -> "Could not find " <> (T.pack . show $ retr) <> "."
            Right ruleText -> ruleText
        ) results

enactCommand m (Roll quant sides) = do
  rolls <- replicateM quant . liftIO $ randomRIO (1, sides)
  sendMessage (messageChannel m) $
    (T.pack . show . sum $ rolls) <>
    " ⟵ " <>
    (T.pack . show $ rolls)

-- TODO: This handles failure to establish message channels very poorly.
enactCommand m (NewVote endCon purpose') = do
  playerIDs <- getPlayerIDs <$> getConfig
  currentVotes <- votes <$> getGameState
  let fixTime t = case t of
        Left diffTime -> getCurrentTime >>= (return . addUTCTime diffTime)
        Right absTime -> utcFromHoursMinutesDayOffset
          (hours absTime, minutes absTime, dayOffset absTime)
  endCon' <- liftIO $ traverse fixTime endCon

  userHandles <- getConfig >>= getDMs . getPlayerIDs
  messageHandles <- mapM ((\c -> lift . restCall
                            $ R.CreateMessage c
                            $ "A new vote has begun on the subject of **"
                            <> purpose'
                            <> "**! "
                            <> endConditionDescription endCon'
                            <> " Please react to this message with a tick or a cross in order to vote:")
                           . channelId) userHandles

  let newid = (+1) $ max 0 (if null (M.keys currentVotes)
                            then 0
                            else maximum . M.keys $ currentVotes)

  sendMessage (messageChannel m) $ "A new vote has been started, and players have been notified. The vote ID is #" <> (T.pack . show) newid
  modifyVotes
    (M.insert newid (Vote { messages = M.fromList $ zip (map messageId $ rights messageHandles) playerIDs
                          , responses = M.fromList $ zip playerIDs $ repeat []
                          , purpose = purpose'
                          , announceChannel = messageChannel m
                          , endCondition = endCon'}))

enactCommand m (VoteStatus voteids) = do
  playerCount <- length . players <$> getConfig
  currentVotes <- votes <$> getGameState
  case voteids of
    Nothing -> sendMessage (messageChannel m) $
               if M.null currentVotes
               then "There are no votes ongoing."
               else "The current active votes are as follows:\n" <>
                    (T.unlines ["**Vote #" <> (T.pack . show) vid <> "**: " <> describe playerCount vote | (vid, vote) <- M.toList currentVotes])
    Just voteids -> sendMessage (messageChannel m) $
                    T.unlines $ (flip map voteids)
                    (\v -> if v `elem` (M.keys currentVotes)
                              then "**Vote #" <> (T.pack . show) v <> "**: " <> describe playerCount (currentVotes M.! v)
                              else "There is no vote with the vote ID #" <> (T.pack . show) v)

enactCommand m (EndVote vs) = forM_ vs $ \vote -> do
  currentVotes <- votes <$> getGameState
  if vote `elem` M.keys currentVotes
    then endVote vote
    else sendMessage (messageChannel m) $
         "There is no vote with the vote ID #" <> (T.pack . show) vote

--- MISC ---

printScores :: Message -> BotM ()
printScores m = do
  config <- getConfig
  currentScores <- scores <$> getGameState
  let scoreText = "The current scores are:\n"
        `T.append` T.unlines [(getPlayerNameFromID config p) <> ": " <> (T.pack . show) score | (p,score) <- M.toList currentScores]
  sendMessage (messageChannel m) scoreText

getRetrieveable :: Retrievable -> BotM (Either Retrievable Text)
getRetrieveable retr =
  do
    channel <- case retr of
      Rule _ -> rulesChannel <$> getConfig
      Motion _ -> motionsChannel <$> getConfig
    messages <- lift . restCall $
                R.GetChannelMessages channel (100, R.LatestMessages)
    let validRules = filter
                     (\t -> T.pack ("**" ++ show retr ++ "**") `T.isPrefixOf` t)
                     (map messageText $ fromRight [] messages)
    return $
      if null validRules
      then Left retr
      else Right (head validRules)

getDMs :: [UserId] -> BotM [Channel]
getDMs users = traverse (lift . restCall . R.CreateDM) users >>= return . rights
