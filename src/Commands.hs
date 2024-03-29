module Commands where

import Data.Either
import Data.Function (on, (&))
import Data.Foldable (foldl')
import Data.Maybe
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
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
import qualified Data.Vector as Vec
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

type Time = Either NominalDiffTime AbsoluteTime
fixTime :: Time -> BotM UTCTime
fixTime t = liftIO $ case t of
  Left diffTime -> getCurrentTime >>= (return . addUTCTime diffTime)
  Right absTime -> utcFromHoursMinutesDayOffset
    (hours absTime, minutes absTime, dayOffset absTime)

data VoteTargets = AllVotes | VoteList (NonEmpty VoteId) deriving (Show, Eq)

getTargetVotes :: VoteTargets -> BotM [VoteId]
getTargetVotes vs = case vs of
  AllVotes -> votes <$> getGameState >>= return . M.keys
  VoteList votes -> return . NE.toList $ votes

handleNoVotes :: [VoteId] -> Message -> BotM () -> BotM ()
handleNoVotes vs m action =
  if null vs
    then sendMessage (messageChannel m) "There are no ongoing votes!"
    else action

data VoteAction =
  NewVote (EndCondition Time) Text
  | EditVoteTime VoteTargets (EndCondition Time)
  | EditVoteSubject VoteTargets Text
  | VoteStatus VoteTargets
  | EndVote VoteTargets
  deriving (Show, Eq)
data ArticleAction =
  NewArticle ArticleType (Maybe Int) Text
  | FetchArticle ArticleType Int
  | FetchArticleInline (NonEmpty (ArticleType, Int))
  | EditArticle ArticleType Int Text
  | RepealArticle ArticleType Int
  | DeleteArticle ArticleType Int
  deriving (Show, Eq)
data Command =
  Help (Maybe Text)
  | PrintScores
  | AddToScore (NonEmpty (Either UserId Text, Int))
  | Roll Int Int
  | Flip [Text]
  | Shuffle [Text]
  | VoteCommand VoteAction
  | ArticleCommand ArticleAction
  | BadCommand
  deriving (Show, Eq)

--- COMMAND ACTION ---

enactCommand :: Message -> Command -> BotM ()

-------- HELP --------
enactCommand m (Help s) = do
  let voteTargetHelp = "You can specify the target of a vote command in several ways. `#1` will target the vote with the ID 1, if such a vote exists. `#1,#2` will similarly target both votes 1 and 2. Finally, `all` will target all currently running votes.\n"
  let voteEndConditionHelp = "The \"end condition\" of a vote is constructed as follows:\n"
        <> "- Firstly, if there is a `!` at the beginning of the end condition then the vote will not end prematurely when all votes are in.\n"
        <> "- Next, optional time constraints can be specified. To specify a period of time, use the syntax `XXdYYhZZm`, where `XX`, `YY`, and `ZZ` are numbers denoting how many days, minutes, and seconds the vote is to last. Parts of this can be omitted, e.g. `YYhZZm`. Alternatively, you can specify the precise time the vote is to end using `HH:MM+D` syntax, where `HH` is the hours (in the 24 hour clock), `MM` is the minutes, and `DD` is the offset in days from the next instance of that time.\n"
  let articleGeneral = "Usage: `!rule/motion <n>`. Retrieves rule <n> or motion <n> respectively. Can also be called inline with `!r<n>`/`!m<n>`.\n"
        <> "There are several other commands that start with `!rule` or `!motion`:\n"
        <> "- `!rule/motion new`\n"
        <> "- `!rule/motion edit`\n"
        <> "- `!rule/motion repeal`\n"
        <> "- `!rule/motion delete`\n"
        <> "Type `!help` before any of these commands to learn more about them (e.g. `!help motion new`)."
  let articleNew = "Usage: `!rule/motion new [number]\\n<rule_or_motion_text>`.\n"
        <> "Posts a new rule or motion to the respective channel.\n"
        <> "You can optionally specify the number that the rule or motion should have. Alternatively, you can omit it, and the number above the current highest is selected.\n"
        <> "After the optional number, add a newline and then put the body of the rule or motion that you wish to post."
  let articleEdit = "Usage: `!rule/motion edit <number>\\n<rule_or_motion_text>`.\n"
        <> "Edits the specified rule or motion.\n"
        <> "After the number of the rule or motion, add a newline and then put the body of the rule or motion that you wish to post."
  let articleRepeal = "Usage: `!rule/motion repeal <number>`.\n"
        <> "Repeals the specified rule or motion. The rule or motion will be displayed struck-through.\n"
        <> "If used on a rule or motion which is already repealed, this command will un-repeal it."
  let articleDelete = "Usage: `!rule/motion delete <number>`\n"
        <> "Deletes the specified rule or motion. This removes it from the bot's internal state and deletes the message in the channel. This command is not reversible."

  sendMessage (messageChannel m)
    (case s of
      Nothing -> "I'm rdan, the robot delightfully aiding Nomic.\n"
        <> "To view help on a specific command, type `!help` followed by the command you want to learn about. For example: `!help scores` to learn more about the `!scores` command.\n"
        <> "The list of commands available are as follows: `help`, `scores`, `addscore`, `rule/motion`, `rule/motion new`, `rule/motion edit`, `rule/motion repeal`, `rule/motion delete`, `roll`, `vote new`, `vote edit time`, `vote edit subject`, `vote status`, `vote end`."
      Just "help" -> "To view help on a specific command, type `!help` followed by the command you want to learn about. For example: `!help scores` to learn more about the `!scores` command.\n"
        <> "The list of commands available are as follows: `help`, `scores`, `addscore`, `rule`, `motion`, `newote`, `roll`, `votestatus`, `endvote`."
      Just "scores" -> "Usage: `!scores`\n"
        <> "Displays the current scores of all the players."
      Just "addscore" -> "Usage: `!addscore <player> <delta> [<player> <delta>...]`\n"
        <> "Changes `<player>`'s score by `<delta>`. You must tag `<player` or type their exact name to use this command. You can specify multiple players and changes. For example: `!addscore @Alice 3 Bob -2` will increase Alice's score by 3 and decrease Bob's by 2."
      Just "rule" -> articleGeneral
      Just "motion" -> articleGeneral
      Just "rule new" -> articleNew
      Just "motion new" -> articleNew
      Just "rule edit" -> articleEdit
      Just "motion edit" -> articleEdit
      Just "rule repeal" -> articleRepeal
      Just "motion repeal" -> articleRepeal
      Just "rule delete" -> articleDelete
      Just "motion delete" -> articleDelete
      Just "roll" -> "Usage: `!roll <x>d<y>`\n"
        <> "Rolls a d`<y>`dice `<x>` times and displays the results."
      Just "flip" -> "Usage: `!flip <thing1> <thing2>...`\n"
        <> "Chooses one of the arguments provided to it at random. You can quote a thing in order to include a multi-word item: `!flip Apples \"Leonard Cohen\"` will choose between Apples and Leonard Cohen.\n"
        <> "If no arguments are provided, the list of players is used instead."
      Just "shuffle" -> "Usage: `!shuffle <thing1> <thing2>...`\n"
        <> "Orders the arguments provided to it at random. You can quote a thing in order to include a multi-word item: `!shuffle Apples \"Leonard Cohen\"` will randomly order Apples and Leonard Cohen.\n"
        <> "If no arguments are provided, the list of players is used instead."
      Just "vote" -> "There are several commands that start with `!vote`:\n"
        <> "- `!vote new`\n"
        <> "- `!vote edit subject`\n"
        <> "- `!vote edit time`\n"
        <> "- `!vote end`\n"
        <> "- `!vote status`\n"
        <> "Type `!help` before any of these commands to learn more about them (e.g. `!help vote end`)."
      Just "vote new" -> "Usage: `!vote new <end_condition> <purpose...`\n"
        <> "Starts a new vote on the subject of `<purpose>`.\n"
        <> voteEndConditionHelp
        <> "\nExamples:\n "
        <> "- `!vote new Apples` will start a new vote on the subject of Apples. The vote will only end when everyone has voted.\n"
        <> "- `!vote new 24h Derek` will start a new vote on the subject of Derek. The vote lasts 24 hours, but can end early (if everyone votes before then).\n"
        <> "- `!vote new !18:00+1 Snakes` will start a new vote on the subject of Snakes. The vote will end at the 18:00 after next (if this vote was proposed on Tuesday at 1700, this would be Wednesday at 1800; if this vote was proposed on Tuesday at 1900, this would be Thursday at 1900). The vote may not end early, even if everyone has voted."
      Just "vote status" -> "Usage: `!vote status <targets>`\n"
        <> "Queries the status of the ongoing votes. Will show how many people have voted, and display the subject of the vote.\n"
        <> voteTargetHelp
      Just "vote end" -> "Usage: `!vote end <targets>`\n"
        <> "Prematurely ends the vote(s) with the specified ID(s).\n"
        <> "Note that a vote will end automatically once every player has voted or the time has been reached anyway; you should use this command only if you want to end the vote prior to everyone having voted.\n"
        <> voteTargetHelp
      Just "vote edit time" -> "Usage: `!vote edit time <targets> <end_condition>`\n"
        <> "Edits the time constraints of the target votes.\n"
        <> voteTargetHelp
        <> voteEndConditionHelp
        <> "Note that if you omit the end condition then it will edit the target votes so that they end only when all votes are received (i.e. the time constraint will be removed)."
      Just "vote edit subject" -> "Usage: `!vote edit subject <targets> <subject>`\n"
        <> "Edits the subject of the target votes.\n"
        <> voteTargetHelp
      )

-------- SCORES --------
enactCommand m PrintScores = printScores m

enactCommand m (AddToScore cs) = do
  config <- getConfig
  deltas <- forM cs $ \(target, delta) -> do
    scores <- scores <$> getGameState
    player <- case target of
      Left uid -> return $ Right uid
      Right name -> do
        config <- getConfig
        case getPlayerIDFromName config name of
          Nothing -> return $ Left name
          Just uid -> return $ Right uid
    case player of
      Left text -> do
        sendMessage (messageChannel m)
          $ "Could not find player **" <> text <> "**."
        return Nothing
      Right uid -> do
        let oldScore = scores M.! uid
        modifyScore uid delta
        return $ Just (getPlayerNameFromID config uid, oldScore, oldScore + delta)
  let successes = catMaybes (NE.toList deltas)
  if null successes
    then return ()
    else sendMessage (messageChannel m)
      $ printDeltas (NE.fromList successes)

-------- ROLLING --------
enactCommand m (Roll quant sides) = do
  rolls <- replicateM quant . liftIO $ randomRIO (1, sides)
  sendMessage (messageChannel m) $
    (T.pack . show . sum $ rolls) <>
    " ⟵ " <>
    (T.pack . show $ rolls)

enactCommand m (Flip options) = do
  options <- if null options then getPlayerNames <$> getConfig else return options
  selection <- liftIO $ randomRIO (0, length options - 1)
  sendMessage (messageChannel m) $
    options !! selection

enactCommand m (Shuffle options) = do
  options <- if null options then getPlayerNames <$> getConfig else return options
  let optionVector = Vec.fromList options
  -- Create a sequence of Fischer-Yates swaps
  swaps <-
    fmap (\(a, b) vec -> vec Vec.// [(a, vec Vec.! b), (b, vec Vec.! a)]) <$>
    zip [0..] <$>
    (liftIO . sequence $
     (\lower -> randomRIO (lower, Vec.length optionVector - 1))
     <$> [0..Vec.length optionVector - 2])
  let shuffledVector = foldl' (&) optionVector swaps
  sendMessage (messageChannel m)
    $ T.intercalate "; " (Vec.toList shuffledVector)

-------- VOTING --------
-- TODO: This handles failure to establish message channels very poorly.
enactCommand m (VoteCommand (NewVote endCon purpose')) = do
  playerIDs <- getPlayerIDs <$> getConfig
  reacts <- defaultReacts <$> getConfig
  currentVotes <- votes <$> getGameState
  endCon' <- traverse fixTime endCon

  -- Getting the DM channels for the users who need to be messaged
  userHandles <- getConfig >>= getDMs . getPlayerIDs
  -- Sending the vote messages to the users (keeping the channels attached, for now)
  messageHandles <- zip userHandles <$>
    forM userHandles
      ((\c -> lift . restCall
      $ R.CreateMessage c
      $ userDMDescription purpose' endCon') . channelId)

  -- "Pre-reacting" to the messages using the reacts specified in the config.
  forM_ reacts $ \r ->
    forM_ messageHandles $ \(u, m') ->
      case m' of
        Left _ -> return ()
        Right m ->
          void . lift . restCall
          $ R.CreateReaction (channelId u, messageId m) r

  let newid = (+1) $ max 0 (if null (M.keys currentVotes)
                            then 0
                            else maximum . M.keys $ currentVotes)

  sendMessage (messageChannel m) $ "A new vote has been started, and players have been notified. The vote ID is #" <> (T.pack . show) newid
  modifyVotes
    (M.insert newid (Vote { messages = M.fromList $ zip (map messageId $ rights . fmap snd $ messageHandles) playerIDs
                          , responses = M.fromList $ zip playerIDs $ repeat []
                          , purpose = purpose'
                          , announceChannel = messageChannel m
                          , lastChecked = Nothing
                          , endCondition = endCon'}))

enactCommand m (VoteCommand (EditVoteTime voteids endCon)) = do
  vs <- getTargetVotes voteids
  handleNoVotes vs m $ do
    endCon' <- traverse fixTime endCon
    voteMap <- votes <$> getGameState
    forM_ vs $ \v -> do
      if v `notElem` M.keys voteMap
        then sendMessage (messageChannel m)
            $ "Could not find vote #" <> (T.pack . show $ v) <> "!"
        else forM_ (M.toList . messages $ voteMap M.! v) $ \(message, user) -> do
        channel' <- lift . restCall . R.CreateDM $ user
        case channel' of
          Left _ -> return ()
          Right channel -> editMessage (channelId channel) message
            $ userDMDescription (purpose $ voteMap M.! v) endCon'
      modifyVotes (flip M.adjust v
                    (\vote -> vote {endCondition = endCon'}))
    sendMessage (messageChannel m) "End conditions for votes updated."

enactCommand m (VoteCommand (EditVoteSubject voteids newSubject)) = do
  vs <- getTargetVotes voteids
  handleNoVotes vs m $ do
    voteMap <- votes <$> getGameState
    forM_ vs $ \v -> do
      if v `notElem` M.keys voteMap
        then sendMessage (messageChannel m)
            $ "Could not find vote #" <> (T.pack . show $ v) <> "!"
        else forM_ (M.toList . messages $ voteMap M.! v) $ \(message, user) -> do
        channel' <- lift . restCall . R.CreateDM $ user
        case channel' of
          Left _ -> return ()
          Right channel -> editMessage (channelId channel) message
            $ userDMDescription (newSubject) (endCondition $ voteMap M.! v)
      modifyVotes (flip M.adjust v
                    (\vote -> vote {purpose = newSubject}))
    sendMessage (messageChannel m) "Subject for votes updated."

enactCommand m (VoteCommand (VoteStatus voteids)) = do
  endAllAutomatically <- endAllVotesWhenAllVoted <$> getConfig
  let endAllAutomaticallyNotice = if endAllAutomatically
        then "\n*The setting `end-all-votes-automatically` is enabled. When all players have voted on all concurrent votes, the votes will all end regardless of the above end conditions.*"
        else ""
  playerCount <- length . players <$> getConfig
  currentVotes <- votes <$> getGameState
  vs <- getTargetVotes voteids
  handleNoVotes vs m $ do
    case voteids of
      AllVotes -> sendMessage (messageChannel m) $
                  "The current active votes are as follows:\n" <>
                  (T.unlines ["**Vote #" <> (T.pack . show) vid <> "**: " <> publicDescription playerCount vote | (vid, vote) <- M.toList currentVotes]) <>
                  endAllAutomaticallyNotice
      VoteList (voteids) -> sendMessage (messageChannel m) $
                      T.unlines $ (flip map $ NE.toList voteids)
                      (\v -> if v `elem` (M.keys currentVotes)
                                then "**Vote #" <> (T.pack . show) v <> "**: " <> publicDescription playerCount (currentVotes M.! v)
                                else "There is no vote with the vote ID #" <> (T.pack . show) v)

enactCommand m (VoteCommand (EndVote vs')) = do
  vs <- getTargetVotes vs'
  forM_ vs $ \vote -> do
    currentVotes <- votes <$> getGameState
    if vote `elem` M.keys currentVotes
      then endVote vote
      else sendMessage (messageChannel m) $
          "There is no vote with the vote ID #" <> (T.pack . show) vote

enactCommand m BadCommand =
  void . lift . restCall
    $ R.CreateReaction (messageChannel m, messageId m) "question"

--- ARTICLES ---
enactCommand m (ArticleCommand (NewArticle atype number abody)) = do
  channel <- getArticleChannel atype
  currentArticles <- articles <$> getGameState
  let highestNumber =
        fromMaybe 0
        . fmap (snd . fst)
        . M.lookupMax
        . M.filterWithKey (\(t, n) v -> t == atype)
        $ currentArticles
  let number' = fromMaybe (highestNumber + 1) number
  let existingArticle = currentArticles M.!? (atype, number')
  if isJust existingArticle
    then sendMessage (messageChannel m)
         $ "Cannot post " <> (T.toLower . T.pack . show) atype <> ": one already exists with the same type and number."
    else do
    amessage <- lift . restCall
      $ R.CreateMessage channel (prettyPrintFromText atype number' abody)
    case amessage of
      Left e -> liftIO $ putStrLn $ show e
      Right amessage' -> do
        modifyArticles
          (M.insert (atype, number')
          Article {
              body = abody,
              message = (messageId amessage'),
              repealed = False})
        sendMessage (messageChannel m)
          $ (printArticleName atype number') <> " posted!"

enactCommand m (ArticleCommand (FetchArticle atype number)) = fetchArticle (messageChannel m) (atype, number)

enactCommand m (ArticleCommand (FetchArticleInline fetches)) = forM_ fetches $ \(atype, number) ->
  fetchArticle (messageChannel m) (atype, number)

enactCommand m (ArticleCommand (EditArticle atype number newbody)) =
  modifyArticle m (atype, number)
    (\a -> a {body = newbody})
    (\a -> printArticleName atype number
      <> " edited! Was previously:\n"
      <> body a)

enactCommand m (ArticleCommand (RepealArticle atype number)) =
  modifyArticle m (atype, number)
    (\a -> a {repealed = not $ repealed a})
    (\a -> printArticleName atype number <> (if repealed a then " unrepealed!" else " repealed!"))

enactCommand m (ArticleCommand (DeleteArticle atype number)) = do
  articleChannel <- getArticleChannel atype
  currentArticles <- articles <$> getGameState
  let existingArticle = currentArticles M.!? (atype, number)
  case existingArticle of
    Nothing -> sendMessage (messageChannel m)
      $ "Couldn't find " <> printArticleName atype number <> "!"
    Just article -> do
      void . lift . restCall $ R.DeleteMessage (articleChannel, message article)
      modifyArticles (M.delete (atype, number))
      sendMessage (messageChannel m)
        $ printArticleName atype number <> " deleted!"

--- MISC ---
printScores :: Message -> BotM ()
printScores m = do
  config <- getConfig
  currentScores <- scores <$> getGameState
  let scoreText = "The current scores are:\n"
        <> T.unlines ["**"
                      <> getPlayerNameFromID config p
                      <> "**: "
                      <> (T.pack . show) score
                     | (p,score) <- sortBy (compare `on` (negate . snd)) $ M.toList currentScores]
  sendMessage (messageChannel m) scoreText

printDeltas :: NonEmpty (Text, Int, Int) -> Text
printDeltas deltas =
  let deltaTexts = fmap
        (\(player, old, new) -> "**" <> player <> "**: " <> (T.pack . show) old <> " ⟶ " <> (T.pack . show) new)
        deltas
  in "Scores updated ("
        <> T.intercalate ", " (NE.toList deltaTexts)
        <> ")."

fetchArticle :: ChannelId -> (ArticleType, Int) -> BotM ()
fetchArticle channel query@(atype, number) = do
  article <- (M.!? query) . articles <$> getGameState
  sendMessage channel $
    case article of
      Nothing -> "Couldn't find " <> printArticleName atype number <> "!"
      Just article -> prettyPrintFromArticle atype number article

modifyArticle :: Message -> (ArticleType, Int) -> (Article -> Article) -> (Article -> Text) -> BotM ()
modifyArticle m (atype, number) mod onSuccess = do
  articleChannel <- getArticleChannel atype
  currentArticles <- articles <$> getGameState
  let existingArticle = currentArticles M.!? (atype, number)
  case existingArticle of
    Nothing -> sendMessage (messageChannel m)
      $ "Couldn't find " <> printArticleName atype number <> "!"
    Just article -> do
      let newarticle = mod article
      let messageText = onSuccess article
      editMessage articleChannel (message newarticle) (prettyPrintFromArticle atype number newarticle)
      modifyArticles
        (M.adjust (const newarticle) (atype, number))
      sendMessage (messageChannel m) messageText

getDMs :: [UserId] -> BotM [Channel]
getDMs users = traverse (lift . restCall . R.CreateDM) users >>= return . rights
