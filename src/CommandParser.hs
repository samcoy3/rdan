module CommandParser where

import Game
import Commands

import Control.Monad (join)
import Data.Char
import Data.Functor (($>))
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T (unwords)
import Data.List.NonEmpty (NonEmpty, fromList)
import Control.Applicative

import Data.Time.Clock

import Data.Attoparsec.Text as A

import Discord.Types

commandParser :: Parser Command
commandParser = helpParser
                <|> printScoresParser
                <|> addToScoreParser
                <|> rollParser
                <|> flipParser
                <|> shuffleParser
                -- Votes
                <|> newVoteParser
                <|> voteEditTimeParser
                <|> voteEditSubjectParser
                <|> voteStatusParser
                <|> endVoteParser
                -- Articles
                <|> newArticleParser
                <|> fetchArticleParser
                <|> editArticleParser
                <|> repealArticleParser
                <|> deleteArticleParser
                -- These two ought to be last
                <|> fetchArticleInlineParser
                <|> badCommandParser

helpParser :: Parser Command
helpParser = do
  string "!help"
  skipSpace
  fmap Help $ option Nothing $
    Just <$> choice (map string [ "help"
                                 , "scores"
                                 , "addscore"
                                 , "rule new"
                                 , "rule edit"
                                 , "rule repeal"
                                 , "rule delete"
                                 , "rule"
                                 , "motion new"
                                 , "motion edit"
                                 , "motion repeal"
                                 , "motion delete"
                                 , "motion"
                                 , "roll"
                                 , "flip"
                                 , "shuffle"
                                 , "vote new"
                                 , "vote edit subject"
                                 , "vote edit time"
                                 , "vote end"
                                 , "vote status"
                                 , "vote"])

printScoresParser :: Parser Command
printScoresParser = oneWordParser "!scores" PrintScores

addToScoreParser :: Parser Command
addToScoreParser = do
  string "!addscore"
  changes <- many1 $ do
    skipSpace
    target <- eitherP
      parseUserId
      (pack . unwords <$> sepBy1 (many1 letter) space)
    skipSpace
    delta <- signed decimal
    return (target, delta)
  return . AddToScore . fromList $ changes

rollParser :: Parser Command
rollParser = do
  string "!roll"
  skipSpace
  quantity <- decimal
  char 'd'
  sidedness <- decimal
  return $ Roll quantity sidedness

flipParser :: Parser Command
flipParser = do
  string "!flip"
  skipSpace
  Flip <$> option [] parseArguments

shuffleParser :: Parser Command
shuffleParser = do
  string "!shuffle"
  skipSpace
  Shuffle <$> option [] parseArguments

newVoteParser :: Parser Command
newVoteParser = do
  string "!newvote" <|> string "!vote new"
  skipSpace
  endCondition <- parseEndCondition
  skipSpace
  votePurpose <- takeText
  return . VoteCommand $ NewVote endCondition votePurpose

voteEditTimeParser :: Parser Command
voteEditTimeParser = do
  string "!vote edit time"
  skipSpace
  voteIds <- parseTargetVotes
  skipSpace
  endCondition <- parseEndCondition
  return . VoteCommand $ EditVoteTime voteIds endCondition

voteEditSubjectParser :: Parser Command
voteEditSubjectParser = do
  string "!vote edit subject"
  skipSpace
  voteIds <- parseTargetVotes
  skipSpace
  votePurpose <- takeText
  return . VoteCommand $ EditVoteSubject voteIds votePurpose

voteStatusParser :: Parser Command
voteStatusParser = do
  string "!votestatus" <|> string "!vote status"
  skipSpace
  (VoteCommand . VoteStatus)
    <$> ( optional parseTargetVotes >>= \case
            Nothing -> return AllVotes
            Just voteTargets -> return voteTargets
        )

endVoteParser :: Parser Command
endVoteParser = do
  string "!endvote" <|> string "!vote end"
  skipSpace
  fmap (VoteCommand . EndVote) parseTargetVotes

newArticleParser :: Parser Command
newArticleParser = do
  atype <- parseArticleType
    <* (string " new"
       <|> string " add"
       <|> string " create"
       <|> string " propose")
  skipWhile isHorizontalSpace
  number <- optional decimal
  skipWhile isHorizontalSpace
  endOfLine
  abody <- takeText
  return . ArticleCommand
    $ NewArticle atype number abody

fetchArticleParser :: Parser Command
fetchArticleParser = do
  atype <- parseArticleType
  skipSpace
  number <- decimal
  skipSpace
  endOfInput
  return . ArticleCommand
    $ FetchArticle atype number

editArticleParser :: Parser Command
editArticleParser = do
  atype <- parseArticleType
    <* (string " edit"
       <|> string " modify"
       <|> string " update")
  skipSpace
  number <- decimal
  skipWhile isHorizontalSpace
  endOfLine
  abody <- takeText
  return . ArticleCommand
    $ EditArticle atype number abody

repealArticleParser :: Parser Command
repealArticleParser = do
  atype <- parseArticleType <* string " repeal"
  skipSpace
  number <- decimal
  return . ArticleCommand
    $ RepealArticle atype number

deleteArticleParser :: Parser Command
deleteArticleParser = do
  atype <- parseArticleType <* string " delete"
  skipSpace
  number <- decimal
  return . ArticleCommand
    $ DeleteArticle atype number

fetchArticleInlineParser :: Parser Command
fetchArticleInlineParser = do
  let parseInlineArticle =
        (string "!r" >> decimal >>= (\n -> return (Rule, n))) <|>
        (string "!m" >> decimal >>= (\n -> return (Motion, n)))
  let fetchOneInlineArticle = do
        skipWhile (/= '!')
        parseInlineArticle
          <|> (A.take 1 >> fetchOneInlineArticle)
  articles <- many1 fetchOneInlineArticle
  return . ArticleCommand
    $ FetchArticleInline (fromList articles)

badCommandParser = do
  foldr1 (<|>) $
    (string . ("!" <>)) <$> [ "help"
                            , "scores"
                            , "addscore"
                            , "rule"
                            , "motion"
                            , "roll"
                            , "flip"
                            , "shuffle"
                            , "newvote"
                            , "votestatus"
                            , "endvote"
                            , "vote"
                            ]
  return BadCommand

--- Generic Parsers --

oneWordParser :: Text -> Command -> Parser Command
oneWordParser commandText command = string commandText >> return command

parseArguments :: Parser [Text]
parseArguments = (term <|> quotedTerm) `sepBy1` skipSpace
  where
    term = takeWhile1 (\c -> c /= '"' && not (isHorizontalSpace c))
    quotedTerm = do
      char '"'
      quoted <- term `sepBy1` skipSpace
      char '"'
      return (T.unwords quoted)

parseUserId :: Parser UserId
parseUserId = do
  string "<@!" <|> string "<@"
  uid <- read <$> many digit
  char '>'
  return uid

parseVoteId :: Parser VoteId
parseVoteId = char '#' >> decimal

parseTargetVotes :: Parser VoteTargets
parseTargetVotes =
  (string "all" $> AllVotes)
  <|> VoteList . fromList <$> sepBy1 parseVoteId (char ',' >> skipSpace)

parseDiffTime :: Parser NominalDiffTime
parseDiffTime = do
  days <- fromIntegral <$> option 0 (parseQuant 'd')
  hours <- fromIntegral <$> option 0 (parseQuant 'h')
  minutes <- fromIntegral <$> option 0 (parseQuant 'm')
  if days == 0 && hours == 0 && minutes == 0
    then fail "Invalid duration"
    else return $
         secondsToNominalDiffTime $ days * 86400 + hours * 3600 + minutes * 60
    where
      parseQuant c = do
        quant <- decimal
        char c
        return quant

parseTimeOfDay :: Parser AbsoluteTime
parseTimeOfDay = do
  hours <- read <$> count 2 digit
  char ':'
  minutes <- read <$> count 2 digit
  dayOffset <- option 0 $ char '+' >> decimal
  if hours >= 0 && hours < 24 && minutes >= 0 && minutes < 60
    then return AbsTime { hours = hours, minutes = minutes, dayOffset = dayOffset }
    else fail "Invalid time"

parseTime :: Parser Time
parseTime = eitherP parseDiffTime parseTimeOfDay

parseEndCondition :: Parser (EndCondition Time)
parseEndCondition = do
  canEndEarlyWithVotes <- peekChar >>= \case
    Just '!' -> char '!' $> False
    _ -> return True
  timeConstraint <- optional parseTime
  if canEndEarlyWithVotes
    then case timeConstraint of
      Nothing -> return AllVoted
      Just x -> return $ AllVotedOrTimeUp x
    else case timeConstraint of
      Nothing -> fail "This vote would not end."
      Just x -> return $ TimeUp x

parseArticleType :: Parser ArticleType
parseArticleType = do
  string "!motion" $> Motion
  <|> string "!rule" $> Rule
