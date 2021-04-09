module CommandParser where

import Commands
import Vote
import Config

import Data.Char
import Data.Text (Text, unpack, pack)
import Control.Applicative

import Data.Time.Clock

import Data.Attoparsec.Text as A

import Discord.Types

commandParser :: Parser Command
commandParser = helpParser
                <|> printScoresParser
                <|> addToScoreParser
                <|> findParser
                <|> rollParser
                <|> newVoteLongParser
                <|> newVoteParser
                <|> voteStatusParser
                <|> endVoteParser
                <|> findInlineParser

helpParser :: Parser Command
helpParser = do
  string "!help"
  skipSpace
  fmap Help $ option Nothing $
    Just <$> choice (map string [ "help"
                                 , "scores"
                                 , "addscore"
                                 , "rule"
                                 , "motion"
                                 , "roll"
                                 , "newvote"
                                 , "votestatus"
                                 , "endvote"])

printScoresParser :: Parser Command
printScoresParser = oneWordParser "!scores" PrintScores

addToScoreParser :: Parser Command
addToScoreParser = do
  string "!addscore"
  changes <- many1 $ do
    skipSpace
    target <- eitherP
      parseUserId
      (pack <$> manyTill anyChar (skipSpace >> signed decimal))
    skipSpace
    delta <- signed decimal
    return $ (target, delta)
  return $ AddToScore changes

findParser :: Parser Command
findParser = do
  command <- (string "!rule" >> return Rule)
             <|> (string "!motion" >> return Motion)
  skipSpace
  index <- decimal
  return $ Find (command index)

findInlineParser :: Parser Command
findInlineParser = do
  retrievables <- many1 findOneInlineParser
  return $ FindInline retrievables

rollParser :: Parser Command
rollParser = do
  string "!roll"
  skipSpace
  quantity <- decimal
  char 'd'
  sidedness <- decimal
  return $ Roll quantity sidedness

newVoteLongParser :: Parser Command
newVoteLongParser = do
  string "!newvote"
  skipSpace
  endCondition <- choice [ string "after"
                           >> skipSpace
                           >> parseDiffTime
                           >>= return . TimeUp . Left

                           , string "at"
                           >> skipSpace
                           >> parseTimeOfDay
                           >>= return . TimeUp . Right

                           , string "all votes or after"
                           >> skipSpace
                           >> parseDiffTime
                           >>= return . AllVotedOrTimeUp . Left

                           , string "all votes or at"
                           >> skipSpace
                           >> parseTimeOfDay
                           >>= return . AllVotedOrTimeUp . Right

                           , string "all votes"
                           >> return AllVoted ]
  skipSpace
  purpose <- takeText
  return $ NewVote endCondition purpose

newVoteParser :: Parser Command
newVoteParser = do
  string "!newvote"
  skipSpace
  canEndEarlyWithVotes <- option True $ char '!' >> return False
  timeConstraint <- option Nothing $
    Just <$> choice [ Left <$> parseDiffTime
                    , Right <$> parseTimeOfDay ]
  skipSpace
  votePurpose <- takeText
  endCondition <- if canEndEarlyWithVotes
                  then case timeConstraint of
                         Nothing -> return AllVoted
                         Just x -> return $ AllVotedOrTimeUp x
                  else case timeConstraint of
                         Nothing -> fail "This vote would not end."
                         Just x -> return $ TimeUp x
  return $ NewVote endCondition votePurpose

voteStatusParser :: Parser Command
voteStatusParser = do
  string "!votestatus"
  fmap VoteStatus $ option Nothing $
    Just <$> many1 (skipSpace >> parseVoteId)

endVoteParser :: Parser Command
endVoteParser = do
  string "!endvote"
  fmap EndVote $ many1 (skipSpace >> parseVoteId)

--- Generic Parsers --

oneWordParser :: Text -> Command -> Parser Command
oneWordParser commandText command = string commandText >> return command

parseUserId :: Parser UserId
parseUserId = do
  string "<@!" <|> string "<@"
  uid <- read <$> many digit
  char '>'
  return uid

findOneInlineParser :: Parser Retrievable
findOneInlineParser = do
  skipWhile (/= '!')
  nextRetrievable <- choice [ parseInlineRetrievable
                            , A.take 1 >> findOneInlineParser]
  return nextRetrievable
    where parseInlineRetrievable =
            (string "!r" >> decimal >>= (\n -> return $ Rule n)) <|>
            (string "!m" >> decimal >>= (\n -> return $ Motion n))

parseVoteId :: Parser VoteId
parseVoteId = char '#' >> decimal

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
