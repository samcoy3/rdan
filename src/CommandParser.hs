{-# LANGUAGE OverloadedStrings #-}

module CommandParser where

import Commands
import Vote

import Data.Char
import Data.Text (Text, unpack)
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
                <|> newVoteParser
                <|> voteStatusParser
                <|> endVoteParser
                <|> findInlineParser

helpParser :: Parser Command
helpParser = do
  asciiCI "!help"
  skipSpace
  fmap Help $ option Nothing $
    Just <$> choice (map asciiCI [ "help"
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
  asciiCI "!addscore"
  skipSpace
  uid <- parseUserId
  skipSpace
  delta <- signed decimal
  return $ AddToScore uid delta

findParser :: Parser Command
findParser = do
  command <- (asciiCI "!rule" >> return Rule)
             <|> (asciiCI "!motion" >> return Motion)
  skipSpace
  index <- decimal
  return $ Find (command index)

findInlineParser :: Parser Command
findInlineParser = do
  retrievables <- many1 findOneInlineParser
  return $ FindInline retrievables

rollParser :: Parser Command
rollParser = do
  asciiCI "!roll"
  skipSpace
  quantity <- decimal
  char 'd'
  sidedness <- decimal
  return $ Roll quantity sidedness

newVoteParser :: Parser Command
newVoteParser = do
  asciiCI "!newvote"
  skipSpace
  endCondition <- choice [ asciiCI "after"
                           >> skipSpace
                           >> parseDiffTime
                           >>= return . TimeUp . Left

                           , asciiCI "at"
                           >> skipSpace
                           >> parseTimeOfDay
                           >>= return . TimeUp . Right

                           , asciiCI "all votes or after"
                           >> skipSpace
                           >> parseDiffTime
                           >>= return . AllVotedOrTimeUp . Left

                           , asciiCI "all votes or at"
                           >> skipSpace
                           >> parseTimeOfDay
                           >>= return . AllVotedOrTimeUp . Right

                           , asciiCI "all votes"
                           >> return AllVoted ]
  skipSpace
  purpose <- takeText
  return $ NewVote endCondition purpose

voteStatusParser :: Parser Command
voteStatusParser = do
  asciiCI "!votestatus"
  fmap VoteStatus $ option Nothing $
    Just <$> many1 (skipSpace >> parseVoteId)

endVoteParser :: Parser Command
endVoteParser = do
  asciiCI "!endvote"
  fmap EndVote $ many1 (skipSpace >> parseVoteId)

--- Generic Parsers --

oneWordParser :: Text -> Command -> Parser Command
oneWordParser commandText command = asciiCI commandText >> return command

parseUserId :: Parser UserId
parseUserId = do
  asciiCI "<@!" <|> asciiCI "<@"
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
            (asciiCI "!r" >> decimal >>= (\n -> return $ Rule n)) <|>
            (asciiCI "!m" >> decimal >>= (\n -> return $ Motion n))

parseVoteId :: Parser VoteId
parseVoteId = char '#' >> decimal

parseDiffTime :: Parser NominalDiffTime
parseDiffTime = do
  days <- fromIntegral <$> option 0 (parseQuant 'd')
  hours <- fromIntegral <$> option 0 (parseQuant 'h')
  minutes <- fromIntegral <$> option 0 (parseQuant 'm')
  return $ secondsToNominalDiffTime $ days * 86400 + hours * 3600 + minutes * 60
    where
      parseQuant c = do
        quant <- decimal
        char c
        return quant

parseTimeOfDay :: Parser (Int, Int)
parseTimeOfDay = do
  hours <- read <$> count 2 digit
  char ':'
  minutes <- read <$> count 2 digit
  if hours >= 0 && hours < 24 && minutes >= 0 && minutes < 60
    then return (hours, minutes)
    else fail "Invalid time"
