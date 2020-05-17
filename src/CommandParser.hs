{-# LANGUAGE OverloadedStrings #-}

module CommandParser where

import Commands

import Data.Char
import Data.Text (Text, unpack)
import Control.Applicative

import Data.Attoparsec.Text as A

import Discord.Types

commandParser :: Parser Command
commandParser = helpParser
                <|> printScoresParser
                <|> addToScoreParser
                <|> findParser
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

newVoteParser :: Parser Command
newVoteParser = do
  asciiCI "!newvote"
  skipSpace
  NewVote <$> takeText

voteStatusParser :: Parser Command
voteStatusParser = oneWordParser "!votestatus" VoteStatus

endVoteParser :: Parser Command
endVoteParser = oneWordParser "!endvote" EndVote

--- Generic Parsers --

oneWordParser :: Text -> Command -> Parser Command
oneWordParser commandText command = asciiCI commandText >> return command

parseUserId :: Parser UserId
parseUserId = do
  asciiCI "<@" <|> asciiCI "<@!"
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
