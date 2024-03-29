# Changelog for rdan

## Unreleased changes
### Changed
- Changed some of the "!vote status" text.

## [1.0.5] - 2021-06-06
### Changed
- Added synonyms for adding and editing rules and motions.
  - For adding, "!rule add", "!rule create", and "!rule propose" are all now synonyms.
  - For editing, "!rule modify" and "!rule update" are both now synonyms.
- Changed vote status message to make it clearer when all players have voted.
- Added config option "announce-channel". If set (to a channel ID), the result of votes are announced in there, rather than in the channel in which they were started.
- Added config option "end-all-votes-automatically". If set as "true", the bot will end all concurrent votes when all players have voted on all of them, regardless of the settings set on the votes themselves.
  - The bot will remind players that this setting is enabled, if it is, when "!vote status all" is used.
- When a rule or motion is edited, the bot will display what it previously was.

### Fixed
- Fixed a bug where un-reacting with a custom emoji on a vote would not remove the vote.
- The bot's reply after "!rule/motion repeal" now correctly indicates whether the the rule or motion was repealed, or unrepealed.

## [1.0.4] - 2021-06-02
### Changed
- "!flip" and "!shuffle" now both use the list of players by default if no options are provided.

## [1.0.3] - 2021-06-02
### Added
- Added "!flip", to choose randomly between a series of options.
- Added "!shuffle", to order a series of options randomly.

## [1.0.2] - 2021-05-08
### Changed
- "!scores" now prints the scores in order, from highest to lowest.
- "!rule/motion edit" is now less fussy about superfluous horizontal space.

### Fixed
- The bot properly displays custom emoji in voting results.

## [1.0.1] - 2021-05-03
### Added
- A "default reactions" setting has been added to the config. The bot will react to new vote messages using these reactions, in the order they are specified.

### Changes
- "!addscore" no longer prints the full list of scores. Instead it prints the scores that changed, inline.

### Fixed
- Fixed "!help" showing an imcomplete list of commands.
- Fixed "!help vote new" showing incorrect examples.

## [1.0] - 2021-04-27
### Added
- The bot keeps track of rules and motions. No human now has to curate the rules and motions channels manually.
  - Available commands for rule/motion management:
    - "!rule/motion [number]" will fetch the rule or motion with the specified number. As before, "!r[n]/!m[n]" will work inline.
    - "!rule/motion new [number]\n[body]" will post a new rule or motion.
    - "!rule/motion edit [number]\n[body]" will edit a specified rule or motion.
    - "!rule/motion repeal [number]" will repeal a specified rule or motion, leaving it formatted as struck-through.
    - "!rule/motion delete [number]" will delete a specified rule or motion, deleting the posted message as well.
- New syntax for vote-related activities:
  - !newvote -> !vote new
  - !votestatus -> !vote status [targets]
  - !endvote -> !vote end [targets]
- New vote actions:
  - "!vote edit subject [targets] [text]" will edit the subject of a vote.
  - "!vote edit time [targets] [time]" will re-set the time of a vote.
- The state of the game is now read from a YAML file on startup and automatically saves during play.
- New options when specifying the target of a !vote command:
  - A comma-separated list of vote IDs, e.g. "#1, #9,#3".
  - The word "all": applies the command to all currently running votes.
- The bot will react with a question mark when your command is invalid.
- The bot can send reminders to players who haven't voted on votes which are about to end.
  - Whether or not to do this, and the amount of forewarning, is configurable in the config.

### Changed
- Updated stackage snapshot to a more modern one.
- Configuration is now done via a YAML file, rather than a .hs file.
- !addscore now does not require you to tag the target player. Instead, you can type their name, as registered in the game.
- Updated the help text to reflect the new syntax for votes and the new syntax for rules and motions.

### Deprecated
- Deprecated the !newvote, !endvote, and !votestatus commands.

### Removed
- Removed the verbose syntax for starting a new vote.

## [0.3.1] - 2020-06-06
### Added
- Added a new shorthand to specify voting options.

### Changed
- Changed the voting options help text.
- The !addscore command now prints the new scores.
- The !addscore command now accepts multiple pairs of users and deltas.
- Changed frequency of vote-ending polling to every 15 seconds (from 60).

### Deprecated
- Deprecated the old syntax for specifying voting options.

## [0.3] - 2020-05-23
### Changed
- Multiple votes can now be run in parallel.
- !newvote, !votestatus, and !endvote have all changed to accommodate this functionality.
- Votes can now be set to either end after everyone has voted, end after a specified time, or both.
- Voting now takes place using reactions.

### Fixed
- Fixed username tags failing to parse properly if the user had a nickname.

## [0.2] - 2020-05-17
### Added
- Added !roll command

### Changed
- Added Attoparsec parser for commands.
- Reworked !help so that it processes all commands as an argument, e.g. !help scores.

### Fixed
- !rule not taking an argument no longer crashes the bot.

## [0.1.1] - 2020-05-11
### Changed
- Moved starting scores into Config.hs.
- Moved all the commands to Commands.hs. Now searches a map of commands rather than using isPrefixOf.
- Improved documentation.

### Fixed
- Fixed bug where !endvote would not end the vote.
- Fixed !rule and !motion breaking with bad input.
- Fixed !endvote breaking when there's no current vote.

## [0.1] - 2020-05-09
### Added
- Can track and update player scores.
- Can initialise, track, and prematurely end votes.
- Can find rules and motions in particular channels.
- Can modify game state in the Config.hs file.
