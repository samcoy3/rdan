# Changelog for rdan

## Unreleased changes
### Added
- New syntax for vote-related activities:
  - !newvote -> !vote new
  - !votestatus -> !vote status [targets]
  - !endvote -> !vote end [targets]
- New vote actions:
  - "!vote edit subject [targets] [text]" will edit the subject of a vote.
  - "!vote edit time [targets] [time]" will re-set the time of a vote.
- New options when specifying the target of a !vote command:
  - A comma-separated list of vote IDs, e.g. "#1, #9,#3".
  - The word "all": applies the command to all currently running votes.

### Changed
- Updated stackage snapshot to a more modern one.
- Configuration is now done via a YAML file, rather than a .hs file.
- The state of the game is now read from a YAML file on startup and automatically saves during play.
- !addscore now does not require you to tag the target player. Instead, you can type their name, as registered in the game.

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
