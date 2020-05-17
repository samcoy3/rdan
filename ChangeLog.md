# Changelog for rdan

## Unreleased changes
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
