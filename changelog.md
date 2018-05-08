# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]
### Fixed
- Fix install instruction

## [0.3] - 2018-04-22
### Added
- Add `unarchive`
- Add box around note
- Add calendar for date edits
- Add date-edit labels and deadline date-edit
- Add dates to tasks
- Add empty delegate
- Add FullMap
- Add label into the start field
- Add note counters
- Add Qt5 to Travis
- Add some little UI tweaks
- Add some sugar

### Fixed
- Fix CI script placement
- Fix organizationDomain
- Fix yaml syntax

### Changed
- Auto-expand sections on start
- Decouple widgets from model
- Disable date editing temporarily
- Disable section editing
- displayed when there some tasks were hidden due to limit.
- Extract NoteModel
- Make ModeMap Functor and Foldable
- Make sections bold
- More compact format to store zeroes in RGA
- Move tasks from personal ff to the project repo
- Optimize imports
- Push items to top
- Reorganize packages
- Restore Qt GUI
- samplesInSections: Add "N task(s) omitted" message
- Save and restore window position
- Set first view item current
- Set proper version in Qt
- Shuffle/Sort notes
- simplify executable configuration
- Split notes in sections
- Style
- This patch also makes "To see more tasks" message conditional: it's only
- Try add pthread
- Update lts to LTS-11.5
- Update Qtah to upstream 0.4.0 + 2018-04-09
- Update to crdt-10.0: use RGA without Maybe
- Use code repository as a note storage
- Use qtah with fixed QAction ctor
- Use qtah with fixed QAction ctor 2
- Use qtah with fixed QAction ctor 3
- Use qtah with fixed QWindow.requestUpdate
- Use QToolBox
- Use scoped ptr for controllable finalization

### Removed
- Remove redundant dependency

## [0.2] - 2018-02-12
### Added
- Add `agenda --all`
- Add `config` command
- Add `ff-delete`
- Add `postpone` command
- Add `search` command
- Add any-note-list features
- Add brittany config
- Add color backgrounds for Features
- Add detection of Yadex.Disk
- Add ending-soon tasks to Agenda
- Add explicit agendaOrdering
- Add Features
- Add ff-edit
- Add FF.Document.list
- Add highlighting to examples
- Add logo
- Add lwwModify
- Add more features
- Add multiline notes feature
- Add Nikolay Loginov to 
- Add realistic note ids
- Add start and end dates to notes
- Add Storage
- Add subcommand `add`; rename `del` -> `delete`
- Add subcommand `dir` to set up data dir
- Add support for default linux Yandex.Disk name
- Add TaskMode
- Add test for `add`/`new` command
- Add Travis config
- Add UTF-8 in Features
- Add weeder to test
- Add workspaces to planned features
- ff-gui: Add agenda list, yet empty
- ff-qt: Add agenda notes

### Fixed
- Fix comma
- Fix note-id pairing
- Fix Qt version
- Fix syntax
- Fix tests
- Fix typo
- Fix wording
- Fix wording about setting data dir
- Fix wording in readme
- Fix wording on stability
    
### Changed
- Be humble
- Calculate collection name from type
- Change start date to non-maybe
- Check start-end linearity when creating and editing note
- CLI UI: Move note id from text to fields
- CLI: prettify multi-line notes
- CLI: Separate overdue and today tasks
- Compress ids with Base36
- Extract CLI options to a separate module
- Extract Document tools to a module
- Extract types to FF.Types; rename fields
- features: Fix ul margins
- features: Make feature DSL even more concise
- ff-edit: Select editor automagically
- Get empty listing if directory does not exist
- Hide unneeded features
- Implemented `postpone`
- Improve agenda CLI
- Limit agenda by default
- Make note fields a bit more compact
- Move Clock instances to `crdt` package
- Move Features to Haskell
- Move ff exe to a subdirectory
- Move focus from desktop to Android
- Move test cases to MonadStorage
- Move to class-based interface for testability
- Postpone with end
- Pretty agenda
- Put config file in the app's dir
- Rename cmdAgenda -> getAgenda
- Rename exe ff-gui -> ff-qt
- Restore total limiting of sample group
- Rethink and rewrite Agenda and sample sections
- Rewrite CLI UI in Mainland-pretty instead of YAML
- Show notes list as a list with bullets
- Show start/end dates in Agenda
- Simplify sampling using State
- Sort agenda by start
- Style
- Switch qtah dependency to master
- Update lts to 10.3
- Update qtah dep
- Update README for v0.2
- Update timestamp when user really wants it even if value do not change
- Use real LamportClock and TVar
- Use RGA for the note text
- Use U+1D453 MATHEMATICAL ITALIC SMALL F
- WIP

### Removed
- Remove content on deletion
- Remove redundant `errors` dependency
- Remove redundant parts
    
## [0.1] - 2018-01-05
### Added
- Add `done` command
- Add CRDT
- Add exe
- Add liveness
- Add New command
- Add notes on the big deal and stability
- Add readme

### Other
- Compact LWW
- Describe `done` in readme
- Generalize loadDocument to CvRDT
- Implement agenda in exe
- Style
- Switch to Hackage crdt
- Use ﬀ symbol in header
- Use the same id for note and first version
