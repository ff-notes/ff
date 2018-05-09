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
- Option `--all` to subcommand `agenda`
- Subcommand `config`
  - Subcommand `dir`
- Subcommand `delete`
- Subcommand `postpone`
- Subcommand `search`
- Yandex.Disk detection
- Show ending-soon tasks in Agenda
- Subcommand `edit`
  - Select editor automagically
- Multiline notes
- Start and end dates for notes
- Subcommand `add`/`new`
- Subcommand `delete`
- Clear note content on deletion

### Changed
- Move note id from text to fields
- Prettify multi-line notes
- Separate overdue and today tasks
- Compress ids with Base36
- Agenda:
  - Split with samples
  - Limit to 10 by default
  - Use Mainland-pretty for UI instead of YAML
  - Show notes list as a list with bullets
  - Show start/end dates
  - Sort notes by start
- Use RGA for the note text

## [0.1] - 2018-01-05
### Added
- Executable `ff`
- Subcommand `agenda`
- Subcommand `done`
- Subcommand `new`

[Unreleased]: https://github.com/ff-notes/ff/compare/v0.3...HEAD
[0.3]: https://github.com/ff-notes/ff/compare/v0.2...v0.3
[0.2]: https://github.com/ff-notes/ff/compare/0.1...v0.2
[0.1]: https://github.com/ff-notes/ff/tree/0.1
