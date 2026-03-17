# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this project is

`ff` is a distributed, CRDT-based note taker and task manager written in Haskell. It stores data using [RON (Replicated Object Notation)](http://replicated.cc) for conflict-free synchronization via any file sync tool (git, Dropbox, etc.).

## Build and test commands

```sh
# Build all packages
stack build

# Run all tests
stack test

# Run tests matching a pattern
stack test ff-test --ta '-p "pattern"'

# Build and install the ff binary
stack install ff

# Run tests with CI-level strictness (pedantic)
stack test --pedantic --ghc-options=-Wincomplete-record-updates --ghc-options=-Wincomplete-uni-patterns --ghc-options=-Wredundant-constraints
```

## Code formatting

The project uses **fourmolu** for formatting (config in `fourmolu.yaml`) and **HLint** (config in `.hlint.yaml`). Key fourmolu settings: hanging if-style, left-align in-style, sorted constraints/derived classes.

## Architecture

The codebase is split into several Stack packages:

- **`ff`** — thin executable wrapper (`ff/Main.hs` calls `FF.CLI.cli`)
- **`ff-core`** — all business logic and CLI code
- **`ff-test`** — test suite (tasty + tasty-golden + tasty-hedgehog)
- **`ff-brick`** — experimental TUI frontend (included in stack.yaml)
- **`ff-qtah`** — experimental Qt GUI frontend (included in stack.yaml)
- **`ff-gtk`** — experimental GTK GUI frontend (not in stack.yaml)

### Core modules (`ff-core/lib/`)

| Module | Role |
|---|---|
| `FF.Types` | All core data types and RON schema definitions |
| `FF` | Business logic: CRUD for notes, contacts, tags |
| `FF.CLI` | CLI command dispatch, output formatting, external editor integration |
| `FF.Options` | CLI option parsing (optparse-applicative) |
| `FF.Config` | Config loading/saving; detects `.ff/` dir in git repos |
| `FF.Github` | GitHub issue tracking integration |
| `FF.UI` | Pretty-printing with prettyprinter/ANSI terminal |
| `FF.Upgrade` | Database schema upgrade logic |

### Data model and RON schema

Types are defined with the `[mkReplicated| ... |]` Template Haskell quasi-quoter in `FF/Types.hs`. CRDT merge semantics per field:
- `lww` — last-write-wins
- `merge` / `RgaString` — RGA (Replicated Growable Array, used for text)
- `set` — CRDT set

Main types: `Note` (tasks and wiki pages), `Contact`, `Tag`, `Track` (external issue tracking), `TagGroup`, `Link`.

`NoteStatus` is either `TaskStatus Active | TaskStatus Archived | Wiki`. Tasks are categorized into display modes (`TaskMode`): `Overdue`, `EndToday`, `EndSoon`, `Actual`, `Starting`.

### Config and data directory resolution

`FF.Config.loadConfig` searches upward from the current directory for a `.ff/` directory (project-local mode). If not found, falls back to `~/.config/ff/config.yaml` (XDG). Running `ff` inside a git repo auto-uses `.ff/` within it.

### Storage backend

Data is stored on the filesystem via `ron-storage` (from the `ff-notes/ron` repo, pinned by git commit in `stack.yaml`). Each collection (`note`, `contact`, `tag`) is a subdirectory under the data dir. Documents are identified by `DocId` (a RON UUID).

### ff-qtah: Qt GUI frontend

The Qt frontend uses [qtah](https://hackage.haskell.org/package/qtah) bindings. Key constraints:

**Thread safety**: All Qt UI operations must happen on the main (GUI) thread. Never call Qt functions from `forkIO` threads — this is UB and causes unpredictable behavior (e.g., sorting not applied). The correct pattern for background data loading:

```haskell
-- background thread: only pure IO, no Qt calls
void . forkIO $ do
    result <- loadData
    putMVar resultVar result
-- main thread: receives result and updates UI
runInGuiThreadWhenReady resultVar $ \result -> updateUI result
```

`runInGuiThreadWhenReady` (in `FF.Qt`) creates a 0ms repeating timer on the main thread, and when the `MVar` is filled it calls `QObject.deleteLater` to stop itself, then runs the action.

**qtah API gaps**: `QTimer` in qtah does not expose `stop` — use `QObject.deleteLater` to destroy the timer from within its own callback.

### External dependencies (extra-deps)

The RON libraries (`ron`, `ron-rdt`, `ron-schema`, `ron-storage`) are pulled directly from the `ff-notes/ron` GitHub repo at a specific commit, not from Hackage.
