# 𝑓𝑓

A distributed note taker and task manager.

## What's the big deal?

1.  𝑓𝑓 synchronizes via any file sync application, like git, Yandex.Disk,
    Dropbox, or Google Drive.
2.  𝑓𝑓 is available offline and synchronizes without conflicts thanks to
    [CRDT](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type)
    and [RON](http://replicated.cc).

## Stability

Works on all my computers and synchronizes between them successfully.
𝑓𝑓's own issues are tracked in 𝑓𝑓 since the first day of development.
I also use 𝑓𝑓 for my personal tasks since the first day of development.
Please feel free to test it in your environment.
But no guarantees.

## Project-based task management

𝑓𝑓 detects if it is run inside a **git** repository and saves notes in it.
Other VCS will be supported in future.
Feel free to open a ticket if you want to extend support.

## Install

    $ git clone https://github.com/ff-notes/ff.git
    $ cd ff
    $ stack install ff

## Install experimental GUI

### GTK version

#### Prerequisites

Follow https://github.com/haskell-gi/haskell-gi#installation.

#### Program

Mac OS:

    $ export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig"
    $ stack install ff-gtk

Ubuntu:

    $ stack install ff-gtk

### Qt version

#### Prerequisites

Mac OS:

    $ brew install qt

Ubuntu:

    # apt install qt5-default

#### Program

    $ stack install ff-qt

### Completion for commands and options

#### Ubuntu 18.04

    ff --bash-completion-script `which ff` | sudo tee /etc/bash_completion.d/ff

#### MacOS

    ff --bash-completion-script `which ff` | sudo tee /usr/local/etc/bash_completion.d/ff

## Usage

### Look what 𝑓𝑓 can do for you

    $ ff --help
    Usage:  [-b|--brief] [-C|--data-dir DIRECTORY] ([-V|--version] | [COMMAND] |
            [-l|--limit ARG] [--tag TAG]...)
      A note taker and task tracker

    Available options:
      -b,--brief               List only note titles and ids
      -C,--data-dir DIRECTORY  Path to the data dir
      -V,--version             Current ff-note version
      -l,--limit ARG           Number of issues
      --tag TAG...             Tag
      -h,--help                Show this help text

    Available commands:
      add                      add new task or note
      agenda                   show what you can do right now [default action]
      config                   show/edit configuration
      contact                  show contacts
      delete                   delete a task
      done                     mark a task done (archive)
      edit                     edit a task or a note
      new                      synonym for `add`
      postpone                 make a task start later
      search                   search for notes with the given text
      show                     show note by id
      tags                     show tags of all notes
      sponsors                 show project sponsors
      track                    track issues from external sources
      unarchive                restore the note from archive
      upgrade                  check and upgrade the database to the most recent
                               format
      wiki                     show all wiki notes

### Get started

#### Personal

To save personal notes and tasks somewhere you must configure 𝑓𝑓's data
directory

    ff config dataDir --help

If you use Yandex.Disk, you can say just

    ff config dataDir -y

#### Project-based

To work with a project's tasks, just run 𝑓𝑓 inside a VCS repository.

    $ ls
    .git
    $ ff add ...
    $ ls
    .ff .git
    $ git add .ff

### Get some unfinished tasks

    $ ff
    Actual:
        * buy milk
          id 458a1456lrc-12z3opnykw, start 2018-01-10

### Add new task or note

    $ ff add 'learn Haskell'

Now you can find it in your agenda:

    $ ff
    Actual:
        * buy milk
          id 458a1456lrc-12z3opnykw, start 2018-01-10
        * learn Haskell
          id 459h2tqxutq-24nuhr86ae, start 2018-01-14

### Mark task done

Marking done is actually moving to archive.

    $ ff done 458a1456lrc-12z3opnykw
    archived:
        buy milk
        id 458a1456lrc-12z3opnykw, start 2018-01-10

Archived tasks don't appear in an agenda:

    $ ff
    Actual:
        * learn Haskell
          id 459h2tqxutq-24nuhr86ae, start 2018-01-14

## Developers community

We have a telegram chat [ff-dev](https://t.me/ff_dev) where you can ask your questions.

Alternative chat: https://gitter.im/ff-notes
