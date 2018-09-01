# 𝑓𝑓

A distributed note taker and task manager.

## What's the big deal?

1.  𝑓𝑓 synchronizes via any file sync application, like git, Yandex.Disk,
    Dropbox, or Google Drive.
2.  𝑓𝑓 is available offline and synchronizes without conflicts thanks to
    [CRDT](https://github.com/cblp/crdt).

## Stability

Works on all my computers and synchronizes between them successfully.
𝑓𝑓's own issues are tracked in 𝑓𝑓 since the first day of development.
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

### Completion for commands and options (tested on Ubuntu 18.04)

    ff --bash-completion-script `which ff` | sudo tee /etc/bash_completion.d/ff

## Usage

### Look what 𝑓𝑓 can do for you

    $ ff --help
    Usage: ff ([COMMAND] | [-l|--limit ARG])
      A note taker and task tracker

    Available options:
      -l,--limit ARG           limit
      -h,--help                Show this help text

    Available commands:
      add                      add a new task or note
      agenda                   show what you can do right now [default action]
      config                   show/edit configuration
      delete                   delete a task
      done                     mark a task done (archive)
      edit                     edit a task or a note
      new                      synonym for `add`
      postpone                 make a task start later
      search                   search for notes with the given text
      unarchive                restore the note from archive

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
