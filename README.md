# 𝑓𝑓

A distributed note taker and task manager.

## What's the big deal?

1.  𝑓𝑓 synchronizes via any file sync application, like Yandex.Disk,
    Dropbox, or Google Drive.
2.  𝑓𝑓 is available offline and synchronizes without conflicts thanks to
    [CRDT](https://github.com/cblp/crdt).

## Stability

Works on all my computers and synchronizes between them successfully.
𝑓𝑓's own issues are tracked in 𝑓𝑓 since the first day of development.
Please feel free to test it in your environment.
But no guarantees.

## Install

    $ git clone https://github.com/cblp/ff.git
    $ stack install

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

### Get started

To save tasks somewhere you must configure 𝑓𝑓's data directory

    ff config dataDir --help

If you use Yandex.Disk, you can say just

    ff config dataDir -y

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
