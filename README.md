# _ﬀ_

The best note taker and task manager.

## Install

    $ git clone https://github.com/cblp/ff.git
    $ stack install

## Usage

### Get some unfinished tasks

    $ ff
    1: buy milk

Or, if no tasks

    $ ff
    nothing

### Add new task or note

    $ ff new 'learn Haskell'

Now you can find it in your agenda:

    $ ff
    1: buy milk
    2: learn Haskell

### Mark task "done"

Marking "done" is actually moving to archive.

    $ ff done 1
    archived:
      1: buy milk

Archived tasks don't appear in an agenda:

    $ ff
    2: learn Haskell
