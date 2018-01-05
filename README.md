# _ﬀ_

The best note taker and task manager.

## What's the big deal?

1.  _ﬀ_ synchronizes via any file sync application, like Yandex.Disk,
    Dropbox or Google Drive.
2.  _ﬀ_ is available offline and synchronizes without conflicts thanks to
    [CRDT](https://github.com/cblp/crdt).

## Stability

Alpha. Works on my laptop.
_ﬀ_'s own issues are tracked in _ﬀ_ since the first day of development.
Please feel free to test it in your environment.

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
