# µTunes

µTunes (`utunes`; pronounced "micro-tunes") is a music library manager
and music player which makes aggressive design decisions in order to
simplify its implementation. Users are expected to perform some
scripting to access advanced functionality. Supported use cases
include:

* Import an existing music library, including metadata embedded in the
  files.
* Search and sort your music library by song, album, artist, or other
  metadata fields.
* Create, edit, and delete playlists.
* Play and pause music from an "up next" queue.
* Organize music files on disk, and update embedded metadata.
* Maintain a directory of album artwork.

## Architecture

Conceptually, µTunes is cleanly separated into three parts:

* *Music library.* Your media files on disk, a JSON file describing
  their metadata and playlists, and a JSON file describing saved
  playback state (used to restart playback when the µTunes playback
  server is stopped).
* *Command-line tool.* Python program which provides a minimal,
  UNIX-style command-line interface to manipulate the music library
  and a transparently managed background playback server.
* *Emacs frontend.* Emacs Lisp package which provides minimal user
  commands that use the command-line tool to implement helpful
  abstraction for music library management (e.g. "edit an album").

These three components are described in the next three sections.

## Music library

µTunes identifies a directory as a music library by the presence of a
file called `utunes.json`, in the same manner that Git identifies a
repository by the presence of a file or directory called `.git`.

Instead of relying on µTunes' automatic library detection, you can
export the environment variable `UTUNES_LIBRARY` to a directory to
force it to be recognized as a music library. This is necessary in
order to initialize a music library for the first time, since
`utunes.json` does not yet exist.

The filesystem layout of a music library is as follows:

    .
    |
    |-- utunes.json
    |
    |-- music
    |   |
    |   |-- <album>
    |   |   |
    |   |   |-- <song>.mp3
    |   |   |
    |   |   |-- ...
    |   |
    |   |-- ...
    |
    |-- playback.json

The file `utunes.json` is structured as follows:

    {
      "version": 1,
      "songs": {
        "60c223fb": {
          "title": "Speed of Sound",
          "album": "X&Y",
          "artist": "Coldplay",
          "track": "6",
          "disc": "1",
          "artwork": "x&y.png",
          ...
        },
        ...
      },
      "playlists": [
        {
          "Up Next": [
            "60c223fb",
            ...
          ],
          ...
        }
      ]
    }

The `version` key will be incremented every time breaking changes are
made to the music library format.

Songs may have arbitrary keys and values, as long as both are
non-empty, but the `id` and `filename` keys are required. The `id` key
is a eight-character hexadecimal string guaranteed to be unique within
the music library. The `filename` key is the path to the media file,
relative to the `music` directory. Some of the keys, if present, are
used in automatically generated filenames for song files: `album`,
`title`, `disc`, `track`, and `id`.

After some normalization is performed on special characters to make
them play nicely with the filesystem, the format for song filenames is
essentially `music/{album}/{disc:02d}-{track:03d}-{id} {title}.mp3`.

The file `playback.json` has the following format:

    {
      "playlist": "Up Next",
      "index": 5,
      "seek": 140
    }

The playlist index is one-based, and the approximate seek position is
in seconds.

## Command-line tool

The µTunes command-line interface exposes several subcommands, which
are described in the following sub-sections.

µTunes recognizes one environment variable, `UTUNES_LIBRARY`. If it is
set, then µTunes will use that directory as its music library.
Otherwise, µTunes will look at the working directory and its parents,
and it will identify the music library directory by the presence of
the file `utunes.json`.

### read

    $ utunes read FORMAT
        [-f, --filter   FIELD=REGEX]...
        [-s, --sort   Q:FIELD      ]...
        [-i, --illegal-chars  REGEX]

This subcommand writes to stdout. By default, all songs are printed.
For each song, the provided format string is passed to Python's
`str.format` with the song's key/value pairs available as keyword
arguments (e.g. `{id}` expands to `60c223fb`). Generally, the format
string is expected to end with a newline.

The `--filter` argument can be used to limit output. The field can be
any key; songs which are missing that key or whose value for that key
does not match the provided regex (with a match spanning the full
value) are skipped. For example, to list a particular album, consider
`--filter album="X&Y"`. Any number of filters are allowed, and song
objects must match all of them to be listed.

Aside from the keys present on the song objects themselves, one
additional field is allowed in filters: `playlist`. If you filter by
`playlist`, then in order for a song to be listed, it must be in a
playlist at an index such that the provided regex matches the string
`PLAYLIST:INDEX`. (You cannot use `playlist` in the format string.)

The `--sort` argument can be used to order the list. The qualifier `Q`
may be either `s` (sort normally; `S` for numeric), `r` (sort in
reverse; `R` for numeric), or `x` (sort randomly). Sorts are applied
stably and from right to left.

The `--illegal-chars` argument can be used to ensure that the output
is machine-parseable. If any field substituted into the format string
contains any of the characters specified, then an error is reported
and the command terminates.

### write

    $ utunes write FORMAT [PLAYLIST]

This subcommand reads from stdin. The input must be a sequence of
strings which match the provided Python-style regular expression, with
no delimiter. (You will probably want to include a trailing newline in
the regex.) The regex must include named capture groups whose names
correspond to keys in a song object (e.g. `(?P<album>...)`). If there
is a format specifier for `id` (e.g. `(?P<id>)`), then the other
fields will be updated in the library database as directed, if needed,
and song files will be renamed accordingly. Otherwise, there must be a
format specifier for `filename`, and the song will be imported into
the library database, with the file renamed appropriately from the
given filename. If the value for a capture group is empty, then the
key is removed from the song. This is not allowed for the `id` field.
If the special field `delete` is non-empty, then songs are removed
from the library database and their files are moved to the `trash`
subdirectory of the library directory (next to `music`, with the same
subdirectory structure).

If you provide a playlist name, then the given playlist is overwritten
with the provided songs in the given order.

### playback

    $ utunes playback

This subcommand reads from stdin and writes to stdout, both in JSON
format. Both the input and output JSON look like this, except that for
the input JSON any or all of the keys may be omitted, and the
`seek_end` key *must* be omitted:

    {
      "playlist": "Up Next",
      "index": 1,
      "seek": 140,
      "seek_end": 288,
      "playing": true
    }

If the playback server is not running, µTunes starts it first. Then,
µTunes makes all of the changes described in the input JSON,
including: switching playlist, seeking to a particular (one-based)
index in the playlist, seeking to a particular offset within the song
(in seconds), and playing or pausing. The new state of the playback
server is returned as the output JSON. Thus, querying the playback
server can be done by passing an empty map as the input JSON.

The response may have a null value for the `playlist`, `index`,
`seek`, and/or `seek_end` keys if these parameters have not yet been
set, or if data is unavailable.

## Emacs frontend

Interactive commands provided by the Emacs frontend are:

* `M-x utunes-import-dir`
* `M-x utunes-read`
  * `M-x utunes-read-album`
  * `M-x utunes-read-playlist`
* `[C-u] M-x utunes-write`
* `M-x utunes-playback`
  * `M-x utunes-playback-toggle`
  * `[C-u] M-x utunes-seek-previous`
  * `[C-u] M-x utunes-seek-current`
  * `[C-u] M-x utunes-seek-next`

## Installation

Playback requires installing [MPV].

The backend may be installed as a standard Python package from this
GitHub repository. It is recommended to use [pipx]:

    $ pipx install utunes --spec git+https://github.com/radian-software/utunes.git

The frontend may be installed as a standard Emacs Lisp package from
this GitHub repository. It is recommended to use
[`straight.el`][straight.el]:

    (straight-use-package '(utunes :host github :repo "radian-software/utunes"))

To make sure the packages remain in sync, I recommend installing the
Emacs package first and then running:

    $ pipx install utunes -e --spec ~/.emacs.d/straight/repos/utunes

(This last command currently suffers from a [bug in Pip][pipx#151].)

## Todo

In order of priority.

* Emacs frontend.
* Generic importer.
* Removing songs.
* Play statistics.
* Better documentation.

[mpv]: https://mpv.io/
[pipx]: https://github.com/pipxproject/pipx
[pipx#151]: https://github.com/pipxproject/pipx/issues/151
[straight.el]: https://github.com/raxod502/straight.el
