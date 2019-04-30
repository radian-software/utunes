# µTunes

µTunes is a music library manager and music player designed to have as
minimal of an implementation as possible, while still being fully
usable as a replacement for major functions of a larger player like
iTunes.

## Architecture

µTunes consists of three components:

* Your music library. This is stored on disk as a collection of media
  files and JSON metadata.
* The backend. This is a command-line utility written in Python, which
  provides very minimal search and editing features for your library
  in the style of Git's `git-rebase-todo` file. It does not run as a
  background daemon or server, although it does start a daemon
  specifically to handle playing songs.
* The frontend. This is an Emacs package which provides some simple
  functions to read and write text data between Emacs buffers and the
  file descriptors of the backend.

### Installation

The backend may be installed as a standard Python package from this
GitHub repository. It is recommended to use [pipx]:

    $ pipx install utunes --spec git+https://github.com/raxod502/utunes.git

The frontend may be installed as a standard Emacs Lisp package from
this GitHub repository. It is recommended to use
[`straight.el`][straight.el]:

    (straight-use-package '(utunes :host github :repo "raxod502/utunes"))

To make sure the packages remain in sync, I recommend installing the
Emacs package first and then running:

    $ pipx install utunes -e --spec ~/.emacs.d/straight/repos/utunes

## Music library format

All metadata for your music library is stored in a single JSON file
called `utunes.json`. Once imported, your music files are
automatically organized into a directory named `media` in the same
directory as `utunes.json`, with the following naming convention:

    media/{album}/[{disk}-]{track}-{song}.{ext}

The album and song names are cleaned up to work nicely as file and
directory names, so you should not expect to reconstruct this data
from the filesystem.

There is an additional directory in the same directory as
`utunes.json`, called `artwork`. This directory contains album artwork
with arbitrary filenames.

The structure of `utunes.json` is as follows. The top level is a map
with keys:

* `version`: integer indicating the version of the data format,
  incremented every time breaking changes are made. Currently 1.
* `songs`: map from *song IDs* to *song objects*.
* `playlists`: map from playlist names (arbitrary strings) to lists of
  *song IDs*.

Song IDs are eight-digit hexadecimal strings which uniquely identify a
song within `utunes.json`. It is thus possible to have a maximum of
4.3 billion songs in a single µTunes library.

Song objects are maps with user-defined keys and values (strings). All
keys starting with an `_` are reserved for use by µTunes, and treated
specially. These are:

* `_id`: *song ID*.
* `_filename`: filesystem path of corresponding media file, relative
  to the `media` directory.
* `_artwork`: filesystem path of corresponding album artwork file,
  relative to the `artwork` directory.
* `_imported`: set to `true` when songs are imported, and can be used
  to identify them for post-import processing.
* `_album`: album name, used in filesystem paths.
* `_song`: song name, used in filesystem paths.
* `_track`: track number, used in filesystem paths.
* `_disc`: disc number, used in filesystem paths.

In the `list` and `update` commands of the backend command-line
interface, song objects appear to have additional `_`-prefixed fields
which do not appear in `utunes.json`. These *virtual fields* are as
follows:

* `_playlist`: name of the playlist in which the song appears (may
  assume different values for the same song object, depending on the
  query passed to `list`).
* `_index`: index at which the song appears in `_playlist`, starting
  from 1.
* `_embed`: if set to `true`, album artwork is re-embedded into the
  ID3 data of the song's file. This is needed whenever you change the
  album artwork of an album.
* `_delete`: if set to `true`, the song is deleted from your library.
  (This occurs by moving it to a `trash` directory in the same
  directory as `utunes.json`, rather than by deleting it from the
  filesystem.)

## Backend command-line interface
### import

    $ utunes import [-r, --recursive] SOURCE...

Import media files into the library. The files are moved, so you
should first make a copy if you wish to preserve the originals. Each
`SOURCE` is a file, unless `-r` is given, in which case directories
are also allowed and are searched recursively. Only files which have a
supported file extension are imported; others are skipped with a
warning printed to stderr.

### list

    $ utunes list
        [-f, --filter FIELD=REGEX]...
        [-s, --sort FIELD]...
        [-r, --reverse FIELD]...
        [-x, --shuffle FIELD]...
        [-i, --illegal-chars CHARS]
        FORMAT

List all songs in your library in an unspecified order. For each song,
the provided `FORMAT` string (which may or may not end with a newline)
is passed to Python's `str.format` with the attributes of the song
object available as substitutions (e.g. `{artist}` or `{_id}`) and then
printed to stdout.

You can filter the output by passing `-f`. In this case, `FIELD` is
something like `artist` or `_id`, and the corresponding value is
required to match the provided Python-style regex in order for the
song object to be included in the output.

You can sort the output by passing `-s`, `-r`, `-x`, or some
combination. `-s` sorts according to the given field; `-r` sorts in
reverse order, and `-x` sorts randomly. Sorts are stable and are
applied right-to-left as given on the command-line. For example,

    $ utunes -s artist -r _album -x _song

would list artists starting with A first; within each artist, it would
list albums starting with Z first; and within each album, it would
list songs in a random order.

The `-i` flag is intended to aid machine parsing of the output of the
`list` command. It causes µTunes to exit with an error if any field of
any song object that is substituted into the format string contains
any character of the given string.

### update

    $ utunes update REGEX [NAME]

Update song metadata, and optionally overwrite the contents of a
playlist. Reads lines from stdin and matches them against the provided
Python-style regex. Each line corresponds to a song object, and the
fields are extracted by means of named capture groups (e.g.
`(?P<artist>)` or `(?P<_id>)`). The regex must contain a capture group
for `_id`.

The `_id` capture group is used to disambiguate song objects. Any
other fields that are provided as capture groups will be taken as
values to overwrite in the song objects on disk. Media files will be
renamed as necessary, and ID3 metadata will be re-embedded as
appropriate.

If `NAME` is provided, then furthermore the contents of the given
playlist are deleted and replaced with the provided song objects in
the given order (identified by `_id`). If no song objects are
provided, then the playlist is deleted.

### play/pause

    $ utunes (play | pause) [-b, --beginning | -e, --end | NAME INDEX]

Start or stop playback. These commands interact with a
transparently-managed persistent background daemon whose identifying
information is written to the file `playback.json` in the same
directory as `utunes.json`. This file contains a JSON map with the
following keys:

* `port`: Port on which the server is listening, integer.
* `playlist`: Name of the playlist which is currently being played,
  string.
* `index`: Index of the song which is currently being played, integer.
  This is updated whenever a new song starts to play, or whenever you
  seek to the beginning of a new song.

Before playing or pausing as appropriate, if the `-b` or `-e` flag is
provided, then µTunes seeks to the beginning of the current or next
song in the playlist, respectively. Alternatively, if `NAME` and
`INDEX` are provided, then before playing or pausing, µTunes seeks to
the beginning of the song at the given index in the given playlist.

## Frontend Emacs interface

The focus of the frontend is on providing a set of composable
primitives which can be used to rapidly develop any desired UI.

### General conventions

`utunes` commands are run asynchronously. Trying to execute another
command while one is already running will result in a user error; an
interactive command is provided in order to kill a running command,
although doing this is not generally recommended since `utunes` does
not guarantee atomicity.

If a `utunes` command is expected to read from stdin or write to
stdout, then its file descriptors will be redirected automatically to
an appropriate Emacs buffer, according to context. If a command fails,
or if it emits warnings, then its output to stderr is displayed in a
shared error buffer and the user is notified via the echo area. The
user is also notified if the command succeeds.

### Variables

    utunes-library-dir

Overrides `default-directory` as the working directory for `utunes`.

### Functions

    (utunes-import RECURSIVE FILES)

Import the given media files or directories using `utunes import`.
`RECURSIVE` is a boolean and `FILES` is a list of filenames relative
to `default-directory` (not `utunes-library-dir`).

    (utunes-list FORMAT &key FILTERS SORTS ILLEGAL-CHARS)

List songs to the current buffer, at point, using `utunes list`.
`FORMAT` is a string. `FILTERS` is an alist mapping field names
(strings) to Python-style regexes (strings). `SORTS` is an alist
mapping field names (strings) to symbols (`sort`, `reverse`, or
`shuffle`). `ILLEGAL-CHARS` is a string.

    (utunes-update REGEX &optional PLAYLIST)

Update song metadata, and optionally overwrite a playlist, reading
data from the current buffer, using `utunes update`. `REGEX` is a
Python-style regex (string), and `PLAYLIST` is a string.

    (utunes-playback CMD &optional SEEK)

Start or stop playback, using `utunes (play | pause)`. `CMD` is a
symbol (`play` or `pause`). `SEEK` is either a symbol (`beginning` or
`end`), or a cons whose car is a playlist name (string) and whose cdr
is a playlist index (integer, starting from 1).

### Interactive commands

    M-x utunes-import-dir

Select a directory using `read-directory-name` and import media files
from it recursively.

    M-x utunes-show-imported

List imported songs into a dedicated buffer.

    M-x utunes-show-album

Select an album using `completing-read`, and list songs into a
dedicated buffer.

    M-x utunes-show-playlist

Select a new or existing playlist using `completing-read` and list
songs into a dedicated buffer.

    M-x utunes-save

Select a currently live dedicated buffer using `completing-read` (if
more than one), run the corresponding update operation, and kill the
buffer.

    M-x utunes-toggle-playback

Play or pause.

    [C-u] M-x utunes-seek-(beginning|end)

Seek to beginning or end of current song, and pause. If a prefix
argument is provided, play instead of pausing.

    [C-u] M-x utunes-seek

Select a playlist and index using `completing-read`, then seek there
and pause. If a prefix argument is provided, play instead of pausing.

[pipx]: https://github.com/pipxproject/pipx
[straight.el]: https://github.com/raxod502/straight.el
