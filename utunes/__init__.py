import argparse
import glob
import json
import os
import pathlib
import random
import re
import string
import sys

import atomicwrites
import parse


class UnsetClass:
    """
    Singleton class used to implement `UNSET`.
    """

    def __repr__(self):
        return "<Unset>"


# Singleton object that can be used to indicate "no value".
UNSET = UnsetClass()


class UserError(Exception):
    pass


class InternalError(Exception):
    pass


def get_nonce(k=4, s=UNSET, l=UNSET):
    """
    Return a random string of length k. If s is provided, the returned
    string is not `in` s.
    """
    while True:
        nonce = "".join(random.choices(string.ascii_lowercase, k=4))
        if s is UNSET or nonce not in s:
            return nonce


def is_path_occupied(p):
    try:
        p.stat()
        return True
    except FileNotFoundError:
        return False


def path_walk(p):
    pattern = pathlib.Path(glob.escape(p)) / "**"
    paths = glob.iglob(pattern, recursive=True)
    return (pathlib.Path(p) for p in paths)


class Paths:
    json_basename = pathlib.Path("utunes.json")
    music_basename = pathlib.Path("music")


class Library:

    @staticmethod
    def find_library_root():
        env_root = os.environ.get("UTUNES_LIBRARY")
        if env_root:
            return pathlib.Path(env_root).resolve()
        cwd = pathlib.Path(".").resolve()
        for directory in (cwd, *cwd.parents):
            if is_path_occupied(directory / Paths.json_basename):
                return directory
        raise UserError("could not find {}, and UTUNES_LIBRARY not set"
                        .format(Paths.json_basename))

    @staticmethod
    def prettify_path(path):
        return "-".join(re.findall(r"[a-z0-9]+", path.lower()))

    @staticmethod
    def get_canonical_filename(song):
        album = prettify_path(song.get("album", "untitled"))
        disc = song.get("disc", "")
        if disc:
            disc = prettify_path("{:02s}-".format(disc))
        track = song.get("track", "")
        if track:
            track = prettify_path("{:03s}-".format(track))
        song_id = prettify_path(song["id"])
        title = song.get("title", "untitled")
        suffix = pathlib.Path(song["filename"]).suffix
        filename = disc + track + song_id + " " + title + suffix
        return pathlib.Path(album) / filename

    @staticmethod
    def filters_to_function(filters):
        def check_song(song):
            for field, regex in filters:
                if field not in song:
                    return False
                return re.fullmatch(regex, song[field])
        return check_song

    def get_json_filename(self):
        return self.library_root / Paths.json_basename

    def get_music_dirname(self):
        return self.library_root / Paths.music_basename

    def __init__(self):
        self.library_root = Library.find_library_root()
        json_fname = self.get_json_filename()
        self.data = UNSET
        if is_path_occupied(json_fname):
            try:
                with open(json_fname) as f:
                    self.data = json.load(f)
            except (OSError, json.JSONDecodeError) as e:
                raise UserError(
                    "error reading library file: {}".format(e)
                ) from None
        # TODO: validate data format
        if self.data is UNSET:
            self.data = {
                "version": 1,
                "songs": {},
                "playlists": {},
            }

    def read(self, filters, sorts):
        if self.updated_song_ids:
            raise InternalError("read between write and commit")
        if any(field == "playlist" for field, regex in filters):
            raise InternalError("not yet implemented")
        check_song = Library.filters_to_function(filters)
        songs = list(filter(check_song, self.data["songs"].values()))
        for field, mode in reversed(sorts):
            if mode in "sr":
                key = id
            elif mode in "SR":
                def key(val):
                    try:
                        return False, int(val)
                    except ValueError:
                        return True, val
            elif mode == "x":
                keys = list({songs[field] for song in songs})
                random.shuffle(keys)
                values = {idx: key for idx, key in keys.enumerate()}
                def key(val):
                    return values[val]
            reverse = mode in "rR"
            songs.sort(key=key, reverse=reverse)
        return songs

    def write(partial_songs, playlist):
        songs = self.data["songs"]
        for partial_song in partial_songs:
            song_id = partial_song.get("id")
            if song_id:
                if song_id not in songs:
                    raise UserError("no such song: id {}".format(repr(song_id)))
                song = songs[song_id]
                for key, value in partial_song.items():
                    if value:
                        song.update(key=value)
                    else:
                        if key == "filename":
                            raise UserError("cannot unset filename: id {}"
                                            .format(repr(song_id)))
                        song.pop(key)
            else:
                song_id = get_nonce(k=8, s=songs)
                song = {"id": song_id, **partal_song}
                songs[song_id] = song
            self.updated_song_ids.add(song_id)
        renames = {}
        for song_id in self.updated_songs:
            song = self.data["songs"][song_id]
            old_filename = pathlib.Path(song["filename"])
            new_filename = Library.get_canonical_filename(song)
            if new_filename == old_filename:
                continue
            renames[Library.get_music_dirname(old_filename)] = (
                Library.get_music_dirname(new_filename)
            )
        for old_filename, new_filename in renames.items():
            if not old_filename.is_file():
                raise UserError("no such file: {}".format(old_filename))
            if is_path_occupied(new_filename):
                raise UserError("already exists: {}".format(new_filename))
        for old_filename, new_filename in renames.items():
            new_filename.parent.mkdir(parents=True, exist_ok=True)
        print("µTunes: moving {} files".format(len(renames)), file=sys.stderr)
        for old_filename, new_filename in renames.items():
            old_filename.rename(new_filename)
        json_fname = self.get_json_filename()
        with atomicwrites.atomic_write(json_fname, overwrite=True) as f:
            json.dump(self.data, f, indent=2)
            f.write("\n")


def extract_fields(format_str):
    return [field for _, field, _, _ in string.Formatter().parse(format_str) if field]


def subcmd_read(filters, sorts, illegal_chars, format_str):
    lib = Library()
    output = []
    fields = extract_fields(format_str)
    for song in lib.read(filters=filters, sorts=sorts):
        for field in fields:
            if field not in song:
                song[field] = ""
            else:
                for char in illegal_chars:
                    if char in song[field]:
                        raise UserError(
                            "song contains {} in field {}: {}"
                            .format(
                                repr(char), repr(field), repr(song["filename"])
                            )
                        )
        output.append(format_str.format(**song))
    print("".join(output))


def subcmd_write(format_str, playlist):
    lib = Library()
    nonce = get_nonce(s=format_str)
    format_str_with_nonce = format_str + "{" + nonce + "}"
    input_str = sys.stdin.read()
    partial_songs = []
    while input_str:
        result = parse.parse(format_str_with_nonce, input_str)
        if res.fixed:
            raise UserError("format string uses anonymous fields: {}"
                            .format(repr(format_str)))
        partial_song = result.named
        partial_song.pop(nonce)
        partial_songs.append(partial_song)
    lib.write(partial_songs=partial_songs, playlist=playlist)


def subcmd_playback():
    raise InternalError("not yet implemented")


def main():
    parser = argparse.ArgumentParser(
        prog="utunes",
        description="Microscopic music library manager and music player"
    )

    parser.add_argument(
        "--version", action="version", version="µTunes pre-release version"
    )
    parser.add_argument(
        "-C", "--cd", dest="cd_dir", default=UNSET, metavar="DIR",
        help="change to given directory before running",
    )

    subparsers = parser.add_subparsers(dest="subcommand", required=True)

    parser_list = subparsers.add_parser(
        "read", help="list media files from library to stdout"
    )
    parser_list.add_argument(
        "format", metavar="FORMAT",
        help="Python str.format string for listing output"
    )
    parser_list.add_argument("-f", "--filter", dest="filters",
                             action="append", metavar="FIELD=REGEX",
                             help="filter songs by the given field")
    parser_list.add_argument("-s", "--sort", dest="sorts",
                             action="append", metavar="Q:FIELD",
                             help="sort songs by the given field (Q is one of srxSR)")
    parser_list.add_argument(
        "-i", "--illegal-chars", default="", metavar="CHARS",
        help="report an error if song fields contain the given characters"
    )

    parser_update = subparsers.add_parser(
        "write", help="update song metadata and playlists from stdin"
    )
    parser_update.add_argument(
        "format", metavar="FORMAT",
        help="Python str.format string for parsing input"
    )
    parser_update.add_argument(
        "playlist", nargs="?", default=UNSET,
        metavar="PLAYLIST", help="name of playlist to update"
    )

    subparsers.add_parser(
        "playback", help="read and write music playback server state"
    )

    args = parser.parse_args()

    try:
        if args.cd_dir is not UNSET:
            try:
                os.chdir(args.cd_dir)
            except OSError as e:
                raise UserError("couldn't change directory to {}: {}"
                                .format(repr(args.cd_dir), e)) from None

        if args.subcommand == "read":
            filters = []
            for filter_str in args.filters:
                try:
                    field, regex = filter_str.split("=")
                except ValueError:
                    parser_list.error("malformed filter string: {}".format(repr(field)))
                filters.append((field, regex))
            sorts = []
            for sort_str in args.sorts:
                match = re.fullmatch(r"([srxSR]):(.*)", sort_str)
                if not match:
                    parser_list.error("malformed sort string: {}".format(repr(sort_str)))
                qualifier, field = match.groups()
                sorts.append((qualifier, field))
            subcmd_read(
                filters=filters, sorts=sorts,
                illegal_chars=args.illegal_chars,
                format_str=args.format,
            )
        elif args.subcommand == "write":
            subcmd_write(format_str=args.format, playlist=args.playlist)
        elif args.subcommand == "playback":
            subcmd_playback()
        else:
            raise InternalError
    except UserError as e:
        print("utunes: {}".format(e), file=sys.stderr)
        sys.exit(1)
    sys.exit(0)
