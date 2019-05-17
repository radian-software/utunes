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


def get_nonce(s):
    """
    Return a string that is not a substring of s.
    """
    while True:
        nonce = "".join(random.choices(string.ascii_lowercase, k=4))
        if nonce not in s:
            return nonce


def is_path_occupied(p):
    return p.exists() or p.is_symlink()


def path_walk(p):
    pattern = pathlib.Path(glob.escape(p)) / "**"
    paths = glob.iglob(pattern, recursive=True)
    return (pathlib.Path(p) for p in paths)


class Paths:
    json_basename = pathlib.Path("utunes.json")


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

    def get_json_filename(self):
        return self.library_root / Paths.json_basename

    def commit_changes(self):
        json_fname = self.get_json_filename()
        with atomicwrites.atomic_write(json_fname, overwrite=True) as f:
            json.dump(self.data, f, indent=2)
            f.write("\n")


def extract_fields(format_str):
    raise InternalError("not yet implemented")


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
    nonce = get_nonce(format_str)
    format_str_with_nonce = format_str + "{" + nonce + "}"
    input_str = sys.stdin.read()
    songs = []
    while input_str:
        result = parse.parse(format_str_with_nonce, input_str)
        if res.fixed:
            raise UserError("format string uses anonymous fields: {}"
                            .format(repr(format_str)))
        song = result.named
        song.pop(nonce)
        songs.append(song)
    lib.write(songs=songs, playlist=playlist)
    lib.commit_changes()


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
