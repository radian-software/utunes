import argparse
import glob
import json
import os
import pathlib
import sys

import atomicwrites


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


def is_path_occupied(p):
    return p.exists() or p.is_symlink()


def path_walk(p):
    pattern = pathlib.Path(glob.escape(p)) / "**"
    paths = glob.iglob(pattern, recursive=True)
    return (pathlib.Path(p) for p in paths)


class Paths:
    json_basename = pathlib.Path("utunes.json")


class Library:

    def __init__(self, library_root):
        self.library_root = library_root
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

    def import_from_file(self, fname):
        raise InternalError("not yet implemented")

    def commit_changes(self):
        json_fname = self.get_json_filename()
        with atomicwrites.atomic_write(json_fname, overwrite=True) as f:
            json.dump(self.data, f, indent=2)
            f.write("\n")


def find_library_root():
    cwd = pathlib.Path(".").resolve()
    for directory in (cwd, *cwd.parents):
        if is_path_occupied(directory / Paths.json_basename):
            return directory
    raise UserError("could not find {}".format(Paths.json_basename))


def subcmd_init():
    json_fname = Paths.json_basename
    if is_path_occupied(json_fname):
        raise UserError("library already initialized: {}"
                        .format(repr(json_fname)))
    library_root = json_fname.resolve().parent
    lib = Library(library_root)
    lib.commit_changes()
    print("Initialized µTunes library in {}"
          .format(library_root), file=sys.stderr)


EXTENSIONS = (".mp3")


def subcmd_import(sources, recursive):
    lib = Library(find_library_root())
    files = []
    for source in sources:
        if source.is_file():
            if source.suffix not in EXTENSIONS:
                raise UserError("file has unsupported suffix: {}"
                                .format(source))
            files.append(source)
        elif source.is_dir():
            if not recursive:
                raise UserError(
                    "cannot import directory without --recursive: {}"
                    .format(source)
                )
            filtered_paths = []
            for path in path_walk(source):
                if not path.is_file():
                    continue
                if path.suffix not in EXTENSIONS:
                    continue
                filtered_paths.append(path)
            if not filtered_paths:
                raise UserError("no media files in directory: {}"
                                .format(source))
            files.extend(filtered_paths)
        else:
            raise UserError("not a file or directory: {}"
                            .format(source))
    for f in files:
        print("Importing: {}".format(f), file=sys.stderr)
        lib.import_from_file(f)
    print("Imported {} files", file=sys.stderr)


def subcmd_list(filters, sorts, illegal_chars, format_str):
    raise InternalError("not yet implemented")


def subcmd_update(regex, playlist):
    raise InternalError("not yet implemented")


def subcmd_playback(cmd, seek):
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

    subparsers.add_parser(
        "init", help="initialize a new library in the current directory"
    )

    parser_import = subparsers.add_parser(
        "import", help="add media files to library"
    )
    parser_import.add_argument("-r", "--recursive", action="store_true",
                               help="allow importing directories recursively")
    parser_import.add_argument("sources", nargs="+", metavar="SOURCE",
                               help="media file or directory")

    parser_list = subparsers.add_parser(
        "list", help="list media files from library to stdout"
    )
    parser_list.add_argument("-f", "--filter", dest="filters",
                             action="append", metavar="FIELD=REGEX",
                             help="filter songs by the given field")
    parser_list.add_argument("-s", "--sort", dest="sorts",
                             action="append", metavar="[s:|r:|x:]FIELD",
                             help="sort songs by the given field ('r:' for reverse, 'x:' for shuffle)")
    parser_list.add_argument(
        "-i", "--illegal-chars", default="", metavar="CHARS",
        help="report an error if song fields contain the given characters"
    )
    parser_list.add_argument(
        "format", metavar="FORMAT",
        help="Python str.format string for listing output"
    )

    parser_update = subparsers.add_parser(
        "update", help="update song metadata and playlists from stdin"
    )
    parser_update.add_argument(
        "regex", metavar="REGEX", help="regex for parsing input"
    )
    parser_update.add_argument(
        "playlist", nargs="?", default=UNSET,
        metavar="PLAYLIST", help="name of playlist to update"
    )

    for cmd in ("play", "pause"):
        parser_playback = subparsers.add_parser(
            cmd, help="{} music, optionally seeking first".format(cmd)
        )
        group_seek = parser_playback.add_mutually_exclusive_group()
        group_seek.add_argument("-b", "--beginning", action="store_true",
                                help="seek to beginning of current song")
        group_seek.add_argument("-e", "--end", action="store_true",
                                help="seek to beginning of next song")
        group_seek.add_argument("-p", "--playlist", metavar="PLAYLIST:INDEX",
                                help="seek to one-based index in given playlist")

    args = parser.parse_args()

    try:
        if args.cd_dir is not UNSET:
            try:
                os.chdir(args.cd_dir)
            except OSError as e:
                raise UserError("couldn't change directory to {}: {}"
                                .format(repr(args.cd_dir), e)) from None

        if args.subcommand == "init":
            subcmd_init()
        elif args.subcommand == "import":
            sources = [pathlib.Path(source) for source in args.sources]
            subcmd_import(sources=sources, recursive=args.recursive)
        elif args.subcommand == "list":
            filters = []
            for filter_str in args.filters:
                try:
                    field, regex = filter_str.split("=")
                except ValueError:
                    parser_list.error("malformed filter string: {}".format(repr(field)))
                filters.append((field, regex))
            sorts = []
            for sort_str in args.sorts:
                if len(sort_str) >= 2 and sort_str[1] == ":":
                    qualifier = sort_str[0]
                    field = sort_str[2:]
                else:
                    qualifier = "s"
                    field = sort_str
                if not field or qualifier not in "srx":
                    parser_list.error("malformed sort string: {}".format(repr(sort_str)))
                sorts.append((qualifier, field))
            subcmd_list(
                filters=filters, sorts=sorts,
                illegal_chars=args.illegal_chars,
                format_str=args.format,
            )
        elif args.subcommand == "update":
            subcmd_update(regex=args.regex, playlist=args.playlist)
        elif args.subcommand in ("play", "pause"):
            seek = UNSET
            if args.beginning:
                seek = "beginning"
            elif args.end:
                seek = "end"
            elif args.playlist:
                try:
                    playlist, index = args.playlist.split(":")
                    index = int(index)
                    if index <= 0:
                        raise ValueError
                except ValueError:
                    parser.error("malformed playlist string: {}"
                                 .format(repr(args.playlist)))
                seek = (playlist, index)
            else:
                raise InternalError
            subcmd_playback(cmd=args.subcommand, seek=seek)
        else:
            raise InternalError
    except UserError as e:
        print("utunes: {}".format(e), file=sys.stderr)
        sys.exit(1)
    sys.exit(0)
