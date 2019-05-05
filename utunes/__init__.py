import argparse
import atomicwrites
import json
import sys


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


class Library:

    def __init__(self, library_root):
        self.library_root = library_root
        json_fname = self.get_json_filename()
        self.data = UNSET
        if json_fname.exists() or json_fname.is_symlink():
            try:
                with open(json_fname) as f:
                    self.data = json.load(f)
            except (OSError, json.JSONDecodeError) as e:
                raise UserError(
                    "error reading library file: {}".format(e)
                ) from None
        # TODO: validate data format

    def get_json_filename(self):
        return self.library_root / "utunes.json"

    def commit_changes(self):
        json_fname = self.get_json_filename()
        with atomicwrites.atomic_write(json_fname, overwrite=True) as f:
            json.dump(self.data, f, indent=2)
            f.write("\n")


def subcmd_import(sources, recursive):
    raise InternalError("not yet implemented")


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

    subparsers = parser.add_subparsers(dest="subcommand", required=True)

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
        if args.subcommand == "import":
            subcmd_import(sources=args.sources, recursive=args.recursive)
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
