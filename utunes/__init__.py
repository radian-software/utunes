import argparse
import sys


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
    parser_list.add_argument("-f", "--filter", action="append", metavar="FIELD=REGEX",
                             help="filter songs by the given field")
    parser_list.add_argument("-s", "--sort", action="append", metavar="[QUALIFIER:]FIELD",
                             help="sort songs by the given field ('r:' for reverse, 'x:' for shuffle)")
    parser_list.add_argument(
        "-i", "--illegal-chars", metavar="CHARS",
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
        "playlist", nargs="?", metavar="PLAYLIST", help="name of playlist to update"
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
    sys.exit(0)
