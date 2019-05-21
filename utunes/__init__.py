import argparse
import glob
import json
import multiprocessing.connection
import os
import pathlib
import random
import re
import shutil
import string
import subprocess
import sys
import time

import atomicwrites
import portalocker
import psutil


class UnsetClass:
    """
    Singleton class used to implement `UNSET`.
    """

    def __repr__(self):
        return "<Unset>"


# Singleton object that can be used to indicate "no value".
UNSET = UnsetClass()


def log(msg):
    print("µTunes: {}".format(msg), file=sys.stderr)


class UserError(Exception):
    pass


class InternalError(Exception):
    pass


def get_nonce(k=4, s=UNSET, alphabet=string.ascii_lowercase):
    """
    Return a random string of length k using letters from the given
    alphabet. If s is provided, the returned string is not `in` s.
    """
    while True:
        nonce = "".join(random.choices(alphabet, k=k))
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


def read_stdin():
    try:
        return sys.stdin.read()
    except OSError as e:
        raise UserError("failed to read stdin: {}".format(e))


class Paths:
    json_basename = pathlib.Path("utunes.json")
    music_basename = pathlib.Path("music")
    socket_basename = pathlib.Path("socket")
    server_log_basename = pathlib.Path("server.log")


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
        album = Library.prettify_path(song.get("album", "untitled"))
        disc = song.get("disc", "")
        if disc:
            disc = Library.prettify_path(disc.zfill(2)) + "-"
        track = song.get("track", "")
        if track:
            track = Library.prettify_path(track.zfill(3)) + "-"
        song_id = Library.prettify_path(song["id"])
        title = Library.prettify_path(song.get("song", "untitled"))
        suffix = pathlib.Path(song["filename"]).suffix
        filename = disc + track + song_id + "-" + title + suffix
        return pathlib.Path(album) / filename

    @staticmethod
    def filters_to_function(filters):
        def check_song(song):
            for field, regex in filters:
                if field not in song:
                    return False
                if not re.fullmatch(regex, song[field]):
                    return False
            return True
        return check_song

    def get_json_filename(self):
        return self.library_root / Paths.json_basename

    def get_music_dirname(self):
        return self.library_root / Paths.music_basename

    def get_socket_filename(self):
        return self.library_root / Paths.socket_basename

    def get_server_log_filename(self):
        return self.library_root / Paths.server_log_basename

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
                )
        if self.data is UNSET:
            self.data = {
                "version": 1,
                "songs": {},
                "playlists": {},
            }

    def get_song_filename(self, playlist, index):
        playlist = self.data["playlists"].get(playlist)
        if playlist is None:
            return UNSET
        try:
            filename = self.data["songs"][playlist[index]]["filename"]
            return self.get_music_dirname() / filename
        except (ValueError, IndexError):
            return UNSET

    def read(self, filters, sorts):
        check_song = Library.filters_to_function(filters)
        if any(field == "playlist" for field, regex in filters):
            filtered_songs = {}
            for playlist_name, song_ids in self.data["playlists"].items():
                for idx, song_id in enumerate(song_ids):
                    if song_id in filtered_songs:
                        continue
                    song = self.data["songs"][song_id]
                    song_copy = dict(song)
                    song_copy["playlist"] = "{}:{}".format(playlist_name, idx)
                    if check_song(song_copy):
                        filtered_songs[song_id] = song
            songs = list(filtered_songs.values())
        else:
            songs = list(filter(check_song, self.data["songs"].values()))
        for field, mode in reversed(sorts):
            if mode in "sr":
                def key(song):
                    return song[field]
            elif mode in "SR":
                def key(song):
                    try:
                        return False, int(song[field])
                    except ValueError:
                        return True, song[field]
            elif mode == "x":
                values = list({song[field] for song in songs})
                random.shuffle(values)
                values = {value: idx for idx, value in enumerate(values)}

                def key(song):
                    return values[song[field]]
            else:
                raise InternalError("unexpected mode: " + mode)
            reverse = mode in "rR"

            songs.sort(key=key, reverse=reverse)
        return songs

    def write(self, partial_songs, playlist):
        songs = self.data["songs"]
        updated_song_ids = []
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
                song_id = get_nonce(k=8, s=songs, alphabet="0123456789abcdef")
                song = {"id": song_id}
                for field, value in partial_song.items():
                    if value:
                        song[field] = value
                songs[song_id] = song
            updated_song_ids.append(song_id)
        renames = {}
        for song_id in set(updated_song_ids):
            song = self.data["songs"][song_id]
            old_filename = pathlib.Path(song["filename"])
            new_filename = Library.get_canonical_filename(song)
            song["filename"] = str(new_filename)
            if new_filename == old_filename:
                continue
            renames[self.get_music_dirname() / old_filename] = (
                self.get_music_dirname() / new_filename
            )
        songs = list(self.data["songs"].values())
        songs.sort(key=lambda s: s["filename"])
        self.data["songs"] = {song["id"]: song for song in songs}
        if playlist is not UNSET:
            if updated_song_ids:
                self.data["playlists"][playlist] = updated_song_ids
            else:
                try:
                    self.data["playlists"].pop(playlist)
                except KeyError:
                    pass
        playlists = list(self.data["playlists"].items())
        playlists.sort(key=lambda i: i[0])
        self.data["playlists"] = dict(playlists)
        for old_filename, new_filename in renames.items():
            if not old_filename.is_file():
                raise UserError("no such file: {}".format(old_filename))
            if is_path_occupied(new_filename):
                raise UserError("already exists: {}".format(new_filename))
        for old_filename, new_filename in renames.items():
            new_filename.parent.mkdir(parents=True, exist_ok=True)
        if renames:
            log(
                "renaming {} file{}"
                .format(len(renames), "s" if len(renames) != 1 else "")
            )
        for old_filename, new_filename in renames.items():
            old_filename.rename(new_filename)
        log("writing library database")
        json_fname = self.get_json_filename()
        with portalocker.Lock(json_fname, "r"):
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
    if output:
        print("".join(output), end="")
    else:
        log("no matching songs")


def subcmd_write(regex, playlist):
    lib = Library()
    input_str = read_stdin()
    location = 0
    partial_songs = []
    while input_str:
        match = re.match(regex, input_str)
        if not match:
            if len(input_str) > 40:
                input_str = input_str[:40] + " (...)"
            raise UserError(
                "regex does not match at location {}: {}".format(
                    location, input_str
                )
            )
        if len(match.groupdict()) < len(match.groups()):
            raise UserError("regex contains unnamed groups: {}"
                            .format(regex))
        input_str = input_str[match.end():]
        location += match.end()
        partial_songs.append(match.groupdict())
    lib.write(partial_songs=partial_songs, playlist=playlist)


class BindFailedError(Exception):
    pass


def call_server(socket_fname, msg):
    try:
        conn = multiprocessing.connection.Client(str(socket_fname))
    except OSError as e:
        raise BindFailedError("failed to bind to socket: {}".format(e))
    with conn:
        try:
            conn.send(msg)
            resp = conn.recv()
        except ValueError as e:
            raise InternalError(
                "failed to send to playback server: {}".format(e)
            )
        except EOFError:
            raise InternalError(
                "did not receive response from playback server"
            )
    error = resp.pop("error")
    async_errors = resp.pop("async_errors")
    for e in async_errors:
        log("error from server: {}".format(e))
    if error:
        raise UserError(error)
    return resp


def is_server_live(socket_fname):
    try:
        call_server(socket_fname, {})
    except BindFailedError:
        return False
    return True


def kill_server():
    for proc in psutil.process_iter():
        try:
            if proc.cmdline() == ["python", "-m", "utunes.server"]:
                proc.terminate()
        except psutil.Error:
            pass


def subcmd_playback():
    if not shutil.which("mplayer"):
        raise UserError("command not found: mplayer")
    lib = Library()
    msg = read_stdin()
    try:
        msg = json.loads(msg)
    except json.JSONDecodeError:
        raise UserError("got malformed JSON from stdin: {}".format(repr(msg)))
    socket_fname = lib.get_socket_filename()
    if not is_server_live(socket_fname):
        kill_server()
        try:
            with open(lib.get_server_log_filename(), "a") as f:
                f.write("---\nStarting playback server...\n")
                env = dict(os.environ)
                env["PYTHONUNBUFFERED"] = "1"
                subprocess.Popen(["nohup", "python", "-m", "utunes.server"],
                                 preexec_fn=os.setpgrp, cwd=lib.library_root,
                                 stdout=f, stderr=f, env=env)
        except OSError as e:
            raise InternalError("failed to spawn playback server: {}" .format(e))
        # Wait up to one second for the server to start.
        live = False
        for i in range(20):
            time.sleep(0.05)
            if is_server_live(socket_fname):
                live = True
                break
        if not live:
            raise InternalError(
                "playback server failed to bind to socket within 1 second"
            )
    try:
        resp = call_server(socket_fname, msg)
    except BindFailedError as e:
        raise InternalError(e)
    json.dump(resp, sys.stdout, indent=2)
    print()


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

    parser_read = subparsers.add_parser(
        "read", help="list media files from library to stdout"
    )
    parser_read.add_argument(
        "format", metavar="FORMAT",
        help="Python str.format string for listing output"
    )
    parser_read.add_argument("-f", "--filter", dest="filters", default=[],
                             action="append", metavar="FIELD=REGEX",
                             help="filter songs by the given field")
    parser_read.add_argument("-s", "--sort", dest="sorts", default=[],
                             action="append", metavar="Q:FIELD",
                             help="sort songs by the given field (Q is one of srxSR)")
    parser_read.add_argument(
        "-i", "--illegal-chars", default="", metavar="CHARS",
        help="report an error if song fields contain the given characters"
    )

    parser_write = subparsers.add_parser(
        "write", help="update song metadata and playlists from stdin"
    )
    parser_write.add_argument(
        "regex", metavar="REGEX",
        help="Python regex for parsing input"
    )
    parser_write.add_argument(
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
                                .format(repr(args.cd_dir), e))

        if args.subcommand == "read":
            filters = []
            for filter_str in args.filters:
                try:
                    field, regex = filter_str.split("=")
                except ValueError:
                    parser_read.error("malformed filter string: {}".format(repr(field)))
                filters.append((field, regex))
            sorts = []
            for sort_str in args.sorts:
                match = re.fullmatch(r"([srxSR]):(.*)", sort_str)
                if not match:
                    parser_read.error("malformed sort string: {}".format(repr(sort_str)))
                qualifier, field = match.groups()
                sorts.append((field, qualifier))
            subcmd_read(
                filters=filters, sorts=sorts,
                illegal_chars=args.illegal_chars,
                format_str=args.format,
            )
        elif args.subcommand == "write":
            try:
                regex = re.compile(args.regex)
            except re.error as e:
                parser_write.error("invalid regex: {}".format(e))
            subcmd_write(regex=regex, playlist=args.playlist)
        elif args.subcommand == "playback":
            subcmd_playback()
        else:
            raise InternalError
    except UserError as e:
        log(e)
        sys.exit(1)
    sys.exit(0)
