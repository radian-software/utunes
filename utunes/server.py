import abc
import argparse
import atexit
import multiprocessing.connection
import os
import re
import signal
import subprocess
import sys
import threading

import utunes
from utunes import UNSET
import utunes.nbstreamreader


def log(msg):
    print("µTunes server: {}".format(msg), file=sys.stderr)


def make_error(msg):
    return {
        "error": msg,
    }


def is_debug_enabled():
    return bool(os.environ.get("UTUNES_SERVER_DEBUG"))


class Player(abc.ABC):
    """
    Abstraction of a music player. At most one song can be loaded at a
    time, and the player can be either playing or paused, with a
    well-defined seek position in either case. After a song finishes
    playing, the user-provided callback is invoked. To continue
    playback, or to seek backwards in the song, a new song must be
    loaded.
    """

    def __init__(self, callback, log_error):
        """
        Construct a new music player with no media file loaded. The
        `callback` is invoked with no arguments every time a media
        file *finishes* playing. The `log_error` function should take
        a positional argument (the error message, as a string) and a
        keyword argument `fatal` (whether or not the player can
        continue to be used, a boolean); this function is called when
        the player encounters an error.
        """
        super().__init__()
        self.callback = callback
        self.log_error = log_error

    @abc.abstractmethod
    def load(self, filename):
        """
        Load a media file from the given filename, either a string or a
        Path. The previously loaded media file, if any, is discarded.
        The music player is paused, and playback will start from the
        beginning of the new file.
        """
        pass

    @abc.abstractmethod
    def unload(self):
        """
        Unload the currently loaded media file, if any. Playback is
        stopped.
        """
        pass

    @abc.abstractmethod
    def play(self):
        """
        If a media file is currently loaded, start playback. If the music
        player was previously paused, playback starts from there. If
        `seek` was called since the last time the music player was
        playing, playback starts from there. Otherwise, playback
        starts from the beginning of the file. If the music player is
        already playing, do nothing.
        """
        pass

    @abc.abstractmethod
    def pause(self):
        """
        If the music player is currently playing, pause it. Otherwise, do
        nothing.
        """
        pass

    @abc.abstractmethod
    def is_playing(self):
        """
        Check whether the music player is currently playing. Return a
        boolean.
        """
        pass

    @abc.abstractmethod
    def seek(self, pos):
        """
        Seek to the given position, in seconds. If this position is after
        the end of the song, then the callback is invoked and playback
        is terminated. If it is before the beginning of the song, it
        is clamped to the beginning.
        """
        pass

    @abc.abstractmethod
    def tell(self):
        """
        Return the current seek position, in seconds, or UNSET if no song
        is loaded.
        """
        pass

    @abc.abstractmethod
    def tell_end(self):
        """
        Return the maximum possible seek position, in seconds, or UNSET if
        no song is loaded.
        """
        pass


class MPlayerPlayer(Player):

    def die(self, msg):
        self.log_error(msg)
        sys.exit(1)

    def __init__(self, callback, log_error):
        super().__init__(callback, log_error)
        try:
            self.proc = subprocess.Popen(
                [
                    "mplayer", "-slave", "-idle", "-quiet",
                    # Recommended in the "docs" at
                    # <http://www.mplayerhq.hu/DOCS/tech/slave.txt>.
                    "-input", "nodefault-bindings", "-noconfig", "all",
                ],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                # Use line buffering.
                bufsize=0,
            )
        except OSError as e:
            self.die("failed to spawn mplayer: {}".format(e))
        self.stdin = self.proc.stdin
        self.stdout = utunes.nbstreamreader.NonBlockingStreamReader(
            self.proc.stdout,
        )
        self.lock = threading.Lock()
        self.last_pos = UNSET
        self.ticket = UNSET

    def runcmd(self, cmd, ans=UNSET, optional=False, timeout=0.1, quoted=False):
        with self.lock:
            if is_debug_enabled():
                print("> " + cmd, file=sys.stderr)
            try:
                self.stdin.write((cmd + "\n").encode())
            except OSError as e:
                self.die("failed to write to mplayer stdin: {}".format(e))
            resp = UNSET
            while True:
                line = self.stdout.readline(timeout=timeout)
                if line is None:
                    break
                line = line.decode()
                if not line:
                    self.die("mplayer stdout closed unexpectedly")
                if is_debug_enabled():
                    print(line, end="", file=sys.stderr)
                if ans is not UNSET:
                    if quoted:
                        fmt = r"ANS_{}='(.+)'\n"
                    else:
                        fmt = r"ANS_{}=(.+)\n"
                    m = re.fullmatch(fmt.format(ans), line)
                    if m:
                        resp = m.group(1)
            if ans is not UNSET and resp is UNSET and not optional:
                self.die("mplayer failed to respond to command: {}"
                         .format(cmd))
            return resp

    def get_float(self, cmd, ans):
        resp = self.runcmd(cmd, ans=ans, optional=True)
        if resp is UNSET:
            return UNSET
        try:
            return float(resp)
        except ValueError:
            self.die("non-numeric {} from mplayer: {}".format(ans, repr(resp)))

    def get_time_pos(self):
        return self.get_float(
            cmd="pausing_keep_force get_time_pos", ans="TIME_POSITION",
        )

    def check_if_done(self, ticket):
        if ticket is not self.ticket:
            return
        new_pos = self.get_time_pos()
        finished = new_pos is UNSET and self.last_pos is not UNSET
        self.last_pos = new_pos
        if finished:
            self.callback()
        else:
            timer = threading.Timer(0.1, self.check_if_done, args=(ticket,))
            timer.daemon = True
            timer.start()

    def load(self, filename):
        self.runcmd("pausing_keep_force loadfile {}".format(filename))
        self.ticket = object()

    def unload(self):
        self.load(os.devnull)

    def play(self):
        if not self.is_playing():
            self.runcmd("pause")
            self.check_if_done(self.ticket)

    def pause(self):
        if self.is_playing():
            self.runcmd("pause")
            self.ticket = object()

    def is_playing(self):
        paused = self.runcmd(
            "pausing_keep_force get_property pause", ans="pause"
        )
        if paused not in ("yes", "no"):
            self.die("unexpected pause state from mplayer: {}"
                     .format(repr(paused)))
        return paused == "no"

    def seek(self, pos):
        self.runcmd("pausing_keep seek {} 2".format(pos))

    def tell(self):
        return self.get_time_pos()

    def tell_end(self):
        return self.get_float(
            cmd="pausing_keep_force get_time_length", ans="LENGTH"
        )


class Server:

    def __init__(self, playlist=UNSET, index=UNSET, seek=UNSET):
        self.lock = threading.Lock()
        self.player = MPlayerPlayer(self.advance_song, self.log_error)
        self.playlist = UNSET
        self.index = UNSET
        self.last_json_mtime = UNSET
        self.next_song_filename = UNSET
        self.errors = []
        self.lib = utunes.Library()
        os.chdir(self.lib.library_root)

    def update(self, playlist=UNSET, index=UNSET, seek=UNSET, playing=UNSET):
        switching_playlists = playlist is not UNSET and playlist != self.playlist
        switching_indices = index is not UNSET and index != self.index
        switching_songs = playlist is not UNSET or index is not UNSET
        switching_to_next = (
            switching_indices and not switching_playlists and
            self.index is not UNSET and
            index == self.index + 1
        )
        if switching_playlists:
            self.playlist = playlist
        if switching_indices:
            self.index = index
        if switching_songs:
            if (
                    switching_to_next and
                    self.next_song_filename is not UNSET and
                    self.last_json_mtime is not UNSET and
                    self.lib.get_json_filename().stat().st_mtime ==
                    self.last_json_mtime
            ):
                filename = self.next_song_filename
            elif self.playlist is not UNSET and self.index is not UNSET:
                filename = self.lib.get_song_filename(self.playlist, self.index)
            else:
                filename = UNSET
            if filename is not UNSET:
                self.player.load(filename)
                t = threading.Thread(target=self.compute_next_song)
                t.start()
            else:
                self.player.unload()
        if seek is not UNSET:
            self.player.seek(seek)
        if playing is not UNSET:
            if playing:
                self.player.play()
            else:
                self.player.pause()

    def compute_next_song(self):
        with self.lock:
            self.last_json_mtime = self.lib.get_json_filename().stat().st_mtime
            filename = self.lib.get_song_filename(self.playlist, self.index + 1)
            self.next_song_filename = filename

    def advance_song(self):
        with self.lock:
            if self.playlist is not UNSET and self.index is not UNSET:
                self.update(self.playlist, self.index + 1)

    def handle_msg(self, msg):
        if not isinstance(msg, dict):
            return make_error("not an object: {}".format(repr(msg)))
        playlist = msg.get("playlist", UNSET)
        index = msg.get("index", UNSET)
        seek = msg.get("seek", UNSET)
        playing = msg.get("playing", UNSET)
        if playlist is not UNSET:
            if not isinstance(playlist, str):
                return make_error("not a string: {}".format(repr(playlist)))
        if index is not UNSET:
            if not isinstance(index, int):
                return make_error("not an integer: {}".format(repr(index)))
            if index < 0:
                return make_error("index is negative: {}".format(repr(index)))
        if seek is not UNSET:
            if not isinstance(seek, (int, float)):
                return make_error("not a number: {}".format(repr(seek)))
            if seek < 0:
                return make_error("seek is negative: {}".format(repr(seek)))
        if playing is not UNSET:
            if playing not in (True, False, "toggle"):
                return make_error(
                    "unknown 'playing' value: {}".format(repr(playing))
                )
        with self.lock:
            self.update(playlist, index, seek, playing)
            playlist = self.playlist if self.playlist is not UNSET else None
            index = self.index if self.index is not UNSET else None
            seek = self.player.tell()
            if seek is UNSET:
                seek = None
            seek_end = self.player.tell_end()
            if seek_end is UNSET:
                seek_end = None
            resp = {
                "error": None,
                "playlist": playlist,
                "index": index,
                "seek": seek,
                "seek_end": seek_end,
                "playing": self.player.is_playing(),
                "async_errors": self.errors,
            }
            self.errors = []
            return resp

    def log_error(self, msg):
        log(msg)
        with self.lock:
            self.errors.append(msg)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--debug", action="store_true")
    args = parser.parse_args()
    os.environ["UTUNES_SERVER_DEBUG"] = "1" if args.debug else ""
    server = Server()
    socket_fname = utunes.Paths.socket_basename.resolve()
    if socket_fname.is_socket():
        try:
            listener = multiprocessing.connection.Listener(str(socket_fname))
            log("another server seems to be live already")
            sys.exit(1)
        except OSError:
            try:
                socket_fname.unlink()
            except OSError:
                log("race condition while unlinking socket")
                sys.exit(1)
    try:
        listener = multiprocessing.connection.Listener(str(socket_fname))
    except OSError as e:
        log("failed to bind to socket: {}".format(e))
        sys.exit(1)
    log("listening on socket {}".format(socket_fname))
    while True:
        with listener.accept() as conn:
            if not conn.poll(timeout=1):
                log("dropped connection after 1 second")
                continue
            try:
                msg = conn.recv()
            except (OSError, EOFError) as e:
                server.log_error("got error during receive: {}".format(e))
                continue
            log("message: {}".format(msg))
            resp = server.handle_msg(msg)
            log("response: {}".format(resp))
            try:
                conn.send(resp)
            except ValueError as e:
                server.log_error("got error during send: {}".format(e))


def kill_children_on_exit():
    # Make sure we clean up any subprocesses we spawn. See
    # <https://stackoverflow.com/a/322317/3538165>,
    # <https://stackoverflow.com/a/320712/3538165>,
    # <https://stackoverflow.com/a/50329596/3538165>,
    # <https://stackoverflow.com/a/31464349/3538165>. If we don't do
    # this, then the mplayer instance we spawn will just be reparented
    # and continue running after we exit.
    os.getpgrp()

    def kill_children():
        signal.signal(signal.SIGTERM, lambda *args: None)
        os.killpg(0, signal.SIGTERM)

    atexit.register(kill_children)


if __name__ == "__main__":
    os.setpgrp()
    atexit.register(lambda: os.killpg(0, signal.SIGTERM))
    main()
