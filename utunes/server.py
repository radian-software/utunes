import abc
import argparse
import atexit
import multiprocessing.connection
import os
import signal
import sys
import threading
import time

import mpv

import utunes
from utunes import UNSET
import utunes.nbstreamreader


def log(msg):
    print("µTunes server: {}".format(msg), file=sys.stderr)


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


class MPVPlayer(Player):

    def __init__(self, callback, log_error):
        super().__init__(callback, log_error)
        self.seek_finished_event = threading.Event()
        self.mpv = mpv.MPV()
        self.mpv.pause = True
        self.mpv.event_callback("end-file")(self.handle_end_file)
        self.mpv.event_callback("seek")(self.handle_seek)

    def handle_end_file(self, event_data):
        if event_data.error:
            self.log_error(f"MPV error: {event_data}")
        elif event_data.event_id.value == mpv.MpvEventID.END_FILE:
            self.mpv.pause = True
            self.callback()

    def handle_seek(self, event):
        self.seek_finished_event.set()

    def load(self, filename):
        self.mpv.pause = True
        self.mpv.loadfile(str(filename))
        # Wait long enough for the operation to complete, probably.
        # Needless to say, this is a total hack. It seems that
        # wait_for_property and friends are pretty buggy, though, so I
        # haven't figured out how to get this working the proper way.
        time.sleep(0.1)

    def unload(self):
        self.mpv.stop()

    def play(self):
        self.mpv.pause = False

    def pause(self):
        self.mpv.pause = True

    def is_playing(self):
        return not self.mpv.pause

    def seek(self, pos):
        self.seek_finished_event.clear()
        self.mpv.seek(pos, "absolute")
        self.seek_finished_event.wait()

    def tell(self):
        pos = self.mpv.playback_time
        if pos is None:
            return UNSET
        return pos

    def tell_end(self):
        # Make sure not to use self.mpv.length. The documentation
        # claims that length and duration are equivalent, but actually
        # length is always 'none'.
        length = self.mpv.duration
        if length is None:
            return UNSET
        return length


class Server:

    def __init__(self, playlist=UNSET, index=UNSET, seek=UNSET):
        self.lock = threading.Lock()
        self.error_lock = threading.Lock()
        self.player = MPVPlayer(self.advance_song, self.log_error)
        self.playlist = UNSET
        self.index = UNSET
        self.last_json_mtime = UNSET
        self.next_song_filename = UNSET
        self.errors = []
        os.chdir(utunes.Library.find_library_root())

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
                    utunes.Paths.json_basename.stat().st_mtime ==
                    self.last_json_mtime
            ):
                filename = self.next_song_filename
            elif self.playlist is not UNSET and self.index is not UNSET:
                lib = utunes.Library()
                filename = lib.get_song_filename(self.playlist, self.index)
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
            lib = utunes.Library()
            self.last_json_mtime = lib.get_json_filename().stat().st_mtime
            filename = lib.get_song_filename(self.playlist, self.index + 1)
            self.next_song_filename = filename

    def advance_song(self):
        with self.lock:
            if self.playlist is not UNSET and self.index is not UNSET:
                self.update(
                    playlist=self.playlist, index=self.index + 1, playing=True
                )

    def make_error(self, msg):
        with self.error_lock:
            async_errors = list(self.errors)
            self.errors = []
        return {
            "error": msg,
            "async_errors": async_errors,
        }

    def handle_msg(self, msg):
        if not isinstance(msg, dict):
            return self.make_error("not an object: {}".format(repr(msg)))
        playlist = msg.get("playlist", UNSET)
        index = msg.get("index", UNSET)
        seek = msg.get("seek", UNSET)
        playing = msg.get("playing", UNSET)
        if playlist is not UNSET:
            if not isinstance(playlist, str):
                return self.make_error("not a string: {}".format(repr(playlist)))
        if index is not UNSET:
            if not isinstance(index, int):
                return self.make_error("not an integer: {}".format(repr(index)))
            if index < 0:
                return self.make_error("index is negative: {}".format(repr(index)))
        if seek is not UNSET:
            if not isinstance(seek, (int, float)):
                return self.make_error("not a number: {}".format(repr(seek)))
            if seek < 0:
                return self.make_error("seek is negative: {}".format(repr(seek)))
        if playing is not UNSET:
            if playing not in (True, False, "toggle"):
                return self.make_error(
                    "unknown 'playing' value: {}".format(repr(playing))
                )
        with self.lock:
            if playing == "toggle":
                playing = not self.player.is_playing()
            self.update(playlist, index, seek, playing)
            playlist = self.playlist if self.playlist is not UNSET else None
            index = self.index if self.index is not UNSET else None
            seek = self.player.tell()
            if seek is UNSET:
                seek = None
            seek_end = self.player.tell_end()
            if seek_end is UNSET:
                seek_end = None
            with self.error_lock:
                async_errors = list(self.errors)
                self.errors = []
            resp = {
                "error": None,
                "playlist": playlist,
                "index": index,
                "seek": seek,
                "seek_end": seek_end,
                "playing": self.player.is_playing(),
                "async_errors": async_errors,
            }
            return resp

    def log_error(self, msg):
        log(msg)
        with self.error_lock:
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


if __name__ == "__main__":
    main()
