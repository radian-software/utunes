# https://gist.github.com/EyalAr/7915597#file-nbstreamreader-py

from threading import Thread
from queue import Queue, Empty


class NonBlockingStreamReader:
    def __init__(self, stream):
        """
        stream: the stream to read from.
                Usually a process' stdout or stderr.
        """

        self._s = stream
        self._q = Queue()

        def _populate_queue(stream, queue):
            """
            Collect lines from 'stream' and put them in 'queue'.
            """

            while True:
                line = stream.readline()
                queue.put(line)
                if not line:
                    return

        self._t = Thread(target=_populate_queue, args=(self._s, self._q))
        self._t.daemon = True
        # Start collecting lines from the stream.
        self._t.start()

    def readline(self, timeout=None):
        try:
            return self._q.get(block=timeout is not None, timeout=timeout)
        except Empty:
            return None


class UnexpectedEndOfStream(Exception):
    pass
