#!/usr/bin/env python3
"""Self-test for river_runtime_arena.py's owned-process teardown (#2719).

The arena driver must stop only the engine it launched. `stop_owned`
works through the child's process handle and never through the console
port, because once the child has exited another engine may hold that
port. Two cases, with no engine at all:

  * an already-exited child next to a live listener on a port: the
    listener sees no connection, and nothing else happens;
  * a live child: it is terminated and reaped.

Usage:
  python3 tools/test_river_runtime_arena.py [-v]
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import socket
import subprocess
import sys

import selftestlib
from selftestlib import FAILURES, expect

from river_runtime_arena import stop_owned


def test_exited_child_leaves_reused_port_alone() -> None:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as listener:
        listener.bind(("127.0.0.1", 0))
        listener.listen(1)
        listener.settimeout(1.0)
        child = subprocess.Popen([sys.executable, "-c", "pass"])
        child.wait(timeout=10)
        stop_owned(child)
        try:
            conn, _ = listener.accept()
            conn.close()
            connected = True
        except socket.timeout:
            connected = False
    expect(not connected, "an exited child's cleanup never connects to the port")
    expect(child.returncode == 0, "the exited child keeps its own exit status")


def test_live_child_is_terminated() -> None:
    child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    stop_owned(child, timeout=5)
    expect(child.poll() is not None, "a live child is stopped through its handle")


def main() -> int:
    selftestlib.parse_verbose()
    test_exited_child_leaves_reused_port_alone()
    test_live_child_is_terminated()
    if FAILURES:
        print(f"{len(FAILURES)} failure(s)")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "river_runtime_arena teardown: all cases pass")


if __name__ == "__main__":
    raise SystemExit(main())
