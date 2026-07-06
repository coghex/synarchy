#!/usr/bin/env python3
"""Shared harness for the headless behavior probes (#529).

Every ``tools/*_probe.py`` used to reimplement engine boot, the READY
wait, the single-line debug-console send/recv (with its short-idle-timeout
read), and ``engine.quit`` teardown — plus, in most cases, the acolyte
spawn + ``find_water`` goal-clear bootstrap. This module owns those
primitives once, so a fix (or a gotcha) lives in one place instead of ~40
copies.

Import it from a probe (probes run as ``python3 tools/<name>_probe.py``,
so ``tools/`` is on ``sys.path``)::

    from probelib import send, boot, quit_engine, spawn_acolyte, poll_until

Design notes / gotchas handled here so probes don't each rediscover them:

* **Debug-console idle read.** The console keeps the TCP connection open
  after replying, so we can't read to EOF. ``send`` reads until a short
  idle gap and returns the last non-empty ``"> "`` result line.
* **Never bind 8008 by default.** 8008 is the user's GUI port. Probes must
  pass their own (9xxx) port; ``boot`` refuses 8008 to avoid killing a
  graphical instance.
* **Acolyte ``find_water`` default goal.** A freshly spawned acolyte
  carries a standing ``find_water`` goal whose search utility can edge out
  the behavior under test (and can walk it off a cliff). ``spawn_acolyte``
  clears it by default.
"""
from __future__ import annotations

import json
import socket
import subprocess
import sys
import time

# The user's graphical instance lives on 8008; probes must never touch it.
GUI_PORT = 8008

DEFAULT_READY_TIMEOUT = 180.0
DEFAULT_IDLE = 0.3


# --------------------------------------------------------------------------
# Debug-console send / recv
# --------------------------------------------------------------------------
def _result_lines(chunks: list[bytes]) -> list[str]:
    """Non-empty ``"> <value>"`` reply lines (banner + trailing prompt are
    empty ``"> "`` lines, which drop out)."""
    out = b"".join(chunks).decode(errors="replace")
    return [ln[2:].strip() for ln in out.splitlines()
            if ln.startswith("> ") and ln[2:].strip()]


def send(port: int, lua: str, timeout: float = 10.0,
         expect_result: bool = True, idle: float = DEFAULT_IDLE) -> str:
    """Run one line of Lua in the debug console; return the result text.

    The console keeps the connection open after replying, so we can't read
    to EOF. The console echoes a banner, then ``"> <result>"`` lines, then a
    trailing empty ``"> "`` prompt; we return the last NON-EMPTY result line.

    ``expect_result=True`` (a ``return ...`` command) reads until a real
    result line appears, waiting up to ``timeout`` across idle gaps — this
    survives server-side BLOCKING calls like ``world.waitForInit`` that emit
    nothing until they unblock, then returns as soon as the value arrives.
    ``expect_result=False`` is for fire-and-forget commands (no ``return``):
    it drains the reply burst and stops at the first ``idle`` gap so it
    doesn't sit out the whole ``timeout``.
    """
    deadline = time.monotonic() + timeout
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks: list[bytes] = []
        s.settimeout(idle)
        while time.monotonic() < deadline:
            try:
                b = s.recv(4096)
            except socket.timeout:
                # Idle gap: settle here (this ~idle wait is deliberate — it
                # paces successive commands the way the original probes' read
                # did, which time-dependent probes rely on for async work like
                # inventory materialization or AI ticks). Only keep waiting if
                # this is a `return` command whose (possibly blocking) result
                # hasn't arrived yet.
                if _result_lines(chunks) or not expect_result:
                    break
                continue
            if not b:
                break
            chunks.append(b)
    # Strip surrounding quotes so a Lua string result ("ok") compares as
    # the bare value (ok) — matches the base probes' send and their string
    # equality checks. A no-op for numbers/booleans/JSON arrays+objects
    # (they don't start/end with a quote), so json.loads still works.
    results = _result_lines(chunks)
    if results:
        return results[-1].strip('"')
    return b"".join(chunks).decode(errors="replace").strip().strip('"')


def send_json(port: int, lua: str, timeout: float = 10.0, idle: float = DEFAULT_IDLE):
    """``send`` + ``json.loads`` the result. Returns ``None`` on empty/invalid."""
    raw = send(port, lua, timeout=timeout, idle=idle)
    if not raw:
        return None
    try:
        return json.loads(raw)
    except (ValueError, TypeError):
        return raw


# --------------------------------------------------------------------------
# Engine boot / teardown
# --------------------------------------------------------------------------
def _log_path(port: int, log: str | None) -> str:
    return log if log else f"/tmp/synarchy_probe_{port}.log"


def boot(port: int, log: str | None = None, args: list[str] | None = None,
         ready_timeout: float = DEFAULT_READY_TIMEOUT,
         label: str = "engine") -> subprocess.Popen:
    """Launch a headless engine on ``port`` and block until it prints READY.

    Exits the probe (non-zero) if the engine dies before READY or never
    prints it. ``args`` are extra CLI args appended after ``--port``.
    ``log`` is the stdout/stderr capture path (defaults to a per-port temp).
    """
    if port == GUI_PORT:
        sys.exit(f"refusing to boot on port {GUI_PORT} (the GUI port); pass a 9xxx port")
    logpath = _log_path(port, log)
    logf = open(logpath, "w")
    cmd = ["cabal", "run", "-v0", "exe:synarchy", "--", "--headless", "--port", str(port)]
    if args:
        cmd += args
    proc = subprocess.Popen(cmd, stdout=logf, stderr=subprocess.STDOUT)
    proc._probe_log = logpath  # type: ignore[attr-defined]
    deadline = time.time() + ready_timeout
    while time.time() < deadline:
        try:
            if "READY" in open(logpath).read():
                return proc
        except FileNotFoundError:
            pass
        if proc.poll() is not None:
            sys.exit(f"{label} exited before READY; see {logpath}")
        time.sleep(0.4)
    proc.kill()
    sys.exit(f"{label} never printed READY; see {logpath}")


def quit_engine(port: int, proc: subprocess.Popen | None = None,
                timeout: float = 15.0, wait_port: bool = True) -> None:
    """Ask the engine to quit, then ensure the process is gone and (by
    default) the listener port is released so the next boot on it can bind.

    Safe to call in a ``finally``: ``engine.quit()`` is a fire-and-forget
    command (``expect_result=False`` so it doesn't sit out the timeout), a
    dead-socket error on the send is swallowed, it waits up to ``timeout``
    for a clean exit and hard-kills if needed, then waits briefly for the
    port to free (which is what restart probes need between boots).
    """
    try:
        send(port, "engine.quit()", timeout=3.0, expect_result=False)
    except OSError:
        pass
    if proc is not None:
        deadline = time.time() + timeout
        while time.time() < deadline:
            if proc.poll() is not None:
                break
            time.sleep(0.2)
        if proc.poll() is None:
            proc.kill()
            try:
                proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                pass
    if wait_port:
        for _ in range(50):  # up to ~5 s for the listener socket to release
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                if s.connect_ex(("localhost", port)) != 0:
                    return
            time.sleep(0.1)


# --------------------------------------------------------------------------
# Polling
# --------------------------------------------------------------------------
def poll_until(seconds: float, fn, interval: float = 0.3):
    """Poll ``fn()`` every ``interval`` s until truthy or the budget runs out.

    Returns the truthy value, or ``None`` on timeout.
    """
    deadline = time.time() + seconds
    while time.time() < deadline:
        v = fn()
        if v:
            return v
        time.sleep(interval)
    return None


# --------------------------------------------------------------------------
# Common bootstrap: AI scripts, worlds, acolytes
# --------------------------------------------------------------------------
#: The unit AI/stat script stack the loading screen would load in the GUI.
AI_STACK = (
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
)


def load_ai_stack(port: int, scripts=AI_STACK) -> None:
    """Load the unit stat/resource/AI Lua stack (headless has no loading screen)."""
    for path, z in scripts:
        send(port, f"engine.loadScript('{path}', {z}); return 'ok'")


def clear_find_water(port: int, uid: int, seconds: float = 10.0) -> bool:
    """Retire a freshly spawned acolyte's standing ``find_water`` goal.

    Without this, the goal's search utility can out-compete the behavior
    under test (and walk the unit off a cliff on a waterless arena).
    """
    return poll_until(seconds, lambda: send(
        port,
        f"local ai = require('scripts.unit_ai'); "
        f"local s = ai.getState({uid}); "
        f"if not s then return false end; "
        f"ai.markGoalAccomplished(s, 'find_water'); return true") == "true") is not None


def spawn_acolyte(port: int, x: float, y: float, unit: str = "acolyte",
                  clear_water: bool = True) -> int:
    """Spawn a unit and (by default) clear its ``find_water`` goal; return its uid."""
    raw = send(port, f"return unit.spawn('{unit}', {x}, {y})")
    try:
        uid = int(float(raw))
    except (ValueError, TypeError):
        sys.exit(f"unit.spawn failed: {raw!r}")
    if clear_water and not clear_find_water(port, uid):
        sys.exit(f"unit {uid} never got AI state (find_water clear failed)")
    return uid


def init_world(port: int, name: str = "probe", seed: int = 42, size: int = 64,
               plates: int = 3, show: bool = True, timeout: float = 190.0) -> None:
    """``world.init`` + block on ``world.waitForInit`` (+ ``world.show``)."""
    send(port, f"world.init('{name}', {seed}, {size}, {plates})", expect_result=False)
    send(port, f"return world.waitForInit({int(timeout)})", timeout=timeout + 10)
    if show:
        send(port, f"world.show('{name}')", expect_result=False)


def init_arena(port: int, name: str = "arena", show: bool = True,
               timeout: float = 60.0) -> None:
    """``world.initArena`` + block on ``world.waitForInit`` (+ ``world.show``)."""
    send(port, f"world.initArena('{name}')", expect_result=False)
    send(port, f"return world.waitForInit({int(timeout)})", timeout=timeout + 10)
    if show:
        send(port, f"world.show('{name}')", expect_result=False)
