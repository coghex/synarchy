#!/usr/bin/env python3
"""Headless save/load pause-semantics probe (regression guard for #42).

The pause module (scripts/pause.lua) promises a single invariant: when the
engine is paused, world time of day is frozen too. Two effects move
together — engine.setPaused(true) AND world.setTimeScale(pageId, 0).

The save/load auto-pause path used to bypass that: it flipped
enginePausedRef directly but left wsTimeScaleRef alone, so World.Thread.Time
kept advancing the clock of a world the engine reported as "paused".

This probe drives the real save/load path through the world thread and
asserts the invariant holds on BOTH sides:

  1. After engine.saveWorld: engine.isPaused() AND getTimeScale == 0.
  2. After engine.loadSave:  engine.isPaused() AND getTimeScale == 0.

The timescale is zeroed on the world thread (handleWorldSaveCommand /
handleWorldLoadSaveCommand), which runs asynchronously after the API call
returns, so each check POLLS until the world thread has processed the
command (or times out — a timeout is a failure).

This probe checks the stored timescale (getTimeScale == 0). The deeper
guarantee — that a paused world's clock never advances even if a stray
WorldSetTimeScale restores a nonzero scale — is enforced engine-side by
World.Thread.tickWorldTime gating advancement on enginePausedRef, and is
not separately asserted here (no world-time-of-day getter is exposed to
the debug console to observe it headless).

Usage:
  python3 tools/save_pause_probe.py [--port 9142] [--seed 42]

Exit 0 = invariant held on both sides.
"""
from __future__ import annotations

import argparse
import os
import shutil
import socket
import subprocess
import sys
import time
import uuid

LOG = "/tmp/save_pause_probe_engine.log"
# Unique per run. Saves overwrite in place (World.Save.Serialize), and this
# probe deletes its save on exit — a fixed name could clobber a real save that
# happened to share it, and a stale directory from an interrupted run could
# make a later run falsely pass by loading old data. A fresh random name avoids
# both, and main() additionally refuses to run if the directory already exists.
SAVE_NAME = "probe_pause_" + uuid.uuid4().hex[:12]


def _results(raw: bytes) -> list[str]:
    # The console emits a "synarchy debug console\n> " banner on connect and
    # a trailing "> " prompt; both yield EMPTY "> " lines. The real answer is
    # the non-empty "> <value>" line, so filter the empties out.
    out = raw.decode(errors="replace")
    return [ln[2:].strip() for ln in out.splitlines()
            if ln.startswith("> ") and ln[2:].strip()]


def send(port: int, lua: str, timeout: float = 10.0,
         expect_result: bool = True) -> str:
    """Run one Lua line over the debug console and return its result.

    With expect_result (a `return ...` command) we read until a non-empty
    "> value" line appears, waiting up to `timeout` — this skips the banner
    and survives server-side BLOCKING calls like world.waitForInit, which
    emit nothing until they unblock. Fire-and-forget commands (no return)
    pass expect_result=False and just drain briefly."""
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks: list[bytes] = []
        s.settimeout(timeout)
        try:
            while True:
                b = s.recv(4096)
                if not b:
                    break
                chunks.append(b)
                if expect_result and _results(b"".join(chunks)):
                    break               # got the real result past the banner
                if not expect_result:
                    s.settimeout(0.3)   # drain remainder then idle out
        except socket.timeout:
            pass
    vals = _results(b"".join(chunks))
    return (vals[-1] if vals else "").strip('"')


def wait_for_init(port: int, timeout: float = 300.0) -> str:
    return send(port, f"return world.waitForInit({int(timeout)})",
                timeout=timeout + 5)


def as_bool(s: str) -> bool:
    return s.strip().lower() in ("true", "1", "1.0")


def as_float(s: str) -> float:
    try:
        return float(s.strip())
    except ValueError:
        return float("nan")


def boot(port: int) -> subprocess.Popen:
    log = open(LOG, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT,
    )
    deadline = time.time() + 240
    while time.time() < deadline:
        try:
            if "READY" in open(LOG).read():
                return proc
        except FileNotFoundError:
            pass
        if proc.poll() is not None:
            sys.exit(f"engine exited before READY; see {LOG}")
        time.sleep(0.4)
    proc.kill()
    sys.exit("engine never printed READY")


def wait_save_written(name: str, timeout: float = 30.0) -> bool:
    """Block until the save file lands on disk. saveWorld() returns as soon
    as the WorldSave command is QUEUED; the world thread zeroes the
    timescale before it finishes serializing, so the frozen-clock signal
    can precede the file existing — load before that and it 'Save not found'."""
    path = os.path.join("saves", name, "world.synworld")
    deadline = time.time() + timeout
    while time.time() < deadline:
        if os.path.isfile(path):
            return True
        time.sleep(0.3)
    return False


def wait_paused_and_frozen(port: int, page: str, timeout: float = 20.0):
    """Poll until (isPaused AND timescale==0), or timeout. Returns the
    final (paused, timescale) pair observed."""
    deadline = time.time() + timeout
    paused, ts = False, float("nan")
    while time.time() < deadline:
        paused = as_bool(send(port, "return engine.isPaused()"))
        ts = as_float(send(port, f'return world.getTimeScale("{page}")'))
        if paused and ts == 0.0:
            return paused, ts
        time.sleep(0.5)
    return paused, ts


def stays_frozen_under_race(port: int, page: str, hold: float = 2.5):
    """Reproduce the queued-unpause race directly: while the world is paused,
    enqueue a stray WorldSetTimeScale(1.0) — exactly what a Space-press
    unpause or a speed control would queue, and what could land after a
    WorldSave. handleWorldSetTimeScaleCommand must refuse to store a running
    scale while paused. We poll for `hold` seconds (NOT just one sample) so a
    later-processed command can't slip a nonzero scale in after an early
    positive. Returns (ok, last_paused, last_ts)."""
    send(port, f'world.setTimeScale("{page}", 1.0)', expect_result=False)
    paused, ts = True, 0.0
    deadline = time.time() + hold
    while time.time() < deadline:
        paused = as_bool(send(port, "return engine.isPaused()"))
        ts = as_float(send(port, f'return world.getTimeScale("{page}")'))
        if not (paused and ts == 0.0):
            return False, paused, ts
        time.sleep(0.3)
    return True, paused, ts


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9142)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()

    # Never reuse or clobber an existing save directory (belt-and-suspenders
    # on top of the random SAVE_NAME).
    save_dir = os.path.join("saves", SAVE_NAME)
    if os.path.exists(save_dir):
        sys.exit(f"refusing to run: {save_dir} already exists")

    proc = boot(args.port)
    failures: list[str] = []
    try:
        # 1. Generate a small world and activate it.
        send(args.port, f'world.init("pausetest", {args.seed}, 64, 3)',
             expect_result=False)
        print(f"[init     ] waitForInit -> {wait_for_init(args.port)}")
        send(args.port, 'world.show("pausetest")', expect_result=False)

        # Sanity: a freshly generated, unpaused world runs at scale 1.
        pre_ts = as_float(send(args.port, 'return world.getTimeScale("pausetest")'))
        pre_paused = as_bool(send(args.port, "return engine.isPaused()"))
        print(f"[pre-save ] isPaused={pre_paused} timeScale={pre_ts}")

        # 2. Save → world thread auto-pauses AND must freeze the clock.
        save_ok = send(args.port, f'return engine.saveWorld("pausetest", "{SAVE_NAME}")')
        print(f"[save     ] engine.saveWorld -> {save_ok}")
        paused, ts = wait_paused_and_frozen(args.port, "pausetest")
        print(f"[post-save] isPaused={paused} timeScale={ts}")
        if not (paused and ts == 0.0):
            failures.append(
                f"after save: expected isPaused=True & timeScale=0, "
                f"got isPaused={paused} timeScale={ts}")
        ok, rp, rt = stays_frozen_under_race(args.port, "pausetest")
        print(f"[save-race] stray setTimeScale while paused -> "
              f"isPaused={rp} timeScale={rt} {'ok' if ok else 'FAIL'}")
        if not ok:
            failures.append(
                f"after save: a stray setTimeScale un-froze a paused world "
                f"(isPaused={rp} timeScale={rt})")

        # 3. Load → world thread must restore paused AND a frozen clock.
        #    Wait for the file to actually land first (saveWorld returns on
        #    enqueue, so the write can lag the frozen-clock signal above).
        if not wait_save_written(SAVE_NAME):
            failures.append(f"save file for '{SAVE_NAME}' never appeared on disk")
        load_ok = send(args.port, f'return engine.loadSave("{SAVE_NAME}")')
        print(f"[load     ] engine.loadSave -> {load_ok}")
        print(f"[load     ] waitForInit -> {wait_for_init(args.port)}")
        time.sleep(3.0)  # let the load settle past LoadDone
        send(args.port, 'world.show("main_world")', expect_result=False)
        paused, ts = wait_paused_and_frozen(args.port, "main_world")
        print(f"[post-load] isPaused={paused} timeScale={ts}")
        if not (paused and ts == 0.0):
            failures.append(
                f"after load: expected isPaused=True & timeScale=0, "
                f"got isPaused={paused} timeScale={ts}")
        ok, rp, rt = stays_frozen_under_race(args.port, "main_world")
        print(f"[load-race] stray setTimeScale while paused -> "
              f"isPaused={rp} timeScale={rt} {'ok' if ok else 'FAIL'}")
        if not ok:
            failures.append(
                f"after load: a stray setTimeScale un-froze a paused world "
                f"(isPaused={rp} timeScale={rt})")
    finally:
        try:
            send(args.port, "engine.quit()", timeout=3, expect_result=False)
        except OSError:
            pass
        try:
            proc.wait(timeout=15)
        except subprocess.TimeoutExpired:
            proc.kill()
        # Clean up the throwaway save this probe created. Safe to delete: the
        # name is unique to this run and main() refused to start if it already
        # existed, so nothing here predates us.
        if os.path.isdir(save_dir):
            shutil.rmtree(save_dir, ignore_errors=True)

    if failures:
        print("\nFAIL:")
        for f in failures:
            print("  - " + f)
        return 1
    print("\nPASS: engine.isPaused() and world time stay consistent across save/load.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
