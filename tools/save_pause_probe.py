#!/usr/bin/env python3
"""Headless save/load pause-semantics probe (regression guard for #42).

The pause module (scripts/pause.lua) promises a single invariant: when the
engine is paused, world time of day is frozen too. Two effects move
together — engine.setPaused(true) AND world.setTimeScale(pageId, 0).

The save/load auto-pause path used to bypass that: it flipped
enginePausedRef directly but left wsTimeScaleRef alone, so World.Thread.Time
kept advancing the clock of a world the engine reported as "paused".

This probe drives the real save/load path through the world thread and
asserts the invariant holds on BOTH sides, then that resuming the loaded
session is coherent:

  1. After engine.saveWorld: engine.isPaused() AND getTimeScale == 0.
  2. After engine.loadSave:  engine.isPaused() AND getTimeScale == 0.
  3. After resuming a LOADED session: NOT engine.isPaused() AND the
     loaded page runs at the default speed -- never the non-default
     speed this probe deliberately ran at before saving (#1572).

Check 3 is taken against a SECOND load of the same save. Check 2's
stray-setTimeScale race writes the paused page's pause-epoch resume
scale, so resuming the page it touched would read the probe's own value
back; reloading gives the oracle a pause epoch nothing here has written
to, while leaving the race coverage untouched.

Every page-targeted read after the load names the page
world.getActiveWorldId() reported, never a hardcoded id: since #763 a
loaded page keeps its saved id verbatim, and Clock.hs answers an
UNREGISTERED page with its documented 1.0 default, so a wrong id reads
as a healthy default world instead of failing.

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

Exit 0 = the invariant held on both sides and the loaded session
resumed at the default speed.
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
from probelib import (quit_engine, boot, send, capture_request_id,
                      wait_load_published)

LOG = "/tmp/save_pause_probe_engine.log"
# Unique per run. Saves overwrite in place (World.Save.Serialize), and this
# probe deletes its save on exit — a fixed name could clobber a real save that
# happened to share it, and a stale directory from an interrupted run could
# make a later run falsely pass by loading old data. A fresh random name avoids
# both, and main() additionally refuses to run if the directory already exists.
SAVE_NAME = "probe_pause_" + uuid.uuid4().hex[:12]

# Pause/resume goes through scripts.pause, the single source of truth for
# the paired engine.setPaused + retime effect, rather than the raw engine
# binding. It has to be reached through package.loaded the way the other
# save/load probes do (tools/transactional_load_probe.py): loadScript
# sandboxes each script's globals, so `type(pause)` in the debug console
# is nil and the bare `pause.set(false)` this file used to send raised
# instead of unpausing anything.
PAUSE = "require('scripts.pause')"

# The speed this probe runs the world at BEFORE saving. Deliberately not
# 1.0: with a session that begins and ends at the default, "the load reset
# the speed" and "the load kept the saved speed" are the same observation,
# and check 3 above cannot distinguish them (#1572).
PRE_SAVE_SCALE = 4.0

# What a resumed loaded session must run at. This is the value check 3
# compares against; flipping it to something the engine will not report is
# the one-line edit that demonstrates the oracle can fail.
RESUMED_SCALE = 1.0


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


def wait_time_scale(port: int, page: str, expected: float,
                    timeout: float = 10.0):
    """Poll until `page` reports `expected`, or timeout. world.setTimeScale
    is QUEUED to the world thread (World.Thread.Command.Time), so a fixed
    sleep is not a completion boundary. Returns (ok, last_ts)."""
    deadline = time.time() + timeout
    ts = float("nan")
    while time.time() < deadline:
        ts = as_float(send(port, f'return world.getTimeScale("{page}")'))
        if ts == expected:
            return True, ts
        time.sleep(0.3)
    return False, ts


def wait_unpaused(port: int, timeout: float = 10.0):
    """Poll until the engine reports itself running. Returns (ok, paused)."""
    deadline = time.time() + timeout
    paused = True
    while time.time() < deadline:
        paused = as_bool(send(port, "return engine.isPaused()"))
        if not paused:
            return True, paused
        time.sleep(0.3)
    return False, paused


def wait_resumed(port: int, page: str, expected: float, timeout: float = 15.0):
    """Poll until (NOT isPaused AND page reports `expected`), or timeout.

    Both halves need polling for different reasons: pause.set flips the
    engine flag synchronously but the speed it reinstates rides the queued
    WorldSetTimeScale path. Returns the last (ok, paused, ts) observed, so
    a timeout reports which half was wrong."""
    deadline = time.time() + timeout
    paused, ts = True, float("nan")
    while time.time() < deadline:
        paused = as_bool(send(port, "return engine.isPaused()"))
        ts = as_float(send(port, f'return world.getTimeScale("{page}")'))
        if not paused and ts == expected:
            return True, paused, ts
        time.sleep(0.3)
    return False, paused, ts


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

    proc = boot(args.port, log=LOG)
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

        # Run the world at a NON-default speed before saving. Without this
        # the session begins and ends at 1.0, so the post-load resumed-speed
        # check below cannot tell "the load reset the speed to the default"
        # from "the load handed back the speed the save was taken at" — both
        # read 1.0. Fast-forwarding here makes those two different
        # observations (#1572).
        send(args.port, f'world.setTimeScale("pausetest", {PRE_SAVE_SCALE})',
             expect_result=False)
        ff_ok, ff_ts = wait_time_scale(args.port, "pausetest", PRE_SAVE_SCALE)
        print(f"[pre-save ] fast-forward to {PRE_SAVE_SCALE} -> timeScale={ff_ts} "
              f"{'ok' if ff_ok else 'FAIL'}")
        if not ff_ok:
            failures.append(
                f"before save: could not run the world at a non-default speed "
                f"(asked for {PRE_SAVE_SCALE}, page reports {ff_ts}); the "
                f"post-load resumed-speed check would be vacuous")

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

        # Resume before loading, so "the load left the session paused" below
        # is a real transition rather than a state that was already true.
        # engine.loadSave pauses synchronously on acceptance
        # (Engine.Scripting.Lua.API.Save), so this is the precondition that
        # check 2 needs to mean anything.
        #
        # This is where #1572's broken resumed-speed assertion used to sit —
        # BEFORE engine.loadSave, reading a "main_world" page this probe
        # never registers, so Clock.hs's absent-page 1.0 default made it
        # compare 1.0 to 1.0 whatever any real page was doing. The repaired
        # check is at the end of the run, on the page the load published.
        #
        # Note the speed this resumes at is 1.0, not PRE_SAVE_SCALE: the
        # save-race check just above enqueued setTimeScale(page, 1.0) while
        # paused, and #1599 routes a paused request into the page's
        # pause-epoch resume scale, which is what a resume reinstates. The
        # save on disk was still taken at PRE_SAVE_SCALE, which is what the
        # post-load check cares about.
        send(args.port, f'{PAUSE}.set(false)', expect_result=False)
        unpaused_ok, pre_load_paused = wait_unpaused(args.port)
        print(f"[pre-load ] resumed before loading -> isPaused={pre_load_paused} "
              f"{'ok' if unpaused_ok else 'FAIL'}")
        if not unpaused_ok:
            failures.append(
                f"before load: expected the session to be running so the load's "
                f"own pause is observable, got isPaused={pre_load_paused}")

        # 3. Load → world thread must restore paused AND a frozen clock.
        #    Wait for the file to actually land first (saveWorld returns on
        #    enqueue, so the write can lag the frozen-clock signal above).
        if not wait_save_written(SAVE_NAME):
            failures.append(f"save file for '{SAVE_NAME}' never appeared on disk")
        load_ok = send(args.port, f'return engine.loadSave("{SAVE_NAME}")')
        print(f"[load     ] engine.loadSave -> {load_ok}")
        # Issue #763: engine.loadSave only ACCEPTS the request — the saved
        # page ("pausetest", its own id verbatim, never remapped to
        # "main_world") doesn't exist live until the transaction actually
        # publishes. Wait for that before touching anything it names.
        published, status = wait_load_published(args.port)
        print(f"[load     ] load transaction published -> {published} ({status})")
        first_load_id = status.get("id") if isinstance(status, dict) else None
        if not published:
            failures.append(f"load transaction never published: {status}")
        active_page = send(args.port, "return world.getActiveWorldId()").strip().strip('"')
        print(f"[load     ] active page after publish -> {active_page!r}")
        # worldGetActiveWorldIdFn returns nil when no page is active, while
        # worldGetTimeScaleFn answers an unknown page with its documented
        # 1.0 default — so an unusable id here would silently turn every
        # page-targeted check below into a read of nothing. Fail instead,
        # and skip those checks rather than run them against a bogus name.
        if not active_page or active_page.lower() in ("nil", "null"):
            failures.append(
                f"after load: world.getActiveWorldId() named no page "
                f"({active_page!r}); the post-load frozen, race and "
                f"resumed-speed checks were skipped")
        else:
            send(args.port, f'world.show("{active_page}")', expect_result=False)
            paused, ts = wait_paused_and_frozen(args.port, active_page)
            print(f"[post-load] isPaused={paused} timeScale={ts}")
            if not (paused and ts == 0.0):
                failures.append(
                    f"after load: expected isPaused=True & timeScale=0, "
                    f"got isPaused={paused} timeScale={ts}")
            ok, rp, rt = stays_frozen_under_race(args.port, active_page)
            print(f"[load-race] stray setTimeScale while paused -> "
                  f"isPaused={rp} timeScale={rt} {'ok' if ok else 'FAIL'}")
            if not ok:
                failures.append(
                    f"after load: a stray setTimeScale un-froze a paused world "
                    f"(isPaused={rp} timeScale={rt})")

            # 4. Reload, then resume: the loaded session runs at the
            #    default speed.
            #
            #    The oracle deliberately runs against a SECOND load rather
            #    than the one above. #1572 requirement 3 keeps the race
            #    check ahead of it, and that check enqueues
            #    setTimeScale(page, 1.0) while paused — which #1599 files
            #    as that page's pause-epoch resume scale
            #    (World.Thread.Command.Time). Resuming the page it wrote to
            #    would read the probe's own 1.0 back and pass even if the
            #    load had handed back the saved speed. Staging builds fresh
            #    WorldStates, so the reloaded page carries a pause epoch
            #    nothing here has written to, and the race coverage above
            #    is kept exactly as it was.
            #
            #    Resume first, so the reload's own pause is a real
            #    transition and its epoch captures whatever speed the
            #    staged page came up at — the regression-sensitive step.
            send(args.port, f'{PAUSE}.set(false)', expect_result=False)
            pre2_ok, pre2_paused = wait_unpaused(args.port)
            print(f"[reload   ] resumed before reloading -> "
                  f"isPaused={pre2_paused} {'ok' if pre2_ok else 'FAIL'}")
            if not pre2_ok:
                failures.append(
                    f"before reload: expected the session to be running so "
                    f"the reload's own pause is observable, got "
                    f"isPaused={pre2_paused}")
            reload_ok = send(args.port, f'return engine.loadSave("{SAVE_NAME}")')
            # The reload must be waited for BY ITS OWN ID: the first load
            # left a terminal LoadPublished behind, and an id-less wait
            # accepts the first terminal phase it sees (probelib's round-5
            # note), which here would be that stale one — the reloaded page
            # is the whole point of this block. Capture it with
            # capture_request_id, not a single query: a getLoadStatus()
            # queued right as a load replaces the session comes back as a
            # REJECTED string rather than a table, and a fast tiny-world
            # load makes that a real window (probelib).
            reload_id = capture_request_id(args.port,
                                           "return engine.getLoadStatus()")
            print(f"[reload   ] engine.loadSave -> {reload_ok} (id={reload_id})")
            if reload_id is None or reload_id == first_load_id:
                # Without a distinct id there is no way to tell the reload's
                # publication from the first load's, so the fresh-epoch
                # guarantee this block exists for is gone. Fail rather than
                # wait on an id that could match the wrong transaction.
                failures.append(
                    f"after reload: could not capture the reload's own load "
                    f"request id (got {reload_id!r}, first load was "
                    f"{first_load_id!r}); the resumed-speed check was skipped")
                page2 = ""
            else:
                published2, status2 = wait_load_published(args.port,
                                                          request_id=reload_id)
                print(f"[reload   ] load transaction published -> {published2} "
                      f"({status2})")
                if not published2:
                    failures.append(
                        f"reload transaction never published: {status2}")
                page2 = send(args.port,
                             "return world.getActiveWorldId()").strip().strip('"')
                print(f"[reload   ] active page after publish -> {page2!r}")
                if not page2 or page2.lower() in ("nil", "null"):
                    failures.append(
                        f"after reload: world.getActiveWorldId() named no page "
                        f"({page2!r}); the resumed-speed check was skipped")
                    page2 = ""
            if page2:
                send(args.port, f'world.show("{page2}")', expect_result=False)
                # Precondition, and what proves the page is REGISTERED:
                # Clock.hs answers an unknown page with 1.0, which could
                # never satisfy timeScale == 0. Everything below therefore
                # reads a real page, which is the second half of what made
                # the assertion this replaces vacuous.
                p2, t2 = wait_paused_and_frozen(args.port, page2)
                print(f"[reload   ] isPaused={p2} timeScale={t2}")
                if not (p2 and t2 == 0.0):
                    failures.append(
                        f"after reload: expected isPaused=True & timeScale=0, "
                        f"got isPaused={p2} timeScale={t2}")
                # The assertion #1572 repaired: it runs after a load
                # published, names the page that load reported, and the
                # save under it was taken while the world ran at
                # PRE_SAVE_SCALE — so "reset to the default" and "kept the
                # saved speed" are different readings.
                send(args.port, f'{PAUSE}.set(false)', expect_result=False)
                r_ok, r_paused, r_ts = wait_resumed(args.port, page2,
                                                    RESUMED_SCALE)
                print(f"[resume   ] loaded page {page2!r} -> "
                      f"isPaused={r_paused} timeScale={r_ts} "
                      f"{'ok' if r_ok else 'FAIL'}")
                if not r_ok:
                    failures.append(
                        f"after load/unpause: expected isPaused=False & "
                        f"timeScale={RESUMED_SCALE} on loaded page {page2!r}, "
                        f"got isPaused={r_paused} timeScale={r_ts} (saved "
                        f"while the world ran at {PRE_SAVE_SCALE})")
    finally:
        quit_engine(args.port, proc)
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
    print("\nPASS: engine.isPaused() and world time stay consistent across "
          "save/load, and the loaded session resumes at the default speed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
