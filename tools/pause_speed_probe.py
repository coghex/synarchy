#!/usr/bin/env python3
"""Headless pause-speed probe: the player's chosen speed survives a pause
imposed by anything (#1599).

`scripts/pause.lua` promises that a chosen fast-forward "survives a pause
cycle and resumes at the speed the player chose". Before #1599 it could
only keep that promise for a pause Lua itself imposed: every engine-side
writer of the pause flag runs no Lua at all -- a `pause: true`
notification category (Engine.PlayerEvent.Emit), engine.saveWorld's
acceptance, the world thread's save re-assertion, a load publish -- so
the resume branch handed back whatever the last Lua-imposed pause had
recorded, 1.0 in an ordinary session.

WHY THIS EXISTS AS A PROBE, not only as hspec. The engine-side half is
covered by `Test.Headless.World.PauseSpeed`, but that suite cannot drive
`engine.saveWorld` at all: `saveOwnerSet` requires SaveUnit,
SaveBuilding, SaveCombat and SaveSimulation to acknowledge the barrier,
and the headless hspec harness starts only the world thread, so every
call would time out after five seconds regardless of the behaviour under
test. The hspec suite therefore enters at `acceptSaveRequest`. Only a
real process has every owner thread live, so the WHOLE-VERB proof --
engine.saveWorld returning true, its asynchronous transaction reaching a
terminal outcome, and only THEN the resume -- lives here.

Deliberately separate from `tools/save_pause_probe.py`, which owns the
#42 invariant (a paused world's clock reads 0) and whose own resumed-
speed oracle is a known, separately-tracked defect (#1572). This probe
asserts the complementary property that file does not: WHICH speed comes
back.

Four stages, one engine, one generated world:

  A. a `pause: true` notification pauses at 10x; the player's own Space
     route (pause.toggle) resumes at 10x, not 1x.
  B. a manual engine.saveWorld at 8x returns true and its transaction
     reaches a terminal SaveSucceeded; resuming afterwards returns 8x.
  C. a save taken from an ALREADY paused session does not overwrite the
     epoch's captured speed with the zero it reads (requirement 4).
  D. load policy is unchanged: a published load comes up paused and
     resumes at the default 1.0, never at a pre-save speed.

Usage:
  python3 tools/pause_speed_probe.py [--port 9147] [--seed 42]

Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import uuid

import probe_protocol
from probelib import (boot, capture_request_id, poll_until, quit_engine,
                      send, wait_load_published, wait_save_complete)

LOG = "/tmp/pause_speed_probe_engine.log"
PAGE = "pausespeed"
# Unique per run, and main() refuses to start if either directory already
# exists: saves overwrite in place, so a fixed name could clobber a real
# save, and a stale directory from an interrupted run could make a later
# run falsely pass by loading old data.
RUN_ID = uuid.uuid4().hex[:12]
SAVE_NAME = "probe_pausespeed_" + RUN_ID
SAVE_NAME_PAUSED = "probe_pausespeed_prepaused_" + RUN_ID


class Checks:
    def __init__(self, rep) -> None:
        self.rep = rep
        self.failed = 0

    def ok(self, cond: bool, label: str, detail: str = "") -> bool:
        self.rep.check(CHECK_ID_BY_LABEL[label], cond, label
                       + (f"  ({detail})" if detail else ""))
        if not cond:
            self.failed += 1
        return bool(cond)


def as_bool(s: str) -> bool:
    return s.strip().lower() in ("true", "1", "1.0")


def as_float(s: str) -> float:
    try:
        return float(s.strip())
    except ValueError:
        return float("nan")


def scale(port: int) -> float:
    return as_float(send(port, f'return world.getTimeScale("{PAGE}")'))


def paused(port: int) -> bool:
    return as_bool(send(port, "return engine.isPaused()"))


def set_scale(port: int, value: float, chk: Checks) -> None:
    """Ask for a speed and wait for the world thread to store it.
    world.setTimeScale is a QUEUED command, so a caller that reads back
    immediately can observe the previous value and stage its assertions on
    a precondition that had not landed yet."""
    send(port, f'world.setTimeScale("{PAGE}", {value})', expect_result=False)
    chk.ok(bool(poll_until(10, lambda: scale(port) == value)),
           f"precondition: the world is running at {value}x",
           f"got {scale(port)}")


def resume(port: int, how: str) -> None:
    send(port, f"require('scripts.pause').{how}", expect_result=False)


def expect_paused_and_frozen(port: int, chk: Checks, label: str) -> None:
    chk.ok(bool(poll_until(20, lambda: paused(port) and scale(port) == 0.0)),
           label, f"isPaused={paused(port)} timeScale={scale(port)}")


def expect_resumed_at(port: int, chk: Checks, want: float, label: str) -> None:
    chk.ok(bool(poll_until(20, lambda: not paused(port) and scale(port) == want)),
           label, f"isPaused={paused(port)} timeScale={scale(port)}")


def notify_pause(port: int) -> None:
    """Pause the way a notification does: `survival_critical` ships
    `pause: true` in data/notification_categories.yaml, and
    Engine.PlayerEvent.Emit acts on that flag without running any Lua."""
    send(port, 'engine.emitEvent("survival_critical", '
               '"pause_speed_probe: a critical event")', expect_result=False)


def save_and_settle(port: int, name: str, chk: Checks, label: str) -> None:
    """The whole manual-save verb, end to end: engine.saveWorld must
    return true, and its transaction must reach a TERMINAL outcome before
    anything downstream resumes. saveWorld only ACCEPTS the request
    synchronously -- the world thread re-asserts the pause and the encode
    plus disk write finish afterwards -- so resuming on the return value
    alone would be racing the very handoff this probe exists to pin."""
    accepted = send(port, f'return engine.saveWorld("{PAGE}", "{name}")')
    chk.ok(accepted.strip() == "true", f"{label}: engine.saveWorld returned true",
           accepted.strip())
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    if not chk.ok(request_id is not None,
                  f"{label}: the save transaction reported a request id"):
        if chk.rep.protocol_mode:
            # A later phase cannot jump over the unobserved completion.
            raise RuntimeError(f"{label}: no request id; later checks were not run")
        return
    succeeded, status = wait_save_complete(port, request_id)
    chk.ok(succeeded, f"{label}: the save transaction reached a terminal "
                      f"SaveSucceeded before the resume", str(status))


PROBE_CHECKS = [
    ('notification_speed', 'precondition: the world is running at 10.0x'),
    ('notification_frozen', 'the notification paused the session and froze its clock'),
    ('notification_resumed', 'pause.toggle resumed at the chosen 10x, not the default 1x'),
    ('manual_speed', 'precondition: the world is running at 8.0x'),
    ('manual_save_accepted', 'manual save: engine.saveWorld returned true'),
    ('manual_save_request', 'manual save: the save transaction reported a request id'),
    ('manual_save_completed', 'manual save: the save transaction reached a terminal SaveSucceeded before the resume'),
    ('manual_frozen', 'the completed save left the session paused and frozen'),
    ('manual_resumed', 'resuming after the save returned the chosen 8x, not 1x'),
    ('prepaused_speed', 'precondition: the world is running at 7.0x'),
    ('prepaused_epoch', 'the notification opened the pause epoch at 7x'),
    ('prepaused_save_accepted', 'pre-paused save: engine.saveWorld returned true'),
    ('prepaused_save_request', 'pre-paused save: the save transaction reported a request id'),
    ('prepaused_save_completed', 'pre-paused save: the save transaction reached a terminal SaveSucceeded before the resume'),
    ('prepaused_frozen', 'it is still paused and frozen after both'),
    ('prepaused_resumed', "the epoch's original 7x survived a second notification and a save"),
    ('before_load_speed', 'precondition: the world is running at 6.0x'),
    ('load_accepted', 'engine.loadSave was accepted'),
    ('load_published', 'the load transaction published'),
    ('load_frozen', 'the loaded session came up paused and frozen'),
    ('load_default_speed', 'the loaded session resumed at the default 1.0, never the pre-save 8x or the pre-load 6x'),
]
DESCRIPTOR = probe_protocol.build_descriptor('pause_speed', PROBE_CHECKS)

CHECK_ID_BY_LABEL = {label: cid for cid, label in PROBE_CHECKS}


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9147)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 descriptor without booting")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args, rep)
    except Exception as exc:
        rep.abort(str(exc))
        raise
    finally:
        rep.close()


def _run(args, rep):
    for name in (SAVE_NAME, SAVE_NAME_PAUSED):
        if os.path.exists(os.path.join("saves", name)):
            sys.exit(f"refusing to run: saves/{name} already exists")

    proc = boot(args.port, log=rep.engine_log_path("pause_speed_engine.log", LOG),
                args=rep.engine_args())
    chk = Checks(rep)
    try:
        send(args.port, f'world.init("{PAGE}", {args.seed}, 64, 3)',
             expect_result=False)
        rep.note(f"[init] waitForInit -> {send(args.port, 'return world.waitForInit(300)', timeout=305)}")
        send(args.port, f'world.show("{PAGE}")', expect_result=False)
        send(args.port, "require('scripts.pause')", expect_result=False)

        rep.note('\nA. a pause: true notification, resumed the way Space does')
        send(args.port, "engine.setPaused(false)", expect_result=False)
        set_scale(args.port, 10.0, chk)
        notify_pause(args.port)
        expect_paused_and_frozen(
            args.port, chk,
            "the notification paused the session and froze its clock")
        resume(args.port, "toggle()")
        expect_resumed_at(
            args.port, chk, 10.0,
            "pause.toggle resumed at the chosen 10x, not the default 1x")

        rep.note('\nB. a manual engine.saveWorld, all the way to a terminal outcome')
        set_scale(args.port, 8.0, chk)
        save_and_settle(args.port, SAVE_NAME, chk, "manual save")
        expect_paused_and_frozen(
            args.port, chk,
            "the completed save left the session paused and frozen")
        resume(args.port, "set(false)")
        expect_resumed_at(
            args.port, chk, 8.0,
            "resuming after the save returned the chosen 8x, not 1x")

        rep.note('\nC. a save taken from an ALREADY paused session')
        set_scale(args.port, 7.0, chk)
        notify_pause(args.port)
        expect_paused_and_frozen(
            args.port, chk, "the notification opened the pause epoch at 7x")
        # Both of these land on a session that is already paused and
        # already zero-scaled. Neither may re-capture that zero as the
        # speed to resume at.
        notify_pause(args.port)
        save_and_settle(args.port, SAVE_NAME_PAUSED, chk, "pre-paused save")
        expect_paused_and_frozen(
            args.port, chk, "it is still paused and frozen after both")
        resume(args.port, "set(false)")
        expect_resumed_at(
            args.port, chk, 7.0,
            "the epoch's original 7x survived a second notification and a save")

        rep.note('\nD. load policy: a published load resumes at the default speed')
        set_scale(args.port, 6.0, chk)
        loaded = send(args.port, f'return engine.loadSave("{SAVE_NAME}")')
        chk.ok(loaded.strip() == "true", "engine.loadSave was accepted",
               loaded.strip())
        published, status = wait_load_published(args.port)
        chk.ok(published, "the load transaction published", str(status))
        expect_paused_and_frozen(
            args.port, chk, "the loaded session came up paused and frozen")
        resume(args.port, "set(false)")
        expect_resumed_at(
            args.port, chk, 1.0,
            "the loaded session resumed at the default 1.0, never the "
            "pre-save 8x or the pre-load 6x")
    finally:
        quit_engine(args.port, proc)
        try:
            proc.wait(timeout=15)
        except subprocess.TimeoutExpired:
            proc.kill()
        # Safe to delete: both names are unique to this run and main()
        # refused to start if either directory already existed.
        for name in (SAVE_NAME, SAVE_NAME_PAUSED):
            shutil.rmtree(os.path.join("saves", name), ignore_errors=True)

    rep.note("")
    if chk.failed:
        rep.note(f'FAIL: {chk.failed} check(s) failed')
        return 1
    rep.note('PASS: the chosen world speed survives every pause source (#1599)')
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
