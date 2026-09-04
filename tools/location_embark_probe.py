#!/usr/bin/env python3
"""Embark-to-discovery end-to-end GPU probe (#782) — the final #159
locations-arc integration gate.

Boots the real offscreen graphical profile (GPU on, window off) and
drives the whole embark flow through the SAME player-facing paths a
real session uses: zoom-map icon inspection, build-tool ghost
validity, the remote-settlement confirmation modal, and — the part no
other probe exercises — discovery triggered by a REAL unit movement
ORDER issued through real input (click-select + right-click move),
never by a direct `unitAi.commandMove`/`unit.setPos` debug-console
call. Debug-console queries are used only to locate generated
locations, inspect authoritative state, pick test coordinates, and
verify results (`world.listPlacedLocations`, `building.canPlaceAt`,
`building.remoteCheck`, `unit.hitTestAt`, `unit.getInfo`,
`engine.getEventLog`) — every player-visible action (portal placement,
modal interaction, zoom-map viewing, unit selection, unit orders) goes
through `input.*`/`ui.dumpWidgets`.

This is deliberately NOT a re-derivation of ground already covered by
landed GPU probes:
  * tools/portal_ghost_probe.py already proves the exact white-vs-red
    ghost RGB tint direction and that an invalid click spawns nothing
    — this probe reuses `canPlaceAt` (the oracle that DRIVES that
    tint) plus screenshot-difference checks rather than re-deriving
    the pixel math.
  * tools/offscreen_probe.py's phases 6-7 already cover the remote
    modal's Choose-Another-Site/Establish-Here branches and paired
    discovery-state icons via a directly-spawned unit.
  * The pure "Location map icons" Hspec group proves the exact
    wrap/seam/upright icon geometry.

What's unique here is the single uninterrupted player-facing session:
inspecting BOTH ruins' hidden icons before any portal exists, the
overlap-rejection ghost, the remote-vs-local placement branches (in
two isolated sessions — the starting portal is unique, so a session
that already placed one remotely can't also exercise the canonical
local-start branch), the real portal roster spawn, discovery driven by
a real move ORDER (not a teleport), event-log assertions (exactly one
`location_discovery` event, none on re-entry), and persistence through
a real save -> quit -> fresh restart -> load.

Two phases:
  1. `--headless` (no GPU): generate a real world containing at least
     two `ruin_small` locations (retrying alternate seeds if the
     default seed doesn't place enough — changing world-generation
     location density is out of scope, so this is handled by seed
     selection, never by generation changes) and save it — the fixture
     both GPU sessions below load via the production Load Game path
     (`scripts.main_menu.loadAndShowSave`), so worldgen is paid once.
  2. `--offscreen` (GPU on, window off), THREE sessions against that
     one fixture:
       a. ghost validity + remote-modal cancel/confirm (never saved
          back over the fixture, so session (b) starts from the same
          clean, portal-free world);
       b. canonical local placement, the real portal roster, real
          click-select + right-click move-order discovery, the
          re-entry no-duplicate-event check, then a save under a new
          name;
       c. fresh restart -> load that save -> verify the discovered/
          undiscovered icon state and location count survived intact.

Needs a GPU (Vulkan device) — manual-only, never CI-gated, same as
tools/offscreen_probe.py / tools/portal_ghost_probe.py.

Every artifact this probe creates — the four engine logs, the two save
slots, and the screenshots — lives under ONE directory this invocation
owns (#1569), and the run deletes that directory again whether it
passes or fails. The engines boot with that directory's own resource
root, so the save slots below are unreachable from an ordinary
`cabal run` and the developer's live `saves/` is neither written nor
rotated. `--keep-artifacts` is the explicit opt-in that retains the
directory instead, for diagnosing a failure.
Who owns what (#2164). This file is the whole visible lifecycle and
nothing else: it parses the command line, allocates the one artifact
root, runs phase 0 and the three sessions in order, and owns the single
aggregate result. Everything each of those phases actually does lives
under `tools/location_embark/`, which registers no probe of its own and
has no entry point:

  * `constants` — the fixture page, the two save slots, and the portal
    and ruin identifiers every owner shares;
  * `invocation` — the aggregate failure ledger and engine-log context,
    the invocation-owned artifact tree and its release (#1569), the
    request-specific save publication (#1746), and the `SessionContext`
    this file threads from one session to the next;
  * `support` — the engine reads and real-input gestures more than one
    session needs;
  * `fixture` — phase 0: the fallback-seed search and its durable
    `SAVE_BASE`;
  * `session_ghost`, `session_discovery`, `session_reload` — sessions
    (a), (b) and (c).

There is still exactly ONE registered `location_embark` probe, one
`--only location_embark` command, and one aggregate pass/fail.

Usage:
  python3 tools/location_embark_probe.py
  python3 tools/location_embark_probe.py --seed 42 --size 64 --port 9420
  python3 tools/location_embark_probe.py --keep-artifacts
"""
from __future__ import annotations

import argparse
import os
import sys
import tempfile
import traceback

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot, quit_engine
from probe_runner_diagnostics import FailureEmitter   # noqa: E402 - durable failure records (#1982)

from location_embark.constants import SAVE_BASE, SAVE_LOCAL
from location_embark.fixture import prepare_fixture
from location_embark.invocation import (RunArtifacts, SessionContext, check,
                                        current_log, failures,
                                        release_artifacts, set_log)
from location_embark.session_discovery import session_local_and_discovery
from location_embark.session_ghost import session_ghost_and_remote
from location_embark.session_reload import session_reload_check

#: #1982 — this run's durable failure records. Built at import, so the
#: offset every record carries is measured from the probe's own start;
#: emitted by `report` below instead of the unflushed `FAIL:` print that
#: the runner's block-buffered pipe used to strand above its 25-line
#: tail. It stays in the file the runner launches, beside the `report`
#: that is its only caller.
FAILURE = FailureEmitter("location_embark_probe")

# `run_probe` below resolves `prepare_fixture`, `boot`, `quit_engine` and
# the three `session_*` names in THIS module's globals, so
# `tools/test_location_embark_probe.py` intercepts them by patching them
# here. The names it patches for `save_and_wait` — `send`,
# `capture_request_id`, `wait_save_complete` — and `REPO`, which
# `RunArtifacts.build` reads, are NOT imported here: they are resolved in
# `location_embark.invocation`, which is the module that has to be
# patched for them (the lesson #2095 recorded, and the reason that
# companion asserts its stubs really intercept).


def run_probe(args, w: int, h: int, art: RunArtifacts) -> None:
    """Phase 0 plus the three offscreen sessions, all inside the caller's
    cleanup guard. Every `boot` here goes through `art.boot_args`, and
    every session shuts its engine down in a `finally` before control
    can return to that guard.

    This function is the ordered sequence in full: build the artifact
    tree, try the fallback seeds, suppress every session unless
    `SAVE_BASE` is durable, run (a), (b) and — only if (b) published
    `SAVE_LOCAL` — (c), each in its own booted-and-quit engine. No owner
    under `tools/location_embark/` boots on its own behalf, opens a port,
    or decides the run's exit; each is handed the port this function
    opened and the one `SessionContext` it resolved."""
    art.build()
    # Named while the run is live so its logs can be tailed, and named
    # honestly: without the opt-in this path is gone by the time the
    # summary prints, and only the summary is allowed to point at
    # artifacts that are still there.
    print(f"isolated resource root: {art.root}"
          + ("" if args.keep_artifacts
             else " (removed on exit; pass --keep-artifacts to retain it)"))
    seeds = [args.seed] + [int(s) for s in args.alt_seeds.split(",") if s.strip()]

    print(f"== phase 0: headless fixture prep (size {args.size}) ==")
    used_seed, ruins = prepare_fixture(args.port, seeds, args.size, art)
    if not check("a candidate seed placed at least two ruin_small locations",
                  len(ruins) >= 2,
                  f"tried seeds {seeds}, best count {len(ruins)}"):
        return
    if used_seed is None:
        # A seed qualified but its save was refused or never completed;
        # save_and_wait already recorded which step failed and why.
        # Sessions (a) and (b) both LOAD this slot, and (c) loads what
        # (b) would have saved, so none of them may run (#1746).
        print("  sessions (a), (b) and (c) skipped: the fixture save never "
              f"reached SaveCaptureComplete, so '{SAVE_BASE}' is not durable")
        return

    # Every fact the three sessions share, resolved once, here: the
    # seed phase 0 settled on, the two ruin identities in their
    # deterministic order, how many locations a reload must still find,
    # the framebuffer this run booted with, and the screenshot root the
    # icon comparisons cross between sessions.
    ruins_sorted = sorted(ruins, key=lambda e: (e["cx"], e["cy"]))
    ctx = SessionContext(port=args.port, w=w, h=h, shots=art.shots,
                         target=ruins_sorted[0], control=ruins_sorted[1],
                         seed=used_seed, expected_total=len(ruins))
    print(f"  fixture ready: seed={ctx.seed}, {len(ruins)} ruin(s), "
          f"saved as '{SAVE_BASE}'")

    win = art.boot_args(["--size", args.win_size])

    print("== session (a): zoom-map icons, ghost validity, remote-modal flow ==")
    set_log(art.log("engine_session_a"))
    proc1 = boot(ctx.port, mode=("--offscreen",), args=win,
                 log=art.log("engine_session_a"),
                 label="offscreen engine (session a)")
    try:
        session_ghost_and_remote(ctx)
    finally:
        quit_engine(ctx.port, proc1)

    print("== session (b): local placement, roster, real-order discovery, save ==")
    set_log(art.log("engine_session_b"))
    proc2 = boot(ctx.port, mode=("--offscreen",), args=win,
                 log=art.log("engine_session_b"),
                 label="offscreen engine (session b)")
    try:
        saved_local = session_local_and_discovery(ctx)
    finally:
        quit_engine(ctx.port, proc2)

    if not saved_local:
        # Session (c) is the fresh-process half of the save -> quit ->
        # restart -> load proof, and it reads SAVE_LOCAL. Without a
        # completed save of that slot the load has nothing durable to
        # find, and its failure would be attributed to the load rather
        # than to the save that never finished (#1746).
        print("== session (c) skipped: session (b) published no durable "
              f"'{SAVE_LOCAL}' ==")
        return

    print("== session (c): fresh restart -> load -> verify persistence ==")
    set_log(art.log("engine_session_c"))
    proc3 = boot(ctx.port, mode=("--offscreen",), args=win,
                 log=art.log("engine_session_c"),
                 label="offscreen engine (session c)")
    try:
        session_reload_check(ctx)
    finally:
        quit_engine(ctx.port, proc3)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--alt-seeds", default="7,99,123,2026",
                    help="comma-separated fallback seeds tried in order if "
                         "--seed doesn't place >= 2 ruin_small locations")
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9420)
    ap.add_argument("--win-size", default="1280x720")
    ap.add_argument("--keep-artifacts", action="store_true",
                    help="keep this run's artifact directory (engine logs, "
                         "isolated resource root with its saves, and "
                         "screenshots) instead of deleting it, and name it "
                         "in the summary — for diagnosing a failure")
    args = ap.parse_args()

    w, h = (int(v) for v in args.win_size.lower().split("x"))
    # The guard starts HERE, one statement after the directory exists,
    # so nothing between this point and the report below — building the
    # isolated root, a phase-0 seed that never boots an engine, an early
    # return, a dead engine, an unexpected exception — can leave the
    # tree behind.
    art = RunArtifacts(tempfile.mkdtemp(prefix="synarchy_location_embark_"))
    try:
        run_probe(args, w, h, art)
    except KeyboardInterrupt:
        release_artifacts(art, args.keep_artifacts)
        raise
    except SystemExit as exc:
        # `probelib.boot` aborts the run this way when an engine dies
        # before READY or never prints it. Recording it as a failing
        # check rather than letting it exit keeps the artifact release
        # below on the path, and names the abort in the summary.
        failures.append(f"the run aborted before finishing: {exc}")
    except Exception as exc:  # noqa: BLE001 - reported, then re-summarised
        failures.append(f"unexpected {type(exc).__name__} during the run: {exc}")
        traceback.print_exc()
    return report(art, args.keep_artifacts)


def report(art: RunArtifacts, keep: bool) -> int:
    if failures:
        # BEFORE release_artifacts below, which removes the tree the
        # engine log lives inside. Requirement 4 is met by retaining a
        # BOUNDED excerpt of that log in the capture, never by keeping the
        # tree, so requirement 5's unconditional cleanup is untouched.
        FAILURE.context_log(current_log())
    release_artifacts(art, keep)
    print("-" * 56)
    if failures:
        # Durable records rather than an unflushed stderr print (#1982):
        # the runner reads these back from the COMPLETE capture, so a
        # failed check survives however much output followed it. Emitted
        # after release_artifacts because that call can record a leftover
        # of its own, which belongs in the same block.
        FAILURE.report(failures)
        FAILURE.context("artifact root",
                        f"{art.base} ({'retained' if keep else 'removed'})")
        print(f"location_embark_probe: {len(failures)} check(s) FAILED")
        if not keep:
            print("  (re-run with --keep-artifacts to retain this run's "
                  "engine logs, saves and screenshots)")
        return 1
    print("location_embark_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
