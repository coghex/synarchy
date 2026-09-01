#!/usr/bin/env python3
"""End-to-end integration gate for the unified transfer system (#1255,
epic #1013 slice UIT-6 — the arc's final child).

Every earlier slice of the arc has its own focused gate. This one proves
they hold together: ONE fixed-seed session in which an exact item
instance moves in BOTH directions between all three endpoint CLASSES
(acolyte, technomule, built storage — two of which share the contract's
`unit` endpoint KIND), reaching the same commit policy through BOTH
player modes, plus the batch, knowledge, widget and persistence
behaviours the arc's design decisions promise.

Shaped after `tools/expedition_loop_probe.py` (#923): independently
reported STAGES so a failure names which part broke, a FRESH process
that re-checks every durable identity, one deterministic `FINGERPRINT`
line so two consecutive runs can be diffed for identity AND result, and
operational failures recorded against the stage they interrupted rather
than allowed to traceback past a PASS summary.

`--offscreen` (GPU on, window off) is required and is what the bare
invocation boots: Mode A's flanking panes, Mode B's row menus and the
container window are real Vulkan-rendered UI, and the real HUD flow
never boots headless (it gates on `fontsReady`, a GPU font atlas) while
`--headless` refuses `input.*` injection outright. `--offscreen` is an
ENGINE boot mode, not a probe argument — nothing has to be passed on the
command line.

TARGETING. Two different oracles, never a hardcoded coordinate:

  * rendered UI controls (menu rows, tab boxes, item rows) are located
    through `ui.dumpWidgets()` and the item-list widget's own `dump()`;
  * WORLD ENTITIES are located and CONFIRMED through the production hit
    tests — `building.hitTestAt` for a building, `unit.hitTestInRect`
    bisection plus `unit.hitTestAt` for a unit — exactly as
    `tools/transfer_context_menu_probe.py` does, after
    `probelib.focus_and_locate` pins the camera's z-slice to the
    target's own `gridZ` (#1286).

Every item-moving gesture below is a REAL right-click on a REAL rendered
row followed by a REAL click on the located menu entry. The OPENING
routes are driven through the real right-click menu wherever the shipped
game has one — "Contents" on a storage building for the container
window, "Transfer" on a building and on a unit for a Mode A session —
and the repeats then call the same entry point that menu row's callback
calls, because what this gate measures is the transfer, not the menu
(which `tools/transfer_context_menu_probe.py` owns). A UNIT endpoint's
container window has NO world right-click route at all
(`scripts/init_context_menu.lua`'s unit menu offers Info / Attack /
Transfer / Cancel transfer and no "Contents"), so the window manager's
own entry point IS the route for one; asserting a menu row that does not
exist would be asserting a feature this arc did not ship.

ENVIRONMENT, and why each choice is load-bearing rather than incidental.
Every one of them was a live failure first:

  * the fixtures are sited on ONE LEVEL SHELF with a level corridor
    between them (`allocate_flat_anchors`), and LEVEL is meant
    literally. Seed 42's origin sits on a ridge between z 9 and z 45,
    and a carrier crossing it plays `climb` / `climb_pullup` engine
    animations during which its AI does not re-decide at all, so a
    twelve-tile leg outlives any sane budget. Worse, a drop of more than
    one z level is a FALL, and every fall is a KNOCKDOWN
    (`Unit.Thread.Movement.Timers`: Collapsed pose plus a self-timed
    get-up) — and an incapacitated endpoint ENDS a Mode A session by
    rule. That is transient enough to be invisible to any poll and
    biased toward long approaches, so it presents as an intermittent
    Mode A bug; it cost two full runs before the pose was caught in the
    act;
  * no unit is ever spawned ON a building's own tile. One that is
    accumulates wounds and is playing `injured_death` a minute later,
    which ends a Mode A session through the incapacitated-source rule
    and reads exactly like a Mode A bug;
  * the camera is settled into the tile zoom band during setup, before
    any session exists. A Mode A arrival snaps the camera, and crossing
    a zoom band is one of `view_teardown`'s triggers — it CLEARS the
    session, so one created on the post-worldgen zoomed-out view tears
    itself down the instant it opens;
  * the never-inspected fixture is retired the moment its observations
    are made. It is the world's only construction site, and
    `build_nearby` scans thirty tiles for one;
  * each Mode B carrier is sent home before its own leg, because the
    previous leg left it standing AT the endpoint it served — and the
    "no adjacency was required" half of a leg has to be a fact about the
    mode rather than about where the unit happened to be.

ISOLATION. The whole run lives on a throwaway resource root: `scripts`,
`assets` and `data` symlinked (read-only content), `config/` COPIED (the
real UI writes settings, and #1266 says a test never touches the
developer's `config/*.local.yaml`), and its own empty `saves/`. Both
processes — the first engine and the fresh-reload one — share that ONE
root and nothing else, so no save and no local configuration can leak
between two consecutive runs.

Stages, in order:

  setup       a fixed-seed world through the real create-world screen,
              the probe's own throwaway defs, and the three endpoint
              classes stocked.
  knowledge   D-2: a never-inspected container reads unknown with its
              capacity still shown and reveals nothing on being opened;
              proximity alone never reveals; contents go GENUINELY stale
              (a wildlife withdrawal mutates storage without revealing,
              because `revealContainerForUnit` is player-gated); and a
              Mode A open is what refreshes them.
  modeB       all six directed legs through the real queued gestures
              (#1249's Store/Retrieve), each committing on arrival.
  modeA       the same six directed legs through three real escort
              sessions (#1250/#1251), committing on the spot.
  batch       D-1: twelve into room for eight stores eight, reports the
              remainder, and no single item half-moves.
  widget      requirement 1d: every container view encountered in this
              run was rendered by the ONE item-list widget, asserted
              from the rendered dumps collected as each view opened.
  save        D-3: a Mode B order left IN FLIGHT and a Mode A session
              left OPEN on a DIFFERENT pair, captured into one save.
  load        a FRESH PROCESS: the order survives with its exact
              identity and a non-terminal state and then completes
              exactly once, while the session is gone and both units it
              held are free.

Both engines are also an oracle for PERSISTENCE INTEGRITY (#1487).
Each one's log is inspected once its process is gone, and any
unexpected `integrity diagnostic` line fails the run — engine A's
against the `save` stage, engine B's against `load`, so the eight-stage
report names the boundary that produced it. These are the non-blocking
dangling-reference warnings the save deliberately tolerates and
therefore only LOGS; nothing else in this file would notice one.

Known-flaky neighbours: `tools/expedition_retrieval_probe.py` and
`tools/repair_ai_probe.py` are the arc-adjacent AI probes with recorded
intermittent failures; this one is deliberately built to avoid their
failure mode by keeping the simulation PAUSED except across the walks it
actually measures.

Manual-only (needs-gpu) per `tools/ci_probes.py`; the CI-blocking gates
for this feature are the hspec groups `--match "Unit transfer"`,
`"Transfer context menu"`, `"Container knowledge"`, `"Item list widget"`,
`"persistence contract"` and `"save components"`.

MODULE OWNERSHIP. This file is the probe: the sole executable, the only
CLI, the only place an engine is booted or quit, and the only place the
run's fingerprint and stage report are published. It holds no scenario
body. The stages it calls in order, and the support they share, live in
sibling LIBRARY modules — libraries, not probes, which is why none of
them is named `*_probe.py` and why `tools/run_probes.py` still registers
exactly one `unified_transfer` entry:

  unified_transfer_probe_support.py      constants, the authored item and
                                         building YAML, `Checks`,
                                         resource-root isolation, engine
                                         boot, the #1487 integrity oracle
  unified_transfer_probe_world.py        the widget oracle, world-entity
                                         hit testing and camera
                                         targeting, the contract readers,
                                         terrain/anchor allocation, unit
                                         control and fixture staging
  unified_transfer_probe_setup.py        the `setup` stage
  unified_transfer_probe_knowledge.py    the `knowledge` stage
  unified_transfer_probe_mode_b.py       the `modeB` stage and its legs
  unified_transfer_probe_mode_a.py       the `modeA` stage, its legs, and
                                         the escort-session lifecycle the
                                         other stages open sessions
                                         through
  unified_transfer_probe_batch.py        the `batch` and `widget` stages
  unified_transfer_probe_persistence.py  the `save` and `load` stages

Shared state is passed EXPLICITLY — the `Checks` recorder, the
`ViewLedger`, the fixture-id map, the viewport and the fingerprint dict
are constructed here (or by `setup`) and handed down. A stage still
writes its own fingerprint section, as it always has; what is centralized
is the final serialization, not the contribution.

Usage: python3 tools/unified_transfer_probe.py
       [--port 9432] [--size 1280x900] [--seed 42] [--world-size 64]
       [--keep-root]

Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import sys
import tempfile
import traceback

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import quit_engine
from unified_transfer_probe_support import (Checks, SetupError, boot_offscreen,
                                            check_boundary_log,
                                            make_isolated_root)
from unified_transfer_probe_world import ViewLedger
from unified_transfer_probe_setup import stage_setup
from unified_transfer_probe_knowledge import stage_knowledge
from unified_transfer_probe_mode_b import stage_mode_b
from unified_transfer_probe_mode_a import stage_mode_a
from unified_transfer_probe_batch import stage_batch, stage_widget
from unified_transfer_probe_persistence import stage_load, stage_save


def main() -> int:
    ap = argparse.ArgumentParser(description="Unified transfer system gate")
    ap.add_argument("--port", type=int, default=9432)
    ap.add_argument("--size", default="1280x900")
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--world-size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    ap.add_argument("--keep-root", action="store_true",
                    help="don't delete the throwaway resource root on exit")
    args = ap.parse_args()

    chk = Checks()
    ledger = ViewLedger()
    base = tempfile.mkdtemp(prefix="synarchy_unified_transfer_")
    root = make_isolated_root(base)
    # Each run owns its two engine logs (#1487). They used to be one
    # fixed pair of `/tmp` names shared by every invocation, which was
    # harmless while nothing read them and is not now that they are this
    # run's integrity ORACLE: two invocations on different `--port`
    # values would write the same two files, and whichever finished last
    # would decide what the other one inspected. Siting them beside the
    # throwaway resource root makes each pair demonstrably this
    # invocation's; `--keep-root` is what preserves them afterwards.
    log_a = os.path.join(base, "engine_a.log")
    log_b = os.path.join(base, "engine_b.log")
    print(f"isolated resource root: {root}", flush=True)
    print(f"engine logs: {log_a} / {log_b}", flush=True)
    fp: dict = {"seed": args.seed, "worldSize": args.world_size,
                "plates": args.plates}
    port = args.port
    state = None

    try:
        # ============ engine A: the whole scenario, then a save =========
        proc = boot_offscreen(root, port, args.size, log_a, "engine A")
        try:
            chk.enter("setup", "a fixed-seed world and the three endpoint "
                               "classes")
            ids, vp = stage_setup(chk, port, fp, base, args)
            if ids is None:
                raise SetupError("the scenario's fixtures could not be built")

            chk.enter("knowledge", "D-2: contents are genuinely stale, and "
                                   "only an interaction refreshes them")
            stage_knowledge(chk, port, ledger, ids, fp, vp)

            chk.enter("modeB", "all six directed legs through the queued "
                               "gestures")
            stage_mode_b(chk, port, ledger, ids, fp, vp)

            chk.enter("modeA", "the same six legs through three real escort "
                               "sessions")
            stage_mode_a(chk, port, ledger, ids, fp, vp)

            chk.enter("batch", "D-1: twelve into room for eight")
            stage_batch(chk, port, ids, fp, vp)

            chk.enter("widget", "one widget rendered every container view")
            stage_widget(chk, ledger, fp)

            chk.enter("save", "a Mode B order in flight and a Mode A session "
                              "open, in one save")
            state = stage_save(chk, port, ids, fp, vp)
        finally:
            quit_engine(port, proc)
            check_boundary_log(chk, "save", "engine A", log_a, proc)

        # ============ engine B: a genuinely fresh process ===============
        chk.enter("load", "a fresh process re-checks every durable identity")
        if state is None:
            chk.ok(False, "load: the save stage produced nothing to reload")
        else:
            proc = boot_offscreen(root, port, args.size, log_b, "engine B")
            try:
                stage_load(chk, port, fp, base, state, args)
            finally:
                quit_engine(port, proc)
                check_boundary_log(chk, "load", "engine B", log_b, proc)
    except SetupError as exc:
        chk.ok(False, f"the scenario could not reach the state it tests: {exc}")
    except SystemExit as exc:
        # `probelib.boot` reports an engine that died before READY, or
        # never printed it, by calling sys.exit() — and SystemExit derives
        # from BaseException, not Exception, so the clause below does not
        # see it. Left uncaught it would unwind straight through the
        # finally, which prints the stage summary first: a stage entered
        # but not yet asserted in would be reported as passing on the way
        # out.
        chk.ok(False, f"the engine could not be started, or died, during stage "
                      f"'{chk.stage}' (SystemExit: {exc.code})")
    except Exception as exc:  # noqa: BLE001
        # An operational failure — a dead engine, a socket timeout, a
        # malformed console response — is a real probe failure and must
        # name its stage like any other. Left to propagate it would exit
        # non-zero with a traceback but NO recorded failing check, and the
        # summary below would then print PASS over the top of it.
        # KeyboardInterrupt is deliberately still allowed to propagate.
        chk.ok(False, f"unexpected {type(exc).__name__} while running stage "
                      f"'{chk.stage}': {exc}")
        traceback.print_exc()
    finally:
        if args.keep_root:
            print(f"kept resource root: {base}", flush=True)
        else:
            shutil.rmtree(base, ignore_errors=True)
        fp["stages"] = chk.outcomes()
        print(f"\nFINGERPRINT {json.dumps(fp, sort_keys=True)}", flush=True)
        chk.report()

    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
