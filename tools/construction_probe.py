#!/usr/bin/env python3
"""Headless construction build-job probe (#96).

Verifies the construct_job AI end-to-end on a flat arena world, WITHOUT a
GPU or a human watching: acolytes claim construction designations (#95),
source materials (inventory → ground items → technomule), pour progress
into the job, place the structure piece, and stake designated buildings
for the existing deliver/build machinery. (A structure ghost does not
solidify with that progress: since #1846 it disappears once materials
are paid for and the site is empty until the piece lands.)

Phases (each spawns its own units and destroys them afterwards so the
next phase's utility scan is clean):
  1. inventory : two floor designations; an acolyte carrying the plates
                 claims (status observable as "claimed"), builds, places
                 both floors, and the designations clear.
  2. ground    : one wall designation; the bars are GROUND items — the
                 acolyte hauls them first (sourcing ladder rung 2).
  3. release   : the claimant is destroyed mid-job; the sweep releases
                 the claim ("pending" again — observable), and a second
                 acolyte picks the job up and finishes it.
  4. occupied  : #1595 — a still-empty tile whose first job is merely
                 PENDING refuses a second designation (rejected 1/0/1,
                 first job untouched). #805 — re-designating an
                 already-built floor's slot is rejected (no job, outcome
                 stream says so), a DIFFERENT slot on that same tile
                 still designates/builds normally once the first job has
                 COMPLETED (coexistence), and a slot that fills in AFTER
                 designation but BEFORE a claimant pays materials is
                 cancelled rather than paid for.
  6. paid_death: #799 — a claimant that dies AFTER paying (progress > 0)
                 leaves the designation's durable 'paid' marker set; a
                 materials-less replacement still finishes the job,
                 proving the cost isn't charged a second time.
  7. preemption: #799 — an ordinary mid-build interruption (the same
                 reset constructOnExit performs) must not re-attempt
                 payment; the claimant carries exactly the required
                 amount with no ground/mule fallback, so a duplicate
                 charge would stall the job outright.
  8. save_load : #799 — a paid, partially-built designation survives a
                 save/load round-trip (paid + progress intact) even
                 though the paying unit does NOT survive to reload with
                 it; a materials-less unit finishes it after load.
  9. cancel    : #799 — the build-tool's right-click cancel refunds an
                 already-PAID structure's materials to the ground (and
                 never fabricates materials for an unpaid one); a
                 placement failure (a post's floor pulled out from under
                 it) refunds too, plus a player-observable notification,
                 instead of completing with the materials simply gone.
  5. stake     : a building designation; the acolyte walks up and stakes
                 it (building.spawn) and the blueprint completes. Runs
                 LAST: the staked portal spawns its roster, which would
                 contaminate later phases.

Usage:
  python3 tools/construction_probe.py [--port 9377] [--phase all]

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import shutil
import socket
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from probelib import clear_find_water, quit_engine, boot, send, send_json, wait_load_published

LOG = "/tmp/construction_probe_engine.log"
REPO = Path(__file__).resolve().parent.parent


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory, so this probe never touches a real player's saves
    (round-6 review, issue #767 requirement 15's cross-referenced-probe
    isolation gap)."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def bootstrap(port: int) -> None:
    """Load defs + the flat arena (the loading screen doesn't run headless).
    unit_ai is auto-loaded at boot and IS the machinery under test, so it
    stays live (unlike movement_probe, which neutralises it)."""
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    send(port,
         "return require('scripts.movement_arena').buildCourse('flat').name")
    # buildCourse queues world.initArena, world.initArenaDone and
    # world.show together, so the arena page does not merely become
    # VISIBLE a tick later — it does not EXIST until the world thread
    # runs the init. Wait for it to be applied before designating: a
    # designate against a not-yet-active page id is a silent no-op, and
    # every helper below reads world.getActiveWorldId().
    #
    # This is NOT the chunk-page binding #2310 fixed, and the fix does
    # not remove the need for it. That one is about which page bulk
    # chunk work is ADMITTED to while a world.show sits queued, and
    # world.loadChunksInRegion now resolves the projected page itself.
    # A page that is only projected has no WorldState yet, so it queues
    # nothing and reports 0 rather than falling back — which is exactly
    # this window. Arena registration and application still have to
    # finish first.
    if not poll_until(port, 30, lambda: wid(port)):
        sys.exit("arena page never became the active world")
    # Make sure the chunks around the test sites exist before designating
    # (designation skips unloaded-chunk tiles). The flat arena's footprint
    # is a fixed 5x5 chunk grid centred on the origin (arenaRadius = 2,
    # World.Generate.Arena) — chunks -2..2 (tiles -32..47). Requesting
    # chunks OUTSIDE that footprint falls through to the real terrain
    # generator against the arena's synthetic gen params and crashes the
    # world thread, so this loads the arena's FULL footprint (needed for
    # the #799 phases' negative-Y sites below) and no further.
    send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)")
    send(port, "return world.waitForChunks(60)", timeout=65.0)


def wid(port: int) -> str | None:
    """APPLIED world page id, or None while the arena's init/show burst
    is still queued on the world thread. The console JSON-encodes strings
    (quotes included) and prints null for nil — both need unwrapping
    before the id is re-usable in Lua."""
    raw = send(port, "return world.getActiveWorldId()")
    raw = raw.strip().strip('"')
    return raw if raw and raw not in ("null", "nil") else None


def designation_at(port: int, x: int, y: int):
    return send_json(
        port,
        f"return construction.getDesignationAt(world.getActiveWorldId(), {x}, {y})")


def pick_tile(port: int, sx: int, sy: int) -> tuple[int, int]:
    """world.pickTile(screenX, screenY) -> (gx, gy) — the SAME hit test
    buildTool.handleMouseDown runs on a real click (mirrors wire_probe.py's
    helper of the same name)."""
    raw = send(port, f"return world.pickTile({sx}, {sy})")
    parts = raw.split()
    return int(float(parts[0])), int(float(parts[1]))


def ground_count(port: int, def_name: str) -> int:
    raw = send(port,
                "local n=0; for _,g in ipairs(item.listGround() or {}) do "
                f"if g.defName=='{def_name}' then n=n+1 end end; return n")
    return int(float(raw))


def spawn_acolyte(port: int, x: float, y: float) -> int:
    uid = send(port, f"return unit.spawn('acolyte', {x}, {y})")
    try:
        n = int(float(uid))
    except ValueError:
        sys.exit(f"unit.spawn failed: {uid!r}")
    # Acolytes spawn at (or beyond) carrying capacity — the engine
    # already drops kit at spawn to fit. Shed the heavy tools so the
    # probe's added build materials don't push the unit over capacity
    # (an over-capacity unit can't haul, which reads as a stuck job).
    for it in ("pick_steel", "shovel_steel", "axe_steel",
               "rations", "rations"):
        send(port, f"unit.removeItem({n}, '{it}'); return 'ok'")
    # Retire the spawn-seeded find_water goal: the arena has no water,
    # so the goal never completes and its search utility (3.0) can edge
    # out a construct job a few tiles away — an artifact of the
    # waterless test world, not the arbitration under test.
    if not clear_find_water(port, n):
        sys.exit(f"unit {n} never got AI state")
    return n


def destroy_unit(port: int, uid: int) -> None:
    send(port, f"unit.destroy({uid}); return 'ok'")


def poll_until(port: int, seconds: float, fn):
    """Poll fn() every 0.3 s until truthy or the budget runs out."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        v = fn()
        if v:
            return v
        time.sleep(0.3)
    return None


def poll_fast(seconds: float, fn):
    """Like poll_until, but at a 0.05 s cadence — for the #799
    cancel/placement-failure checks that must act on the FIRST tick a
    designation shows paid=true, before further build progress lands."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        v = fn()
        if v:
            return v
        time.sleep(0.05)
    return None


CHECKS: list[tuple[str, bool]] = []


def check(label: str, ok: bool) -> None:
    CHECKS.append((label, bool(ok)))
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}")


# --- construct_job timeout diagnostics (#2172) -------------------------
#
# Every construct_job wait below used to leave NOTHING behind on expiry:
# poll_until returns None and check prints a bare [FAIL]. The state that
# classifies such a miss — which phase the worker's own job was in, what
# the designation record actually said, where the worker stood — was
# gone by the time anyone read the output, so a single dropped wait
# could not be told apart from a real product failure.
#
# This is #1758's craft_timeout_bundle shape (tools/power_workshop_probe
# .py:322-378) applied to construct_job: capture on expiry only, print
# beneath the FAIL line, leave a passing run's output untouched.
#
# IN SCOPE: every bounded wait whose success depends on a spawned
# worker's construct_job producing a designation state, a progress
# artifact, a placement, a cancellation, or a staking outcome. The
# phase-1 transition wait added below is one of them.
#
# NOT IN SCOPE, and deliberately uninstrumented: the bootstrap wait for
# the arena page (`bootstrap`), the direct fixture placements that call
# `scripts.structures` themselves with no worker involved, the
# pending-refusal cleanup wait, and phase 8's post-load waits — the load
# transaction (`wait_load_published`) and the designation-survives-load
# poll, whose paying unit is destroyed before the save and whose
# successor is not spawned until afterwards. None of them is waiting on
# a construct_job, so a bundle describing one would name a worker that
# is not driving the wait.

CONSTRUCT_BUNDLE_FAMILIES = ("designations", "aiState", "unit")


def construct_job_state(port: int, uid: int):
    """The worker's own construct_job, as `scripts/unit_ai` sees it.

    One console round trip. Every field falls back to ``false`` rather
    than ``nil``: Lua drops nil fields on the way out, so an absent
    value would vanish from the record entirely and read as "not
    captured" instead of "the AI had nothing there". ``None`` for the
    WHOLE record means the unit has no AI state at all — normally
    because it no longer exists.

    ``x``/``y``/``attempt`` are the job's own identity
    (`scripts/unit_ai_construct.lua:255`), so a caller can tell a job on
    the designation it is watching from one on a neighbouring tile or a
    successor attempt at the same tile. ``constructCandidate`` separates
    a claim-stage miss that never scanned the job from one that scanned
    it and lost the arbitration.
    """
    return send_json(port,
        f"local s = require('scripts.unit_ai').getState({uid}); "
        "if not s then return nil end; local j = s.constructJob; "
        "return {currentAction = s.currentAction or false, "
        "constructJob = j ~= nil, "
        "constructCandidate = s.constructCandidate ~= nil, "
        "phase = j and j.phase or false, "
        "x = j and j.x or false, "
        "y = j and j.y or false, "
        "attempt = j and j.attempt or false}")


def construct_segment(state) -> str:
    """Which construct_job segment a worker was sampled in.

    Names where the worker WAS, not necessarily the root cause:
    `releaseConstructJob` (`scripts/unit_ai_construct.lua:80-91`) clears
    the job when the designation vanishes or a successor attempt takes
    the tile, so "no constructJob" does not prove the job was never
    claimed.
    """
    if not isinstance(state, dict):
        return "no AI state (unit gone?)"
    if not state.get("constructJob"):
        return "no constructJob"
    return str(state.get("phase") or "unknown phase")


def construct_timeout_bundle(port: int, coords, uid: int | None,
                             state=None) -> dict:
    """Read-only state that classifies an expired construct_job wait.

    ``coords`` is every tile the expired predicate queried. Each one's
    WHOLE ``construction.getDesignationAt`` record (status, progress,
    paid, attempt, kind, edge) is retained under its own ``"x,y"`` key,
    including an explicit ``null`` for a tile whose designation has
    already cleared — a predicate spanning two designations cannot be
    reconstructed from a single record, and "cleared" is a different
    fact from "not queried".

    ``uid`` is the unit intended to DRIVE that wait. A unit that no
    longer exists yields explicit ``null`` for both ``aiState`` and
    ``unit``, so a live ``aiState`` beside a live ``unit`` is the only
    shape that reads as "the unit is alive", and a populated
    ``aiState`` always describes a unit that still exists. ``state``
    lets a caller hand over an AI sample it has ALREADY taken (the
    phase-1 transition wait keeps its last one) instead of taking a
    fresh one after the fact.

    EXISTENCE IS ESTABLISHED FIRST, and it gates the AI family — a
    handed-over ``state`` included. `unitAi.aiState`
    (`scripts/unit_ai_core.lua:80`) is never pruned when an individual
    unit is destroyed; it is emptied only at teardown or a load
    reconciliation. So `getState` would happily return a dead worker's
    last decision, and a sample taken while it was alive is equally
    stale — either would print a phase and a `currentAction` next to
    `unit: null` and invite a classification the engine no longer
    supports.

    Read-only throughout: it must not perturb the scenario it is
    describing, and callers must run it immediately on expiry, before
    any further stimulus reaches the engine.
    """
    designations = {}
    for (x, y) in coords:
        designations[f"{x},{y}"] = designation_at(port, x, y)
    if uid is None:
        ai, info = None, None
    else:
        info = send_json(port, f"return unit.getInfo({uid})")
        if info is None:
            ai = None
        else:
            ai = state if state is not None else construct_job_state(port, uid)
    return {"designations": designations, "aiState": ai, "unit": info}


def emit_timeout_bundle(bundle) -> None:
    """Print a captured `construct_timeout_bundle` beneath its FAIL line.

    ``None`` means the poll succeeded, so a passing run's output is
    unchanged. One line per family, always all three, so a family the
    engine had nothing for reads as an explicit ``null``.
    """
    if bundle is None:
        return
    for family in CONSTRUCT_BUNDLE_FAMILIES:
        print(f"    (debug) {family}: "
              f"{json.dumps(bundle.get(family), sort_keys=True)}")


# --- phases ------------------------------------------------------------


PHASE1_FLOORS = ((8, 8), (9, 8))
PROGRESS_LABEL = "build progress accrued on the designation"

# Phase 1's two post-claim segments (#2172).
#
# The old single 30 s window spanned fetch, walking, utility arbitration
# AND the build itself, so an expiry could not say which of them ran
# long — and a job still in 'fetch' failed an assertion about progress
# ACCRUAL, which it had not yet had the chance to produce. The two
# segments are now bounded separately and both are measured on every
# run, so a later miss has a recorded distribution to be compared
# against.
#
# Measured on master@da96202c8 (2026-09-05, M-series laptop, one engine
# at a time, with an unrelated GHC build occupying the machine — the
# ordinary condition this manual-only probe is run under). Six solo runs
# of `python3 tools/construction_probe.py --port 9377`, all 72/72; the
# last three are the consecutive runs against this file as committed:
#
#   claim → 'building'   'building' → progress
#     8.69 s               0.31 s     } three runs measured while the
#    17.94 s               0.93 s     } budget below was still the
#    26.27 s               0.32 s     } original single 30 s window
#    30.90 s               0.31 s     } three consecutive runs against
#     4.95 s               0.31 s     } this file as committed
#    22.56 s               0.96 s     }
#
# That record is why the transition gets a budget of its own AND a
# larger one. The transition alone spans 5-31 s while the accrual it
# was lumped in with never exceeds 1 s — so the old single 30 s window
# was, in practice, a budget on fetch + walking + utility arbitration,
# and the 30.90 s run above would have blown it. The assertion it would
# have failed is the one about progress ACCRUAL, in a run whose later
# slope and terminal checks then pass: exactly the retained TH-3 shape
# (`docs/coordinated_test_harness_findings.md`).
#
# 60 s is ~1.9x the worst of the six. Widening it costs nothing this
# probe was relying on: the wait decides only HOW a miss is classified,
# while phase 1's terminal 60 s "both floors placed and designations
# cleared" check still gates the outcome on its own budget.
#
# BUILDING_TO_PROGRESS_S is the newly separated budget. Progress lands
# on the first think tick after the building branch is entered
# (`thought_interval` 1.0 s, `scripts/unit_ai_construct.lua:423-449`),
# and the record above tops out at 0.96 s, so 15 s is 15x the worst
# observed — and, unlike the old window, independent of the fetch/walk
# variance above.
CLAIM_TO_BUILDING_S = 60.0
BUILDING_TO_PROGRESS_S = 15.0


def _latency_line(label: str, seconds, note: str) -> None:
    """One phase-1 latency line. Never invents a number: a transition
    that was not observed prints why, and the bound it ran out of."""
    if seconds is None:
        print(f"  [latency] {label}: unavailable — {note}")
    else:
        print(f"  [latency] {label}: {seconds:.2f} s ({note})")


def phase_inventory(port: int) -> None:
    """Two floors, materials already in the builder's inventory."""
    print("\n[phase 1] structure job from INVENTORY (2 floors)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, 8, 9, 8, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("both tiles designated",
          designation_at(port, 8, 8) is not None
          and designation_at(port, 9, 8) is not None)

    uid = spawn_acolyte(port, 5.5, 8.5)
    send(port, f"unit.addItem({uid}, 'steel_plate', 0); "
               f"unit.addItem({uid}, 'steel_plate', 0); return 'ok'")

    # Latch ONE designation attempt and measure everything below about
    # that one: the first record observed 'claimed', by coordinate AND
    # attempt id. Without the latch the two-floor predicate could pair a
    # claim on (8,8) with progress on (9,8) — two different jobs — and
    # the latencies would describe neither.
    def first_claimed():
        for (x, y) in PHASE1_FLOORS:
            d = designation_at(port, x, y)
            if isinstance(d, dict) and d.get("status") == "claimed":
                return (x, y, d.get("attempt"))
        return None

    claimed = poll_until(port, 20, first_claimed)
    claimed_at = time.time()
    claim_bundle = (None if claimed
                    else construct_timeout_bundle(port, PHASE1_FLOORS, uid))
    check("a designation became 'claimed'", claimed is not None)
    emit_timeout_bundle(claim_bundle)

    # With nothing latched the claim assertion has already failed; the
    # remaining checks still run, in order, against both floors and any
    # attempt.
    site = claimed[:2] if claimed else None
    attempt = claimed[2] if claimed else None
    watched = [site] if site else list(PHASE1_FLOORS)

    def latched_progress():
        for (x, y) in watched:
            d = designation_at(port, x, y)
            if (isinstance(d, dict) and d.get("progress", 0) > 0
                    and (attempt is None or d.get("attempt") == attempt)):
                return d
        return None

    obs = {"ai": None, "building_at": None, "progress_at": None}

    def sample() -> None:
        """One observation of the latched attempt: the worker's own job
        phase, then that designation's progress. Each is timestamped the
        FIRST time it is seen and never overwritten."""
        st = construct_job_state(port, uid)
        obs["ai"] = st
        if (obs["building_at"] is None
                and isinstance(st, dict) and st.get("constructJob")
                and st.get("phase") == "building"
                and (site is None
                     or (st.get("x") == site[0] and st.get("y") == site[1]))
                and (attempt is None or st.get("attempt") == attempt)):
            obs["building_at"] = time.time()
        if obs["progress_at"] is None and latched_progress() is not None:
            obs["progress_at"] = time.time()

    def saw_transition() -> bool:
        # Positive progress is itself proof the job reached 'building' —
        # only that branch pours it — so two samples straddling a short
        # building phase cannot read as a transition that never
        # happened.
        sample()
        return (obs["building_at"] is not None
                or obs["progress_at"] is not None)

    def saw_progress() -> bool:
        sample()
        return obs["progress_at"] is not None

    reached = poll_until(port, CLAIM_TO_BUILDING_S, saw_transition)
    # #1758's ordering: sample the classifying state HERE, on expiry,
    # before the second wait sends anything further at the engine.
    miss_bundle = (None if reached else construct_timeout_bundle(
        port, watched, uid, state=obs["ai"]))
    if reached and obs["progress_at"] is None:
        if not poll_until(port, BUILDING_TO_PROGRESS_S, saw_progress):
            miss_bundle = construct_timeout_bundle(
                port, watched, uid, state=obs["ai"])
    # Name the segment from the BUNDLE's AI family, never from the raw
    # sample: the bundle gates it on the worker still existing, and a
    # worker that died mid-wait must not be reported as standing in a
    # phase (see construct_timeout_bundle). Every branch below that
    # names a segment runs only on a miss, and a miss always has a
    # bundle.
    miss_state = miss_bundle["aiState"] if miss_bundle else None

    if claimed is None:
        _latency_line("claim → 'building'", None,
                      "no 'claimed' record was ever latched")
        _latency_line("'building' → first progress", None,
                      "no 'claimed' record was ever latched")
    else:
        if obs["building_at"] is not None:
            _latency_line("claim → 'building'",
                          obs["building_at"] - claimed_at,
                          f"budget {CLAIM_TO_BUILDING_S:.0f} s")
        elif obs["progress_at"] is not None:
            _latency_line(
                "claim → 'building'", None,
                "the phase was never sampled; progress first seen "
                f"{obs['progress_at'] - claimed_at:.2f} s after the claim")
        else:
            _latency_line(
                "claim → 'building'", None,
                f"EXPIRED after {CLAIM_TO_BUILDING_S:.0f} s; worker in "
                f"{construct_segment(miss_state)!r}")
        if obs["building_at"] is not None and obs["progress_at"] is not None:
            _latency_line("'building' → first progress",
                          max(0.0, obs["progress_at"] - obs["building_at"]),
                          f"budget {BUILDING_TO_PROGRESS_S:.0f} s")
        elif obs["building_at"] is None:
            _latency_line("'building' → first progress", None,
                          "the 'building' phase was never sampled")
        else:
            _latency_line(
                "'building' → first progress", None,
                f"EXPIRED after {BUILDING_TO_PROGRESS_S:.0f} s; worker in "
                f"{construct_segment(miss_state)!r}")

    # The causal rule (#2172 review): an expiry in fetch/walking/no-job
    # is a phase-transition miss and says so; only a miss observed AFTER
    # 'building' is a failure to accrue progress.
    if obs["progress_at"] is not None:
        progress_label, progressed = PROGRESS_LABEL, True
    elif obs["building_at"] is None:
        progressed = False
        progress_label = (
            f"{PROGRESS_LABEL} — PHASE-TRANSITION MISS, not a failure to "
            f"accrue: the construct_job never reached 'building' within "
            f"{CLAIM_TO_BUILDING_S:.0f} s (worker in "
            f"{construct_segment(miss_state)!r})")
    else:
        progressed = False
        progress_label = (f"{PROGRESS_LABEL} — none within "
                          f"{BUILDING_TO_PROGRESS_S:.0f} s of 'building'")
    check(progress_label, progressed)
    emit_timeout_bundle(miss_bundle)

    # The mining-style corner-progress display: once ≥ 2 corners have
    # drained (past ~40% of the job) the tile's slope mask goes
    # non-zero, exactly like a mid-dig tile.
    sloped = poll_until(port, 30, lambda: any(
        send(port, f"return world.getSlopeAt({x}, 8)") not in ("0", "nil")
        for x in (8, 9)))
    slope_bundle = (None if sloped
                    else construct_timeout_bundle(port, PHASE1_FLOORS, uid))
    check("corner-progress display visible mid-build (slope mask set)",
          sloped is not None)
    emit_timeout_bundle(slope_bundle)

    done = poll_until(port, 60, lambda: (
        send(port, "return structure.hasAt(8, 8, 'floor')") == "true"
        and send(port, "return structure.hasAt(9, 8, 'floor')") == "true"
        and designation_at(port, 8, 8) is None
        and designation_at(port, 9, 8) is None))
    done_bundle = (None if done
                   else construct_timeout_bundle(port, PHASE1_FLOORS, uid))
    check("both floors placed and designations cleared", done is not None)
    emit_timeout_bundle(done_bundle)
    check("tiles returned to flat after completion",
          send(port, "return world.getSlopeAt(8, 8)") == "0"
          and send(port, "return world.getSlopeAt(9, 8)") == "0")
    destroy_unit(port, uid)


def phase_ground(port: int) -> None:
    """One wall; the two steel bars are ground items the builder hauls."""
    print("\n[phase 2] structure job from GROUND items (1 wall)")
    w = wid(port)
    send(port, "item.spawnGround('steel_bar', 12.5, 14.5); "
               "item.spawnGround('steel_bar', 13.5, 15.5); return 'ok'")
    send(port, f"construction.designate('{w}', 8, 14, 8, 14, "
               "'structure', 'dungeon_1', 'wall', 'ne'); return 'ok'")
    time.sleep(0.5)
    check("wall tile designated", designation_at(port, 8, 14) is not None)

    uid = spawn_acolyte(port, 10.5, 14.5)
    done = poll_until(port, 90, lambda: (
        send(port, "return structure.hasAt(8, 14, 'wall_ne')") == "true"
        and designation_at(port, 8, 14) is None))
    done_bundle = (None if done
                   else construct_timeout_bundle(port, [(8, 14)], uid))
    check("wall placed from ground-sourced bars", done is not None)
    emit_timeout_bundle(done_bundle)
    check("ground bars were consumed",
          send(port,
               "local n = 0; "
               "for _, g in ipairs(item.listGround() or {}) do "
               "if g.defName == 'steel_bar' then n = n + 1 end end; "
               "return n") == "0")
    destroy_unit(port, uid)


def phase_occupied(port: int) -> None:
    """#805: an already-occupied structure slot must not be re-designated
    (or paid for if it fills in mid-job), while a compatible slot on the
    same tile keeps working normally. #1595: a tile that already carries
    an OUTSTANDING designation refuses a second one outright, whatever
    slot it names, because the designation map is keyed by tile
    coordinate alone and the second insert would erase the first."""
    print("\n[phase 5] occupied structure slots (#805, #1595)")
    w = wid(port)

    # --- #1595: a STILL-EMPTY tile whose first job is merely pending
    # must refuse a second, otherwise-compatible designation. No unit is
    # alive at this point, so the first job stays pending for the whole
    # check; #805's placed-slot filter cannot fire here (nothing is
    # built), which is exactly what isolates the pending-key rule.
    send(port, f"construction.designate('{w}', 8, 33, 8, 33, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    first = designation_at(port, 8, 33)
    check("still-empty tile took its first (floor) designation",
          isinstance(first, dict) and first.get("kind") == "floor")

    send(port, "return debug.drainActionOutcomes()")  # clear the slate
    send(port, f"construction.designate('{w}', 8, 33, 8, 33, "
               "'structure', 'dungeon_1', 'wall', 'ne'); return 'ok'")
    time.sleep(0.5)
    still = designation_at(port, 8, 33)
    check("the pending job survives the second designation intact",
          isinstance(still, dict) and still.get("kind") == "floor"
          and still.get("edge") is None
          and still.get("status") == (first or {}).get("status")
          and still.get("paid") == (first or {}).get("paid"))
    pendOutcomes = send_json(port, "return debug.drainActionOutcomes()")
    if not isinstance(pendOutcomes, list):
        pendOutcomes = []
    pendMatches = [o for o in pendOutcomes
                   if isinstance(o, dict)
                   and o.get("kind") == "construction.designate"
                   and (o.get("where") or {}).get("x") == 8
                   and (o.get("where") or {}).get("y") == 33]
    # Exact accounting, not merely "not accepted": a bare != 'accepted'
    # would also pass for a 'noop' (empty sweep) or a mis-counted
    # 'partial', neither of which is the refusal under test.
    check("the refused designation records rejected 1/0/1",
          len(pendMatches) == 1
          and pendMatches[0].get("outcome") == "rejected"
          and pendMatches[0].get("requested") == 1
          and pendMatches[0].get("applied") == 0
          and pendMatches[0].get("dropped") == 1)

    # Leave nothing behind: a stray pending job here would be claimed by
    # a later phase's acolyte and pollute its utility scan.
    send(port, "construction.cancelDesignation(8, 33); return 'ok'")
    cleared = poll_until(port, 10, lambda: designation_at(port, 8, 33) is None)
    check("pending-refusal fixture cleaned up", cleared is not None)

    # --- a slot that's already built must not accept a re-designation.
    send(port, "require('scripts.structures').floor(8, 30); return 'ok'")
    built = poll_until(port, 10, lambda: send(
        port, "return structure.hasAt(8, 30, 'floor')") == "true")
    check("fixture floor placed directly", built is not None)

    send(port, "return debug.drainActionOutcomes()")  # clear the slate
    send(port, f"construction.designate('{w}', 8, 30, 8, 30, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("re-designating an already-built floor creates no job",
          designation_at(port, 8, 30) is None)
    check("the existing floor is untouched",
          send(port, "return structure.hasAt(8, 30, 'floor')") == "true")
    outcomes = send_json(port, "return debug.drainActionOutcomes()")
    if not isinstance(outcomes, list):
        outcomes = []
    matches = [o for o in outcomes
               if isinstance(o, dict) and o.get("kind") == "construction.designate"
               and (o.get("where") or {}).get("x") == 8
               and (o.get("where") or {}).get("y") == 30]
    check("occupied-slot designation reports a non-accepted outcome",
          bool(matches) and all(o.get("outcome") != "accepted" for o in matches))

    # --- the ACTUAL UI path (build_tool.lua), not just the lower-level
    # construction.designate call above, must not claim "accepted" for a
    # fully-occupied commit either (review round 1).
    send(port, "camera.setPosition(0, 0); return 'ok'")
    send(port, "local bt = require('scripts.build_tool'); "
               f"bt.hud = {{ worldId = '{w}' }}; return 'ok'")
    px, py = 960, 540
    ux, uy = pick_tile(port, px, py)
    send(port, f"require('scripts.structures').floor({ux}, {uy}); return 'ok'")
    uiBuilt = poll_until(port, 10, lambda: send(
        port, f"return structure.hasAt({ux}, {uy}, 'floor')") == "true")
    check("UI-path fixture floor placed", uiBuilt is not None)
    send(port, "local bt = require('scripts.build_tool'); "
               "bt.enterPlacement({kind='structure', pack='dungeon_1', "
               "piece='floor', edge=nil, displayName='Floor'}); return 'ok'")
    send(port, "return debug.drainActionOutcomes()")  # clear the slate
    send(port, f"local bt = require('scripts.build_tool'); "
               f"return bt.handleMouseDown(1, {px}, {py})")   # anchor
    send(port, f"local bt = require('scripts.build_tool'); "
               f"return bt.handleMouseDown(1, {px}, {py})")   # commit (same tile)
    time.sleep(0.5)
    check("UI commit over an occupied floor creates no job",
          designation_at(port, ux, uy) is None)
    uiOutcomes = send_json(port, "return debug.drainActionOutcomes()")
    if not isinstance(uiOutcomes, list):
        uiOutcomes = []
    uiMatches = [o for o in uiOutcomes
                 if isinstance(o, dict) and o.get("kind") == "buildTool.commitPlacement"]
    check("UI outcome for the occupied commit is not 'accepted'",
          bool(uiMatches) and all(o.get("outcome") != "accepted" for o in uiMatches))
    send(port, "local bt = require('scripts.build_tool'); "
               "bt.exitPlacement(); return 'ok'")

    # --- coexistence: a DIFFERENT slot on the same tile still designates
    # and builds normally, and never disturbs the existing floor. The
    # floor's own job has COMPLETED and left the map, so #1595's
    # pending-key rule does not apply — placed pieces coexist, jobs
    # queue one at a time.
    send(port, f"construction.designate('{w}', 8, 30, 8, 30, "
               "'structure', 'dungeon_1', 'wall', 'ne'); return 'ok'")
    time.sleep(0.5)
    check("a compatible slot on the same tile still designates",
          designation_at(port, 8, 30) is not None)

    coUid = spawn_acolyte(port, 5.5, 30.5)
    send(port, f"unit.addItem({coUid}, 'steel_bar', 0); "
               f"unit.addItem({coUid}, 'steel_bar', 0); return 'ok'")
    done = poll_until(port, 60, lambda: (
        send(port, "return structure.hasAt(8, 30, 'wall_ne')") == "true"
        and designation_at(port, 8, 30) is None))
    co_bundle = (None if done
                 else construct_timeout_bundle(port, [(8, 30)], coUid))
    check("wall built alongside the pre-existing floor", done is not None)
    emit_timeout_bundle(co_bundle)
    check("floor still present after the coexisting wall build",
          send(port, "return structure.hasAt(8, 30, 'floor')") == "true")
    destroy_unit(port, coUid)

    # --- race: the requested slot fills in AFTER designation but BEFORE
    # the claimant pays materials — must resolve without paying/overwriting.
    send(port, f"construction.designate('{w}', 8, 36, 8, 36, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("race-case tile designated", designation_at(port, 8, 36) is not None)

    racer = spawn_acolyte(port, 1.5, 36.5)
    send(port, f"unit.addItem({racer}, 'steel_plate', 0); return 'ok'")
    claimed = poll_until(port, 20, lambda: (
        (designation_at(port, 8, 36) or {}).get("status") == "claimed"))
    race_bundle = (None if claimed
                   else construct_timeout_bundle(port, [(8, 36)], racer))
    check("racer claimed the tile before the slot filled", claimed is not None)
    emit_timeout_bundle(race_bundle)

    # Fill the slot out from under the claimant while it's still walking
    # over — simulates a second worker winning the race for the same slot
    # (a re-designation cannot do this since #1595).
    send(port, "return debug.drainActionOutcomes()")  # clear the slate
    send(port, "require('scripts.structures').floor(8, 36); return 'ok'")
    filled = poll_until(port, 10, lambda: send(
        port, "return structure.hasAt(8, 36, 'floor')") == "true")
    check("competing floor landed mid-job", filled is not None)

    resolved = poll_until(port, 60, lambda: designation_at(port, 8, 36) is None)
    resolve_bundle = (None if resolved
                      else construct_timeout_bundle(port, [(8, 36)], racer))
    check("raced designation resolves (cancelled), no stuck job",
          resolved is not None)
    emit_timeout_bundle(resolve_bundle)
    check("claimant never paid its material",
          send(port, f"local n = 0; for _, it in ipairs(unit.getInventory({racer}) "
                     "or {}) do if it.defName == 'steel_plate' then n = n + 1 end "
                     "end; return n") == "1")
    raceOutcomes = send_json(port, "return debug.drainActionOutcomes()")
    if not isinstance(raceOutcomes, list):
        raceOutcomes = []
    # #1844: the world-side invalidator now cancels the job the instant
    # its slot is filled, rather than waiting for a worker to arrive and
    # notice — so the observable record can come from EITHER path. Both
    # are player-visible outcomes at this tile; which one wins is a
    # timing detail, and demanding a particular one would pin the race
    # rather than the observability this check is about.
    raceMatches = [o for o in raceOutcomes
                   if isinstance(o, dict)
                   and o.get("kind") in ("construction.designate",
                                         "construction.invalidate")
                   and (o.get("where") or {}).get("x") == 8
                   and (o.get("where") or {}).get("y") == 36]
    check("mid-job cancellation reports an observable non-accepted outcome",
          bool(raceMatches) and all(o.get("outcome") != "accepted" for o in raceMatches))
    destroy_unit(port, racer)


def phase_stake(port: int) -> None:
    """A building designation gets staked into a real Constructing building."""
    print("\n[phase 3] building blueprint STAKED (acolyte_portal)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 20, 8, 20, 8, "
               "'building', 'acolyte_portal'); return 'ok'")
    time.sleep(0.5)
    check("building tile designated", designation_at(port, 20, 8) is not None)

    uid = spawn_acolyte(port, 16.5, 8.5)
    # Booleans come back unquoted from the console; strings are
    # JSON-quoted — so return a boolean.
    staked = poll_until(port, 45, lambda: (
        designation_at(port, 20, 8) is None
        and send(port,
                 "local ok = false; "
                 "for _, b in ipairs(building.getActiveIds() or {}) do "
                 "local i = building.getInfo(b); "
                 "if i and i.defName == 'acolyte_portal' then ok = true end "
                 "end; return ok") == "true"))
    stake_bundle = (None if staked
                    else construct_timeout_bundle(port, [(20, 8)], uid))
    check("blueprint became a real building and cleared", staked is not None)
    emit_timeout_bundle(stake_bundle)
    destroy_unit(port, uid)


def phase_release(port: int) -> None:
    """Kill the claimant mid-job; the claim must release and another
    acolyte must finish the tile."""
    print("\n[phase 4] dead claimant RELEASES the job (floor)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, 22, 8, 22, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)

    a = spawn_acolyte(port, 3.5, 22.5)   # a few tiles out: claim precedes arrival
    send(port, f"unit.addItem({a}, 'steel_plate', 0); return 'ok'")
    claimed = poll_until(port, 20, lambda: (
        (designation_at(port, 8, 22) or {}).get("status") == "claimed"))
    # #2172: this wait's own ad-hoc debug print used to run AFTER the
    # check; the standard bundle captures the same state (plus the whole
    # designation record and the worker's position) on expiry, before
    # anything else is sent.
    claim_bundle = (None if claimed
                    else construct_timeout_bundle(port, [(8, 22)], a))
    check("first acolyte claimed the job", claimed is not None)
    emit_timeout_bundle(claim_bundle)

    destroy_unit(port, a)
    # A materials-less scout triggers the sweep (any scanning acolyte
    # releases a dead claimant's job) but can't take the job itself, so
    # the released "pending" state is observable.
    scout = spawn_acolyte(port, 12.5, 22.5)
    released = poll_until(port, 30, lambda: (
        (designation_at(port, 8, 22) or {}).get("status") == "pending"))
    release_bundle = (None if released
                      else construct_timeout_bundle(port, [(8, 22)], scout))
    check("claim released back to 'pending' after claimant death",
          released is not None)
    emit_timeout_bundle(release_bundle)

    send(port, f"unit.addItem({scout}, 'steel_plate', 0); return 'ok'")
    done = poll_until(port, 60, lambda: (
        send(port, "return structure.hasAt(8, 22, 'floor')") == "true"
        and designation_at(port, 8, 22) is None))
    done_bundle = (None if done
                   else construct_timeout_bundle(port, [(8, 22)], scout))
    check("second acolyte finished the released job", done is not None)
    emit_timeout_bundle(done_bundle)
    destroy_unit(port, scout)


def phase_paid_death(port: int) -> None:
    """#799: a claimant that dies AFTER paying leaves the durable 'paid'
    marker on the designation — a materials-less replacement must still
    finish the job, proving it's never charged the cost a second time.

    (phase_release above already covers a claimant dying BEFORE payment;
    once #799's fix makes a paid job claimable by a materials-less
    worker too, a normal nearby scout would sweep AND re-claim the job
    in close succession, too transient to poll for reliably — so the
    release is instead observed via a scout placed OUTSIDE
    construct_scan_range (30 tiles) but still INSIDE construct_scan_chunks
    (getPendingJobs' wider chunk-radius query, ~2 chunks/32 tiles): its
    scan triggers the sweep (releasing the stale claim) but it can never
    be picked as the claim's own 'best' candidate, since that requires
    distance <= construct_scan_range. This holds 'pending' stationary
    long enough to observe before a real, nearby replacement claims it.)"""
    print("\n[phase 6] paid-then-dead claimant: no second payment (floor)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, -8, 8, -8, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("tile designated", designation_at(port, 8, -8) is not None)

    a = spawn_acolyte(port, 5.5, -7.5)
    send(port, f"unit.addItem({a}, 'steel_plate', 0); return 'ok'")
    paid = poll_until(port, 90, lambda: (
        (designation_at(port, 8, -8) or {}).get("paid") is True
        and (designation_at(port, 8, -8) or {}).get("progress", 0) > 0))
    paid_bundle = (None if paid
                   else construct_timeout_bundle(port, [(8, -8)], a))
    check("materials paid and progress accrued before death", paid is not None)
    emit_timeout_bundle(paid_bundle)
    check("payer's inventory shows the material spent",
          send(port, f"local n=0; for _,it in ipairs(unit.getInventory({a}) "
                     "or {}) do if it.defName=='steel_plate' then "
                     "n=n+1 end end; return n") == "0")

    destroy_unit(port, a)

    # Far scout: 32 tiles out (> construct_scan_range=30, so it can never
    # claim) but still inside construct_scan_chunks' getPendingJobs query,
    # so its scan sweeps the dead claim back to "pending" without racing
    # to re-claim it itself.
    farScout = spawn_acolyte(port, 40.5, -7.5)
    released = poll_until(port, 60, lambda: (
        (designation_at(port, 8, -8) or {}).get("status") == "pending"))
    release_bundle = (None if released
                      else construct_timeout_bundle(port, [(8, -8)], farScout))
    check("claim released back to 'pending' after claimant death",
          released is not None)
    emit_timeout_bundle(release_bundle)
    check("payment marker survives the claimant's death",
          (designation_at(port, 8, -8) or {}).get("paid") is True)
    destroy_unit(port, farScout)

    # Replacement carries ZERO materials, with no ground/mule stock
    # anywhere nearby — it must still claim and finish the job.
    b = spawn_acolyte(port, 12.5, -7.5)
    done = poll_until(port, 120, lambda: (
        send(port, "return structure.hasAt(8, -8, 'floor')") == "true"
        and designation_at(port, 8, -8) is None))
    done_bundle = (None if done
                   else construct_timeout_bundle(port, [(8, -8)], b))
    check("materials-less replacement finished the already-paid job "
          "after the original claimant died", done is not None)
    emit_timeout_bundle(done_bundle)
    destroy_unit(port, b)


def phase_preemption(port: int) -> None:
    """#799: an ordinary mid-build interruption (constructOnExit resets
    phase back to 'walking' for thirst / combat / a player order) must
    not re-attempt payment. The claimant carries EXACTLY the required
    material with no ground/mule fallback, so a duplicate charge attempt
    would stall the job outright rather than silently succeed."""
    print("\n[phase 7] preempted mid-build: no duplicate consumption (floor)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, -16, 8, -16, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("tile designated", designation_at(port, 8, -16) is not None)

    uid = spawn_acolyte(port, 5.5, -15.5)
    send(port, f"unit.addItem({uid}, 'steel_plate', 0); return 'ok'")
    building = poll_until(port, 90, lambda: (
        (designation_at(port, 8, -16) or {}).get("progress", 0) > 0))
    building_bundle = (None if building
                       else construct_timeout_bundle(port, [(8, -16)], uid))
    check("payment made and progress underway", building is not None)
    emit_timeout_bundle(building_bundle)
    check("exactly one payment taken (inventory now empty)",
          send(port, f"local n=0; for _,it in ipairs(unit.getInventory({uid}) "
                     "or {}) do if it.defName=='steel_plate' then "
                     "n=n+1 end end; return n") == "0")

    # Simulate the exact reset constructOnExit performs on preemption,
    # without needing to engineer a real interrupt.
    send(port, f"local s = require('scripts.unit_ai').getState({uid}); "
               f"s.constructJob.phase = 'walking'; return s.constructJob.phase")

    done = poll_until(port, 120, lambda: (
        send(port, "return structure.hasAt(8, -16, 'floor')") == "true"
        and designation_at(port, 8, -16) is None))
    done_bundle = (None if done
                   else construct_timeout_bundle(port, [(8, -16)], uid))
    check("job completed after preemption with no re-payment attempt "
          "(a duplicate charge would stall: no spare material, no "
          "ground/mule stock)", done is not None)
    emit_timeout_bundle(done_bundle)
    destroy_unit(port, uid)


def phase_save_load(port: int) -> None:
    """#799: the durable payment marker (and progress) must survive a
    save/load round-trip, even when the paying unit does not."""
    print("\n[phase 8] paid designation survives save/load (floor)")
    w = wid(port)
    send(port, f"construction.designate('{w}', 8, -24, 8, -24, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("tile designated", designation_at(port, 8, -24) is not None)

    a = spawn_acolyte(port, 5.5, -23.5)
    send(port, f"unit.addItem({a}, 'steel_plate', 0); return 'ok'")
    paid = poll_until(port, 90, lambda: (
        (designation_at(port, 8, -24) or {}).get("paid") is True
        and (designation_at(port, 8, -24) or {}).get("progress", 0) > 0))
    paid_bundle = (None if paid
                   else construct_timeout_bundle(port, [(8, -24)], a))
    check("materials paid and progress accrued before save", paid is not None)
    emit_timeout_bundle(paid_bundle)
    destroy_unit(port, a)   # the paying unit does NOT survive to save

    send(port, f"engine.saveWorld('{w}', 'construct_payment_check'); "
               "return 'ok'")
    time.sleep(5.0)
    send(port, "engine.loadSave('construct_payment_check'); return 'ok'")
    # Issue #763: engine.loadSave only ACCEPTS synchronously -- the saved
    # page (its own id `w` verbatim -- no more main_world remap) doesn't
    # exist live until the transaction actually publishes.
    published, load_status = wait_load_published(port, 60)
    check(f"load transaction published ({load_status})", published)
    send(port, f"world.show('{w}'); return 'ok'")
    send(port, "engine.setPaused(false); return 'ok'")
    send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)", timeout=45)
    send(port, "return world.waitForChunks(90)", timeout=95)

    # The chunk/designation refs can take a beat to settle right after a
    # load on a loaded/slow machine — poll rather than a single read.
    def paid_reload():
        d = designation_at(port, 8, -24)
        return d if isinstance(d, dict) and d.get("paid") is True else None
    reloaded = poll_until(port, 60, paid_reload)
    check("designation survives load with paid=true and progress intact",
          isinstance(reloaded, dict) and reloaded.get("paid") is True
          and reloaded.get("progress", 0) > 0)

    b = spawn_acolyte(port, 12.5, -23.5)   # zero materials
    done = poll_until(port, 120, lambda: (
        send(port, "return structure.hasAt(8, -24, 'floor')") == "true"
        and designation_at(port, 8, -24) is None))
    done_bundle = (None if done
                   else construct_timeout_bundle(port, [(8, -24)], b))
    check("materials-less unit finished the reloaded paid job", done is not None)
    emit_timeout_bundle(done_bundle)
    destroy_unit(port, b)


def phase_cancel_refund(port: int) -> None:
    """#799 no-silent-loss policy: the build-tool's right-click cancel
    refunds an already-PAID structure's materials to the ground and
    never fabricates materials for an unpaid one; a placement failure
    (a post's floor pulled out from under it mid-job) refunds too,
    alongside a player-observable notification, instead of completing
    with the paid materials simply gone."""
    print("\n[phase 9] cancellation / placement-failure material policy")
    w = wid(port)
    send(port, "camera.setPosition(0, 0); return 'ok'")
    send(port, "local bt = require('scripts.build_tool'); "
               f"bt.hud = {{ worldId = '{w}' }}; return 'ok'")

    def ui_right_click(px: int, py: int) -> None:
        send(port, "local bt = require('scripts.build_tool'); "
                   "bt.enterPlacement({kind='structure', pack='dungeon_1', "
                   "piece='floor', edge=nil, displayName='Floor'}); "
                   "return bt.state.mode")
        send(port, f"local bt = require('scripts.build_tool'); "
                   f"return bt.handleMouseDown(2, {px}, {py})")

    # --- (a) an UNPAID designation cancelled via the UI creates no
    # ground item — the refund gate must not fire for free.
    px1, py1 = 200, 500
    ux1, uy1 = pick_tile(port, px1, py1)
    send(port, f"construction.designate('{w}', {ux1}, {uy1}, {ux1}, {uy1}, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    check("unpaid fixture tile designated", designation_at(port, ux1, uy1) is not None)
    before1 = ground_count(port, "steel_plate")
    ui_right_click(px1, py1)
    time.sleep(0.5)
    check("UI cancel of an unpaid designation creates no ground item",
          designation_at(port, ux1, uy1) is None
          and ground_count(port, "steel_plate") == before1)

    # --- (b) a PAID designation cancelled via the UI refunds its
    # material to the ground instead of deleting it. Acts on the FIRST
    # tick 'paid' is observed, before further progress could complete
    # the job out from under the cancel.
    px2, py2 = 500, 800
    ux2, uy2 = pick_tile(port, px2, py2)
    send(port, f"construction.designate('{w}', {ux2}, {uy2}, {ux2}, {uy2}, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    # NB: construct job pacing runs off engine.gameTime() (paused/real-
    # time, World.Engine.Core.Init's gameTimeRef), NOT world.setTimeScale
    # (a PER-PAGE calendar rate flora/power read) — so there's no lever
    # to slow this down; poll_fast's tight 0.05s cadence is what gives
    # the check a real shot at catching 'paid' before the job completes.
    payer = spawn_acolyte(port, ux2 - 3 + 0.5, uy2 + 0.5)
    send(port, f"unit.addItem({payer}, 'steel_plate', 0); return 'ok'")
    paid2 = poll_fast(90, lambda: (
        (designation_at(port, ux2, uy2) or {}).get("paid") is True))
    paid2_bundle = (None if paid2 else construct_timeout_bundle(
        port, [(ux2, uy2)], payer))
    check("second fixture tile paid before cancel", paid2 is not None)
    emit_timeout_bundle(paid2_bundle)
    before2 = ground_count(port, "steel_plate")
    ui_right_click(px2, py2)
    time.sleep(0.5)
    check("UI cancel of a PAID designation refunds the material to the "
          "ground (not placed, not deleted)",
          designation_at(port, ux2, uy2) is None
          and send(port, f"return structure.hasAt({ux2}, {uy2}, 'floor')") == "false"
          and ground_count(port, "steel_plate") == before2 + 1)
    destroy_unit(port, payer)

    # --- (c) placement failure: a post's floor vanishes mid-job. The
    # already-paid wood_log is refunded, the post never lands, and the
    # existing "site changed" warning is player-observable in the event
    # log (#799 adds the refund; the warning itself predates it).
    px3, py3 = 1400, 500
    ux3, uy3 = pick_tile(port, px3, py3)
    send(port, f"require('scripts.structures').floor({ux3}, {uy3}); return 'ok'")
    built = poll_until(port, 20, lambda: send(
        port, f"return structure.hasAt({ux3}, {uy3}, 'floor')") == "true")
    check("placement-failure fixture floor placed", built is not None)
    send(port, f"construction.designate('{w}', {ux3}, {uy3}, {ux3}, {uy3}, "
               "'structure', 'dungeon_1', 'post'); return 'ok'")
    time.sleep(0.5)
    check("post job designated (floor present)",
          designation_at(port, ux3, uy3) is not None)

    # A post's build_work (2.0) is lower than a floor's (3.0), so it has
    # LESS real-time margin between "paid" and "placed" than sub-case (b)
    # above; poll_fast's tight cadence is the only lever here (see the
    # engine.gameTime()/setTimeScale note in sub-case (b)).
    poster = spawn_acolyte(port, ux3 - 3 + 0.5, uy3 + 0.5)
    send(port, f"unit.addItem({poster}, 'wood_log', 0); return 'ok'")
    paid3 = poll_fast(90, lambda: (
        (designation_at(port, ux3, uy3) or {}).get("paid") is True))
    paid3_bundle = (None if paid3 else construct_timeout_bundle(
        port, [(ux3, uy3)], poster))
    check("post job paid before its floor is pulled", paid3 is not None)
    emit_timeout_bundle(paid3_bundle)

    beforeLogs = ground_count(port, "wood_log")
    send(port, f"structure.clear({ux3}, {uy3}, 'floor'); return 'ok'")
    done3 = poll_until(port, 90, lambda: designation_at(port, ux3, uy3) is None)
    done3_bundle = (None if done3 else construct_timeout_bundle(
        port, [(ux3, uy3)], poster))
    check("job resolved (removed) after the placement failure", done3 is not None)
    emit_timeout_bundle(done3_bundle)
    check("post was never actually placed",
          send(port, f"return structure.hasAt({ux3}, {uy3}, 'post_n')") == "false")
    check("its material was refunded to the ground, not lost",
          ground_count(port, "wood_log") == beforeLogs + 1)
    logs = send_json(port, "return engine.getEventLog()")
    notified = any(isinstance(e, dict)
                   and "returned to the ground" in (e.get("text") or "")
                   for e in (logs or []))
    check("a player-observable failure notification was logged", notified)
    destroy_unit(port, poster)

    # --- (d) a claimant mid-build on a PAID job must not still place the
    # structure after the designation is cancelled out from under it
    # (#799 review round 5): abandonClaim interrupts the claimant's own
    # cached job immediately rather than waiting for its next decision
    # tick to notice the designation is gone.
    px4, py4 = 700, 180
    ux4, uy4 = pick_tile(port, px4, py4)
    send(port, f"construction.designate('{w}', {ux4}, {uy4}, {ux4}, {uy4}, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    builder = spawn_acolyte(port, ux4 - 3 + 0.5, uy4 + 0.5)
    send(port, f"unit.addItem({builder}, 'steel_plate', 0); return 'ok'")

    def mid_build():
        d = designation_at(port, ux4, uy4)
        if not isinstance(d, dict):
            return None
        p = d.get("progress", 0)
        return d if d.get("paid") is True and 0 < p < 0.9 else None
    caught = poll_fast(90, mid_build)
    caught_bundle = (None if caught else construct_timeout_bundle(
        port, [(ux4, uy4)], builder))
    check("caught the job mid-build (paid, partial progress)", caught is not None)
    emit_timeout_bundle(caught_bundle)
    beforePlate = ground_count(port, "steel_plate")
    ui_right_click(px4, py4)
    time.sleep(2.0)   # comfortably longer than the job would take to finish
    check("mid-build cancel refunds the material",
          ground_count(port, "steel_plate") == beforePlate + 1)
    check("structure never placed after a mid-build cancel",
          send(port, f"return structure.hasAt({ux4}, {uy4}, 'floor')") == "false")
    destroy_unit(port, builder)

    # --- (e) non-post placement failure (#799 review round 5 nit):
    # structures.floor/ceiling/wall and wire.place all now propagate
    # structure.place's own success/failure — previously only post's
    # floorZAt-gated failure was ever detected. Tiles far outside the
    # flat arena's 5x5 chunk footprint are never loaded, so placement
    # there deterministically fails without needing a real mid-job race.
    check("structures.floor returns false for an unloaded-chunk target",
          send(port, "return require('scripts.structures').floor(500, 500)") == "false")
    check("structures.ceiling returns false for an unloaded-chunk target",
          send(port, "return require('scripts.structures').ceiling(501, 500)") == "false")
    check("structures.wall returns false for an unloaded-chunk target",
          send(port, "return require('scripts.structures').wall(502, 500, 'ne')") == "false")
    check("wire.place returns false for an unloaded-chunk target",
          send(port, "return require('scripts.wire').place(503, 500)") == "false")

    # --- (f) the exact race a review round found: a payment
    # immediately followed by cancelDesignationForRefund must observe it,
    # not a stale queued write that hasn't landed yet. Payment is a
    # SYNCHRONOUS direct write, so this can never race the atomic cancel
    # pop.
    #
    # #1844 replaced the boolean setMaterialsPaid with
    # construction.payMaterials, which charges ONE exact attempt out of
    # ONE unit's inventory and records the durable RECEIPT in the same
    # step. So this now proves more than it did: the popped job reports
    # not just THAT it was paid but exactly WHAT was removed, and the
    # payer's inventory really lost it.
    px6, py6 = 1600, 700
    ux6, uy6 = pick_tile(port, px6, py6)
    payer = spawn_acolyte(port, ux6 + 2, uy6)
    send(port, f"unit.addItem({payer}, 'steel_plate'); return 'ok'")
    send(port, f"construction.designate('{w}', {ux6}, {uy6}, {ux6}, {uy6}, "
               "'structure', 'dungeon_1', 'floor'); return 'ok'")
    time.sleep(0.5)
    job6 = send_json(port,
        f"return construction.getDesignationAt('{w}', {ux6}, {uy6})")
    check("the designation carries an attempt identity",
          isinstance(job6, dict) and isinstance(job6.get("attempt"), (int, float)))
    attempt6 = job6.get("attempt") if isinstance(job6, dict) else 0
    paid6 = send(port, f"return tostring(construction.payMaterials('{w}', "
                       f"{ux6}, {uy6}, {attempt6}, {payer}))")
    check("payMaterials charges the exact attempt", paid6.strip('"') == "true")
    check("a second payment for the same attempt removes nothing",
          send(port, f"return tostring(construction.payMaterials('{w}', "
                     f"{ux6}, {uy6}, {attempt6}, {payer}))").strip('"')
          == "false")
    popped = send_json(port,
        f"return construction.cancelDesignationForRefund('{w}', {ux6}, {uy6})")
    check("a payment immediately followed by a cancel pop "
          "observes paid=true, not a stale queued write",
          isinstance(popped, dict) and popped.get("paid") is True)
    receipt = popped.get("receipt") if isinstance(popped, dict) else None
    check("the popped job carries the exact receipt that was charged",
          isinstance(receipt, list) and len(receipt) == 1
          and receipt[0].get("name") == "steel_plate"
          and receipt[0].get("count") == 1)
    destroy_unit(port, payer)

    send(port, "local bt = require('scripts.build_tool'); "
               "bt.exitPlacement(); return 'ok'")


# Order matters: the stake phase runs LAST — the staked portal spawns
# its starting roster (building_spawn.lua auto-loads at boot), and six
# fresh acolytes + a stocked technomule would contaminate any phase
# that runs after it (they claim jobs and source from the mule).
PHASES = {
    "inventory": phase_inventory,
    "ground": phase_ground,
    "release": phase_release,
    "paid_death": phase_paid_death,
    "preemption": phase_preemption,
    "save_load": phase_save_load,
    "cancel": phase_cancel_refund,
    "occupied": phase_occupied,
    "stake": phase_stake,
}


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9377)
    ap.add_argument("--phase", default="all", choices=["all"] + list(PHASES))
    args = ap.parse_args()

    tmpdir = tempfile.mkdtemp(prefix="construction_probe_")
    try:
        root = make_isolated_root(tmpdir)
        proc = boot(args.port, log=LOG, args=["--resource-root", root])
        return _run(args, proc)
    finally:
        shutil.rmtree(tmpdir, ignore_errors=True)


def _run(args, proc) -> int:
    try:
        bootstrap(args.port)
        if not wid(args.port):
            print("FAIL: no active world after arena build", file=sys.stderr)
            return 2
        todo = PHASES.values() if args.phase == "all" else [PHASES[args.phase]]
        for phase in todo:
            phase(args.port)
    finally:
        quit_engine(args.port, proc)
        try:
            proc.wait(timeout=10)
        except subprocess.TimeoutExpired:
            proc.kill()

    failed = [label for label, ok in CHECKS if not ok]
    print(f"\n{len(CHECKS) - len(failed)}/{len(CHECKS)} checks passed"
          + (f"; FAILED: {failed}" if failed else ""))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
