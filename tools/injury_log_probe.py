#!/usr/bin/env python3
"""Headless injury-log backend probe.

The injury/combat/event LOG PANELS are GUI and can't be verified headless,
but the data path that feeds them is pure engine plumbing and IS testable.
This guards that plumbing against silent breakage:

  1. injury.emit / injury.drainEvents roundtrip (+ drain clears the buffer).
  2. unit.injure on a live spawned unit emits an "injure" injury event
     attributed to THAT unit, carrying back the part/woundKind/severity
     it was called with.
  3. engine.emitEventForUnit tags the event with a uid that getEventLog
     surfaces (the per-unit log panel filters on this).
  4. A real fall emits a "fall" injury event whose target is the unit that
     fell and whose payload carries a well-formed detail / count / severity
     (closes the Fall.hs gap). Gating, exactly like phases 1-3.

This probe must be the injury stream's ONLY consumer, and it is not enough
not to load the panel: scripts/init_loader.lua loads
scripts/injury_log_panel.lua unconditionally at boot — headless included —
and its 0.1s tick calls injury.drainEvents(). So `bootstrap` neutralises
that tick (the same stub it applies to unit_ai's wander). Without it a
phase that drains asynchronously, like phase 4's fall, loses the event to
the panel before it can read it; the single-line phases 1-2 only survive
because they emit and drain inside one console command.

Usage:
  python3 tools/injury_log_probe.py [--port 9140] [--no-fall]

Exit 0 = all checks passed.

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps its human-readable
per-check output.
"""
from __future__ import annotations

import argparse
import glob
import math
import socket
import subprocess
import sys
import time
import probe_protocol
from probelib import quit_engine, boot, send, send_json

LOG = "/tmp/injury_log_probe_engine.log"
LOG_NAME = "injury_log_probe_engine.log"
PROBE_KEY = "injury_log"
PROBE_CHECKS = [
    ("emit_roundtrip", "emit then drain returns the event"),
    ("drain_destructive", "second drain is empty"),
    ("injure_event", "unit.injure -> 'injure' event for the injured unit"),
    ("event_log_uid", "getEventLog().uid carries the unit id"),
    ("fall_lane_damaging", "the lane drops at least 2 z (guaranteed damaging)"),
    ("fall_event", "fall landing -> 'fall' event for the falling unit"),
]
DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, PROBE_CHECKS)
CHECK_ID_BY_LABEL = {label: check_id for check_id, label in PROBE_CHECKS}
_REPORTER: probe_protocol.Reporter | None = None

# Phase 4's polling window: the fall_edge walk is ~7 tiles at the acolyte's
# comfort speed, so 20 s of drains leaves room for a stalled approach to be
# re-commanded and still finish.
FALL_POLLS = 40
FALL_POLL_SLEEP = 0.5

# Phase 4 walks its own lane, this many tiles north of the fall_edge
# course's own row. Phase 2's unit is standing on tile (0, 0) — which the
# plateau then raises under it — and that is the course's LAST plateau
# column on its own row, i.e. exactly the tile the faller has to cross to
# reach the edge. Two units do not share a tile, so on the course row the
# walk stops at the blocker and never goes over. The lane is still on the
# same plateau, which the drop check below verifies rather than assumes.
FALL_LANE_OFFSET = 2

# The minimum drop the fall model guarantees is damaging for every acolyte
# profile (test-headless/Test/Headless/Unit/Fall.hs:205-252): fallInjuries
# treats <= 1 z as a free walk-off and yields a non-empty blunt injury set
# from 2 z up. The event fires on ANY non-empty injury set
# (src/Unit/Thread/Movement.hs:193-203), so this — not a fracture
# threshold — is what the fixture height has to clear.
FALL_MIN_DAMAGING_DROP = 2

# Phase 2's inputs to unit.injure, asserted back out of the drained event
# rather than pattern-matched inside a concatenation. A correctly shaped
# event attributed to a stale id, a sentinel, or another unit is exactly
# what this phase exists to reject, so target/kind/part/woundKind are
# compared as WHOLE values against these.
#
# severity is the exception: the engine renders it with `show` on the
# Float the wound stores (src/Engine/Scripting/Lua/API/Units/Combat.hs:472,
# src/Unit/Types/Wound.hs:30), so the text coming back is not guaranteed
# to be the literal this probe typed. Compare it numerically.
INJURE_PART = "l_thigh"
INJURE_WOUND_KIND = "stab"
INJURE_SEVERITY = 0.4
INJURE_SEVERITY_TOL = 1e-6

# One phase-4 poll: keep the session running, then scan one drained batch
# for the event this phase is about — kind "fall" AND target == the unit we
# pushed off the edge. Unrelated events in the same batch (another unit's
# fall, an "injure" from elsewhere) are counted, never matched: they can
# neither satisfy the assertions nor hide the real event behind an early
# return. Payload values reach Lua as STRINGS (CombatEvent.cePayload is
# HashMap Text Text), so they come back as text and are parsed in Python.
#
# The setPaused(false) is load-bearing, not defensive. `survival_critical`
# is the one notification category shipping pause: true
# (data/notification_categories.yaml), and Engine.PlayerEvent.Emit pauses
# the whole session on any emit of it. Phase 3 emits one directly, and a
# unit dying of its phase-2 wounds emits another at an unpredictable
# moment — either freezes the walk mid-approach. Re-asserting it every
# poll is what makes the fall happen at all. (engine.setNotificationOverrides
# would silence the category instead, but it PERSISTS to
# config/notifications.local.yaml, which a probe must not touch.)
#
# For the same reason the poll re-issues the walk when it finds the unit
# idle and still up on the plateau: a pause landing mid-step can drop the
# order and leave it parked at the edge. Re-commanding only recovers a
# stalled approach — it cannot manufacture the event, which still has to
# come from a real fall with the real payload.
FALL_POLL_LUA = (
    "engine.setPaused(false); "
    "local e=injury.drainEvents(); local other=0; local hit=nil; "
    "for _,ev in ipairs(e) do "
    "if hit==nil and ev.kind=='fall' and ev.target==_FU then "
    "local p=ev.payload or {}; "
    "hit={found='1',kind=tostring(ev.kind),target=tostring(ev.target),"
    "detail=tostring(p.detail or ''),count=tostring(p.count or ''),"
    "severity=tostring(p.severity or '')} "
    "else other=other+1 end end; "
    "if hit==nil then hit={found='0'}; "
    "local i=unit.getInfo(_FU); "
    "if i and unit.getActivity(_FU)=='idle' and (i.gridZ or 0) > _FGZ then "
    "hit.requeued=tostring(unit.moveTo(_FU,_FGX,_FGY,_FSPEED,'allow_falls')) "
    "end end; "
    "hit.other=tostring(other); return hit"
)


def bootstrap(port: int, with_movement: bool) -> None:
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    # init_loader.lua loads injury_log_panel.lua at boot regardless of
    # headlessness, and its tick drains injury.drainEvents(). Silence that
    # tick so this probe owns the stream (verified: without the stub an
    # event emitted and drained a second later is already gone).
    send(port, "local ok,m = pcall(require, 'scripts.injury_log_panel'); "
               "if ok and type(m) == 'table' then m.update = function() end end; "
               "return 'ok'")
    if with_movement:
        # Stat + resource ticks drive movement; unit_ai is auto-loaded and
        # its wander would steer the unit off the cliff edge, so neutralise
        # its tick (movement_probe does the same).
        send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); return 'ok'")
        send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); return 'ok'")
        send(port, "pcall(function() require('scripts.unit_ai').update = "
                   "function() end end); return 'ok'")
    send(port, "require('scripts.movement_arena'); return 'ok'")


def parse_int(text: str | None) -> int | None:
    """Payload values arrive as strings; None when absent/not an integer."""
    try:
        return int(str(text).strip())
    except (TypeError, ValueError):
        return None


def parse_float(text: str | None) -> float | None:
    try:
        return float(str(text).strip())
    except (TypeError, ValueError):
        return None


def parse_uid(text: str | None) -> int | None:
    """A uid the console may render as "12" or "12.0" — never as text.

    None when absent, not a number, or not an exact whole number. The
    float fallback exists only for that trailing ".0": truncating
    anything else would round a target of "1.5" into agreement with a
    spawned uid of 1, which is precisely the mismatch the callers exist
    to report, and int() on nan/inf raises instead of reporting it.
    """
    v = parse_int(text)
    if v is not None:
        return v
    f = parse_float(text)
    if f is None or not math.isfinite(f) or not f.is_integer():
        return None
    return int(f)


def terrain_z(port: int, gx: int, gy: int) -> int | None:
    """Ground z of TILE (gx, gy) — integer tile coords, not the +0.5
    world positions a unit is spawned at (getTerrainAt takes tiles).
    It returns two values (surface, terrain surface); the first is the
    one a walking unit stands on."""
    v = parse_float(send(port,
        f"local a = world.getTerrainAt({gx}, {gy}); return tostring(a)"))
    return None if v is None else int(v)


def check(name: str, ok: bool, detail: str = "") -> bool:
    if _REPORTER is None:
        raise RuntimeError("injury-log reporter is not initialised")
    payload = {"detail": str(detail)} if detail else None
    return _REPORTER.check(CHECK_ID_BY_LABEL[name], bool(ok), name, payload)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9140)
    ap.add_argument("--no-fall", action="store_true",
                    help="skip the movement-driven fall test")
    ap.add_argument("--describe", action="store_true")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args, rep)
    finally:
        rep.close()


def _run(args, rep: probe_protocol.Reporter) -> int:
    global _REPORTER
    _REPORTER = rep
    proc = boot(args.port, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    passed = True
    try:
        bootstrap(args.port, with_movement=not args.no_fall)

        rep.note("1. injury.emit / drainEvents roundtrip")
        r = send(args.port,
            "injury.emit(7,'fall','blood loss','l_shin','fracture',0.7); "
            "local e=injury.drainEvents(); if #e<1 then return 'NONE' end; "
            "return e[1].target..'|'..e[1].kind..'|'..(e[1].payload.cause or '?')"
            "..'|'..(e[1].payload.woundKind or '?')")
        passed &= check("emit then drain returns the event", r == "7|fall|blood loss|fracture", r)
        r2 = send(args.port, "return #injury.drainEvents()")
        passed &= check("second drain is empty", r2 == "0", r2)

        rep.note("2. unit.injure on a live unit emits an injury event")
        send(args.port, "return require('scripts.movement_arena').buildCourse('flat')")
        time.sleep(0.8)
        tu = parse_uid(send(args.port,
                            "_TU = unit.spawn('acolyte', 0, 0); return _TU"))
        time.sleep(0.8)
        # The injure and the drain stay ONE console command. bootstrap has
        # already stubbed injury_log_panel's tick, but a single round trip
        # additionally leaves no window at all for another consumer to take
        # the event between the two calls.
        #
        # Every field is read off the SAME drained event (drainEvents returns
        # oldest-first, src/Engine/Scripting/Lua/API/Combat.hs:146-157, and
        # phase 1 left the buffer empty, so e[1] is the event this call just
        # produced). `target` is the point: pushInjuryEvent records the victim
        # uid as ceTarget (src/Combat/Types.hs:96-103), so comparing it with
        # the spawned uid is what proves unit.injure attributes its event to
        # the unit it wounded — which phase 3's different producer cannot show.
        ev = send_json(args.port,
            f"local ok=unit.injure(_TU,'{INJURE_PART}','{INJURE_WOUND_KIND}',"
            f"{INJURE_SEVERITY}); "
            "local e=injury.drainEvents(); local n=#e; "
            "if n<1 then return {n=tostring(n), injured=tostring(ok)} end; "
            "local v=e[1]; local p=v.payload or {}; "
            "return {n=tostring(n), injured=tostring(ok), "
            "target=tostring(v.target), kind=tostring(v.kind), "
            "part=tostring(p.part or ''), woundKind=tostring(p.woundKind or ''), "
            "severity=tostring(p.severity or '')}")
        if not isinstance(ev, dict):
            passed &= check("unit.injure -> 'injure' event for the injured unit",
                            False, f"malformed drain response {ev!r}")
        else:
            severity = parse_float(ev.get("severity"))
            ok = (tu is not None
                  and parse_uid(ev.get("target")) == tu
                  and ev.get("kind") == "injure"
                  and ev.get("part") == INJURE_PART
                  and ev.get("woundKind") == INJURE_WOUND_KIND
                  and severity is not None
                  and abs(severity - INJURE_SEVERITY) <= INJURE_SEVERITY_TOL)
            passed &= check(
                "unit.injure -> 'injure' event for the injured unit", ok,
                f"target={ev.get('target')} (spawned {tu}) "
                f"kind={ev.get('kind')} part={ev.get('part')} "
                f"woundKind={ev.get('woundKind')} "
                f"severity={ev.get('severity')} "
                f"injured={ev.get('injured')} "
                f"({ev.get('n')} event(s) drained)")

        rep.note("3. emitEventForUnit tags a uid that getEventLog surfaces")
        r = send(args.port,
            "engine.emitEventForUnit('survival_critical','probe',4242); "
            "local l=engine.getEventLog(); if #l<1 then return 'NONE' end; "
            "return tostring(l[#l].uid)")
        passed &= check("getEventLog().uid carries the unit id", r == "4242", r)

        if not args.no_fall:
            # GATING: exercises the real Fall.hs producer by walking a unit
            # off the arena's `fall_edge` plateau — the same 3-z fixture
            # tools/movement_probe.py already gates its fall checks on. The
            # height is chosen from the pure model, not a fracture
            # threshold: the event fires on ANY non-empty injury set
            # (src/Unit/Thread/Movement.hs:193-203), and a 2-z drop already
            # injures every acolyte profile
            # (test-headless/Test/Headless/Unit/Fall.hs:205-252). fallInjuries
            # is RNG-free (src/Unit/Fall.hs:127-128), so once the unit steps
            # off, the injuries — and therefore the event — are determined.
            # `bootstrap` has already silenced injury_log_panel's tick, so
            # this drain is the stream's only consumer.
            rep.note("4. a real fall emits a 'fall' injury event")
            course = send_json(args.port,
                "return require('scripts.movement_arena').buildCourse('fall_edge')")
            if not isinstance(course, dict) or "sx" not in course:
                rep.abort("fall_edge course did not build", {"result": course})
                return 2
            else:
                sx = course["sx"] + 0.5
                gx = course["gx"] + 0.5
                sy = course["sy"] + FALL_LANE_OFFSET + 0.5
                gy = course["gy"] + FALL_LANE_OFFSET + 0.5
                time.sleep(0.5)
                start_z = terrain_z(args.port, course["sx"],
                                    course["sy"] + FALL_LANE_OFFSET)
                goal_z = terrain_z(args.port, course["gx"],
                                   course["gy"] + FALL_LANE_OFFSET)
                drop = (None if start_z is None or goal_z is None
                        else start_z - goal_z)
                passed &= check(
                    f"the lane drops at least {FALL_MIN_DAMAGING_DROP} z "
                    f"(guaranteed damaging)",
                    drop is not None and drop >= FALL_MIN_DAMAGING_DROP,
                    f"tile ({course['sx']},{course['sy'] + FALL_LANE_OFFSET}) "
                    f"z={start_z} -> tile ({course['gx']},"
                    f"{course['gy'] + FALL_LANE_OFFSET}) z={goal_z}, "
                    f"drop={drop}")
                send(args.port, "injury.drainEvents(); return 'cleared'")  # clear prior
                fu_raw = send(args.port,
                              f"_FU = unit.spawn('acolyte', {sx}, {sy}); return _FU")
                # Compare the event's target numerically, never as text.
                fu = parse_uid(fu_raw)
                if fu is None or fu < 0:
                    rep.abort("could not spawn the falling unit",
                              {"result": fu_raw})
                    return 2
                time.sleep(1.5)  # settle onto the plateau before commanding
                # Phase 3's survival_critical emit paused the session (see
                # FALL_POLL_LUA); an ordered move on a paused world never
                # takes a step. "allow_falls" is unit.moveTo's default, but
                # this walk exists to go over an edge, so say so: the
                # ambient "avoid_falls" policy would make the last step
                # impassable (scripts/ambient_movement.lua).
                #
                # Travel at the unit's COMFORT speed, not a fixed fast
                # value: comfort is stamina-neutral, and driving an acolyte
                # above it collapses it from exhaustion short of the edge
                # (observed, repeatedly, at 2.0 tiles/s). movement_probe
                # commands its own courses the same way.
                speed = send(args.port,
                     f"_FGX={gx}; _FGY={gy}; _FGZ={goal_z}; "
                     f"_FSPEED=require('scripts.movement_speed').comfort(_FU); "
                     f"engine.setPaused(false); "
                     f"unit.moveTo(_FU, _FGX, _FGY, _FSPEED, 'allow_falls'); "
                     f"return tostring(_FSPEED)")
                hit, others = None, 0
                for _ in range(FALL_POLLS):
                    time.sleep(FALL_POLL_SLEEP)
                    r = send_json(args.port, FALL_POLL_LUA)
                    if not isinstance(r, dict):
                        continue
                    others += int(r.get("other") or 0)
                    if r.get("found") == "1":
                        hit = r
                        break
                if hit is None:
                    where = send(args.port,
                        "local i = unit.getInfo(_FU); "
                        "if not i then return 'gone' end; "
                        "return tostring(i.gridX)..','..tostring(i.gridY)"
                        "..','..tostring(i.gridZ)..' '"
                        "..tostring(unit.getPose(_FU))..'/'"
                        "..tostring(unit.getActivity(_FU))")
                    passed &= check(
                        "fall landing -> 'fall' event for the falling unit",
                        False,
                        f"no fall event with target={fu} in "
                        f"{FALL_POLLS * FALL_POLL_SLEEP:.0f}s "
                        f"({others} unrelated event(s) drained); "
                        f"commanded at {speed} tiles/s; "
                        f"unit ended at {where}")
                else:
                    detail = hit.get("detail") or ""
                    count = parse_int(hit.get("count"))
                    severity = parse_float(hit.get("severity"))
                    ok = (hit.get("kind") == "fall"
                          and fu is not None
                          and parse_int(hit.get("target")) == fu
                          and bool(detail)
                          and count is not None and count >= 1
                          and severity is not None and severity > 0.0)
                    passed &= check(
                        "fall landing -> 'fall' event for the falling unit",
                        ok,
                        f"target={hit.get('target')} (spawned {fu}) "
                        f"kind={hit.get('kind')} detail='{detail}' "
                        f"count={hit.get('count')} "
                        f"severity={hit.get('severity')} "
                        f"({others} unrelated event(s) drained)")
        else:
            rep.note("4. fall test skipped (--no-fall)")

        rep.note(f"\n  {'PASS' if passed else 'FAIL'}: injury-log backend"
                 + ("" if passed else " — see failures above"))
        return 0 if passed else 1
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
