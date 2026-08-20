#!/usr/bin/env python3
"""Headless demonstration that ambient wander never routes over a damaging
drop (issue #1217).

Four stages on ONE deterministic arena, all on ONE unit, so the run proves
the hazard policy is a property of the REQUEST rather than of the mover:

  A. AMBIENT WANDER. A player acolyte stands on ``wander_ledge``: a 5x5
     3-high plateau with a damaging drop (>= ``fall_trigger_drop``, default
     2) on every side and no ramp anywhere. An acolyte's ``wander_radius``
     (5.0) is twice the plateau's half-extent, so most sampled legs aim off
     the edge. Over a sustained window the unit must stay on top: zero
     falls, z never leaves the plateau, and at least one leg observed
     carrying the protected policy (so the window is a real test, not an
     idle unit).

  B. POLICY RETENTION. A protected move to a REACHABLE spot on the
     plateau, then ``unit.setMoveSpeed`` mid-flight: retargeting the pace
     of a request must not quietly re-permit the fall it refused, so the
     in-flight policy must still read ``avoid_falls`` afterwards.

  C. PROTECTED COMMAND across the edge. With the AI quiet, a
     ``unit.moveTo(..., 'avoid_falls')`` at the low ground east must
     likewise never fall, and the request must TERMINATE (no safe route)
     rather than sit there replanning forever.

  D. ORDINARY COMMAND across the SAME edge, on the SAME unit, immediately
     after C. It must fall — which is what proves the policy is neither
     stochastic (C was not luck) nor sticky (C did not leave the unit
     permanently fall-proof).

Deliberately out of scope, and deliberately neutralised here: the acolyte's
standing ``find_water`` goal. ``search_for_water`` is purposeful movement
and keeps the fall-permitted default by design (#1217's out-of-scope list),
so leaving it live would test the wrong thing.

Usage:
  python3 tools/wander_hazard_probe.py [--port 9231] [--wander-seconds 60]

Exit code 0 = every check passed.
"""
from __future__ import annotations

import argparse
import glob
import sys
import time

from probelib import (boot, quit_engine, send, send_json,
                      clear_find_water, poll_until)

LOG = "/tmp/wander_hazard_probe_engine.log"

#: Plateau top z: arena base 0 + the course's 3-high plateau.
LEDGE_Z = 3

#: Low ground the commanded stages aim at (the course's own goal tile).
GOAL_X, GOAL_Y = 6, 0


def bootstrap(port: int) -> None:
    """Load the defs + the AI stack the loading screen would load in a GUI
    session, plus the arena module. Unlike movement_probe, the AI is left
    LIVE — its ambient wander is exactly what stage A is testing."""
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
    for path, z in (("scripts/unit_stats.lua", 0.1),
                    ("scripts/unit_resources.lua", 0.2),
                    ("scripts/unit_ai.lua", 0.1)):
        send(port, f"engine.loadScript('{path}', {z}); return 'ok'")
    send(port, "require('scripts.movement_arena'); return 'ok'")


def neutralize_ai(port: int) -> None:
    """Replace unit_ai's tick with a no-op for the COMMANDED stages, so the
    ambient wander under test in stage A can't re-issue a move on top of a
    command under test in B/D. It is never restored — nothing after D needs
    the AI, and a restored tick would re-seed the acolyte's spawn goals."""
    send(port,
         "pcall(function() local ai = require('scripts.unit_ai'); "
         "ai.update = function() end end); return 'ai-off'")


def reset_to_ledge(port: int, uid: int) -> None:
    """Park the unit back on the plateau centre with no in-flight request,
    so every commanded stage starts from the identical state (a wander leg
    or a previous stage may have left it at the rim)."""
    send(port, f"unit.stop({uid}); return 'ok'")
    send(port, f"unit.setPos({uid}, 0.5, 0.5, {LEDGE_Z}); return 'ok'")
    time.sleep(0.5)


def terrain_z(port: int, gx: int, gy: int):
    """Terrain surface z at a tile, or None. world.getTerrainAt returns two
    values (surfaceZ, terrainSurfaceZ); we want the terrain one."""
    raw = send(port, f"local a, b = world.getTerrainAt({gx}, {gy}); return b")
    try:
        return int(float(raw))
    except (TypeError, ValueError):
        return None


def sample(port: int, uid: int):
    """One state sample, or None if the unit is gone."""
    lua = (
        f"local i = unit.getInfo({uid}); if not i then return 'DEAD' end; "
        f"return {{x=i.gridX, y=i.gridY, z=i.gridZ, "
        f"hazard=i.moveHazard or '', pose=unit.getPose({uid}), "
        f"act=unit.getActivity({uid})}}"
    )
    s = send_json(port, lua)
    return s if isinstance(s, dict) else None


def watch(port: int, uid: int, seconds: float, interval: float = 0.25):
    """Poll the unit for `seconds`, returning the sample list.

    Defensively unpauses each pass: the user's notifications config can set
    ``unit_warning: pause: true``, and a stuck-walk warning mid-probe would
    otherwise freeze the whole sim."""
    out = []
    deadline = time.time() + seconds
    while time.time() < deadline:
        send(port, "engine.setPaused(false); return 'ok'")
        s = sample(port, uid)
        if s is None:
            break
        out.append(s)
        time.sleep(interval)
    return out


def fell(samples) -> bool:
    """Did the unit fall during this window? EITHER signal counts: the
    Falling pose, or a z below the plateau. Both are checked because a short
    fall can be sampled mid-flight or only after the landing snap, and a
    poll interval can miss the pose entirely."""
    return any(s.get("pose") == "falling" for s in samples) \
        or any(int(s.get("z", LEDGE_Z)) < LEDGE_Z for s in samples)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9231)
    ap.add_argument("--wander-seconds", type=float, default=60.0)
    ap.add_argument("--command-seconds", type=float, default=25.0)
    args = ap.parse_args()

    proc = boot(args.port, log=LOG)
    failures: list[str] = []
    try:
        bootstrap(args.port)
        # buildCourse creates the arena itself (movement_arena.M.create);
        # a separate world.initArena beforehand re-inits it underneath the
        # course's own addTile edits and silently leaves the plateau flat.
        send(args.port,
             "return require('scripts.movement_arena').buildCourse('wander_ledge').name")
        if not poll_until(20, lambda: (send_json(args.port,
                "return world.getChunkInfo(0,0)") or {}).get("loaded")):
            return fail(["arena chunk never loaded"])
        # The course's tile edits are queued, so a loaded chunk is NOT yet a
        # built plateau. Wait for the terrain itself to report the ledge —
        # spawning early grounds the unit on the flat arena floor and the
        # whole run then proves nothing.
        if not poll_until(20, lambda: terrain_z(args.port, 0, 0) == LEDGE_Z):
            return fail([f"plateau never reached z={LEDGE_Z} at (0,0) "
                         f"(saw {terrain_z(args.port, 0, 0)})"])

        # Spawn ON the plateau, as a PLAYER unit (unit.spawn defaults to
        # wildlife), and retire find_water — see the module docstring. No
        # explicit z: the engine's own surface lookup is the authority.
        raw = send(args.port, "return unit.spawn('acolyte', 0.5, 0.5, nil, 'player')")
        uid = int(float(raw))
        if terrain_z(args.port, 0, 0) != LEDGE_Z:
            return fail(["the plateau vanished before the spawn"])
        if not clear_find_water(args.port, uid):
            return fail([f"unit {uid} never got AI state"])
        send(args.port, f"unit.stop({uid}); return 'ok'")

        # ---- Stage A: sustained ambient wander -----------------------
        print(f"[A] watching ambient wander for {args.wander_seconds:.0f}s ...")
        a = watch(args.port, uid, args.wander_seconds)
        if not a:
            failures.append("A: the unit vanished during the wander window")
        else:
            if fell(a):
                failures.append(
                    "A: an ambient wander leg fell off the ledge "
                    f"(min z {min(int(s['z']) for s in a)}, "
                    f"poses {sorted({s['pose'] for s in a})})")
            else:
                print(f"[A] ok: {len(a)} samples, z stayed {LEDGE_Z}, no fall")
            hazards = {s.get("hazard") for s in a if s.get("hazard")}
            if "avoid_falls" not in hazards:
                failures.append(
                    "A: never observed a protected wander leg in flight "
                    f"(hazards seen: {sorted(hazards) or 'none'}) — the window "
                    "proved nothing about routing")
            else:
                print(f"[A] ok: observed protected legs, hazards={sorted(hazards)}")
            # Deliberately NOT asserting that no fall-permitted leg appears:
            # a PURPOSEFUL action winning the window (go_to_sleep walks to a
            # spot at meander pace, for instance) is correct behavior under
            # this policy, and failing on it would gate the probe on which
            # action happened to score highest rather than on routing. The
            # zero-falls check above already covers what matters.

        # ---- Stage B: setMoveSpeed retains the request's policy --------
        neutralize_ai(args.port)
        reset_to_ledge(args.port, uid)
        # A REACHABLE destination on the plateau, crawled at 0.2 t/s so the
        # request is still in flight when the speed is retargeted.
        send(args.port, f"return unit.moveTo({uid}, -1.5, 0.5, 0.2, 'avoid_falls')")
        time.sleep(1.0)
        before = sample(args.port, uid)
        send(args.port, f"return unit.setMoveSpeed({uid}, 0.9)")
        time.sleep(0.5)
        after = sample(args.port, uid)
        if not (before and before.get("hazard") == "avoid_falls"):
            failures.append(
                "B: the reachable protected move was not in flight before "
                f"setMoveSpeed (saw {before})")
        elif not (after and after.get("hazard") == "avoid_falls"):
            failures.append(
                "B: setMoveSpeed dropped the in-flight request's hazard "
                f"policy (saw {after}) — retargeting a pace must not "
                "re-permit a fall the caller refused")
        else:
            print("[B] ok: setMoveSpeed retained the protected policy")

        # ---- Stage C: protected command over the edge -----------------
        reset_to_ledge(args.port, uid)
        send(args.port,
             f"return unit.moveTo({uid}, {GOAL_X + 0.5}, {GOAL_Y + 0.5}, 1.5, 'avoid_falls')")
        print(f"[C] protected command to ({GOAL_X},{GOAL_Y}) ...")
        c = watch(args.port, uid, args.command_seconds)
        if fell(c):
            failures.append("C: a protected command fell off the ledge")
        else:
            print(f"[C] ok: {len(c)} samples, no fall")
        if c and c[-1].get("hazard"):
            failures.append(
                "C: the protected request never terminated — it is still "
                "in flight with no safe route, which is the replan-forever "
                "state #1217 rules out")
        else:
            print("[C] ok: the unreachable protected request terminated")

        # ---- Stage D: ordinary command, same unit, same edge ----------
        reset_to_ledge(args.port, uid)
        send(args.port, f"return unit.moveTo({uid}, {GOAL_X + 0.5}, {GOAL_Y + 0.5}, 1.5)")
        print(f"[D] ordinary command to ({GOAL_X},{GOAL_Y}) ...")
        d = watch(args.port, uid, args.command_seconds)
        if not fell(d):
            failures.append(
                "D: an ORDINARY command over the same edge did not fall — the "
                "policy is sticky (or the arena stopped being a hazard)")
        else:
            print("[D] ok: the ordinary command took the fall")
    finally:
        quit_engine(args.port, proc)

    return fail(failures)


def fail(failures: list[str]) -> int:
    if failures:
        print("\nFAILED:")
        for f in failures:
            print(f"  - {f}")
        return 1
    print("\nAll wander-hazard checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
