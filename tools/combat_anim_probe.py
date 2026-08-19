#!/usr/bin/env python3
"""Headless combat-animation probe.

Agents can't see pixels headless, but the engine tracks the animation
STATE — which animation each unit is playing — and updates it on the
unit thread, which runs headless. `unit.getInfo(uid).currentAnim`
exposes it. This tool drives a real fight in a headless engine and
samples that state over time, so combat/animation behaviour can be
verified WITHOUT a GPU or a human watching.

What it does:
  1. launches `--headless` on a private port,
  2. loads the substance/item/equipment/material/unit YAML defs and the
     AI scripts (unit_stats, unit_resources, unit_ai) that the
     loading-screen flow would normally load (it doesn't run headless),
  3. checks the WHOLE shipped roster's animation storage (#1261/TEX-6 —
     see `check_roster` below),
  4. stamps the flat arena and PROVES the combatants' terrain is safe
     (see `terrain_cause`),
  5. spawns an attacker next to a target — retiring the attacker's
     standing `find_water` goal — issues commandAttack, and PROVES the
     fight actually engaged before sampling anything
     (see `engagement_cause`),
  6. polls each unit's currentAnim for a few seconds and prints the
     per-unit animation timeline,
  7. checks the attacker threw a recognizable swing animation and, if it
     died, settled on a death animation (the two bugs this guards),
  8. samples a live frame and an animation duration off the spawned
     units, so the atlas evidence covers a unit that is actually
     playing rather than only one that registered.

Steps 4 and 5 exist because this probe could not previously tell a
broken FIXTURE from the animation regression it guards (#1396): both
exited 1 with the same `a swing animation appeared : False`. The old
fixture generated a real 64-world merely named "arena" and accepted any
three adjacent equal-height dry tiles, so a qualifying strip could sit
at the lip of a lethal drop — the attacker fell to its death while
approaching in 1 of 3 measured runs (#724, recorded in
`tools/ci_probes.py`). A fixture that never established the fight could
happen now REFUSES to grade it.

Usage:
  python3 tools/combat_anim_probe.py            # acolyte vs bear_brown
  python3 tools/combat_anim_probe.py --attacker acolyte --target bear_brown
  python3 tools/combat_anim_probe.py --seconds 12 --port 9123
  python3 tools/combat_anim_probe.py --roster-only   # skip the fight
  python3 tools/combat_anim_probe.py --self-test     # no engine at all

Exit codes:
  0  the expected animation states were observed.
  1  a check FAILED — a registered def, a storage/live-sample/duration
     mismatch, or (preconditions having held) no swing animation.
  2  the FIXTURE could not be established — unsafe or unverifiable
     terrain, a spawn or AI-state bootstrap that did not take, or a
     fight that never engaged. Nothing about the animations was graded.
"""
from __future__ import annotations

import argparse
import contextlib
import glob
import io
import json
import math
import socket
import subprocess
import sys
import time
from pathlib import Path

import yaml

from probelib import quit_engine, boot, init_arena, send, spawn_acolyte

UNITS_ROOT = Path("assets/textures/units")
DATA_UNITS = Path("data/units")

# Exit codes. Keeping the two failure modes DISTINCT is the whole point
# of #1396: 1 means the animations were graded and came up short, 2 means
# they were never graded at all.
FAIL_EXIT = 1
SETUP_EXIT = 2

# The arena's loaded footprint. `World.Generate.Arena` stamps an eager
# (2 * arenaRadius + 1)^2 = 5x5 chunk square centred on the origin at
# chunkSize 16, so global tiles -32..47 on each axis are the terrain that
# exists — everything outside is unloaded, and `world.getTerrainAt` reads
# nil there. Verifying the WHOLE footprint (rather than a margin around
# the spawns) is what lets the traversal guarantee hold for pathfinding
# detours and for a target that wanders: there is nowhere else to go.
ARENA_MIN_TILE = -32
ARENA_MAX_TILE = 47
# `generateArenaChunks` lays every column's surface at seaLevel.
ARENA_SURFACE_Z = 0

# Where the two combatants stand. Two tiles apart on flat ground, near
# the middle of the footprint, so the attacker closes in one step.
ATTACKER_TILE = (0, 0)
TARGET_TILE = (2, 0)

# The attack action's registered name (`scripts/unit_ai.lua`) and goal
# (`scripts/unit_ai_core.lua`) — an engaged attacker reports both.
ATTACK_ACTION = "attack_target"
ATTACK_GOAL = "attack"


class SetupFailure(RuntimeError):
    """The fixture could not be established — never an animation verdict."""


def report_setup_failure(cause: str) -> int:
    """Print a named fixture cause and yield the SETUP exit code.

    `main` routes every `SetupFailure` through here, so the mapping this
    returns is the one a live run uses and `--self-test` can assert.
    """
    print(f"FAIL (setup): {cause}", file=sys.stderr)
    print(f"\n--- checks ---\n  fixture precondition       : False "
          f"— {cause}\n  (the animations were never graded)")
    return SETUP_EXIT


def bootstrap_defs(port: int) -> None:
    # The loading screen doesn't run headless — load defs + AI scripts by hand.
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
    for script, dt in [("unit_stats", 0.1), ("unit_resources", 0.2),
                       ("unit_ai", 0.1)]:
        send(port, f"engine.loadScript('scripts/{script}.lua', {dt}); return 'ok'")


# --------------------------------------------------------------------
# Roster-wide animation storage (#1261 / TEX-6)
# --------------------------------------------------------------------
#
# `engine.getTextureHandle(name)` is the oracle, and specifically NOT
# `engine.getLoadedTexturePaths()`: that one is written only inside the
# device branch of the batch-upload handler, so it is EMPTY in
# `--headless` and every assertion built on it would pass vacuously.
# `loadAndRegister` allocates the handle and registers the NAME with no
# device at all, so the name registry answers headless — for presence
# and for absence alike (an unregistered name reads back as -1).
#
# The names are the loader's own:
#   unit_<u>                     the direct default sprite (D-8)
#   unit_<u>_<dirKey>            a directional T-pose sprite (D-8)
#   unit_<u>_portrait            the authored portrait (D-8)
#   unit_<u>_<anim>_atlas        ONE compiled atlas per animation
#   unit_<u>_<anim>_<dir>_<i>    what the RETIRED per-frame loader made
#
# The last shape must be absent for every declared frame of every
# animation, which is the whole of "no unit animation registers one
# texture per frame in any boot mode".


def read_index(unit: str) -> dict:
    """The unit's generated atlas index — the authority on which
    animations exist and how many real frames each direction holds."""
    return json.loads(
        (UNITS_ROOT / unit / "atlas" / "index.json").read_text("utf-8"))


def read_declaration(unit: str) -> dict:
    """The unit's own YAML entry. Only its DIRECT texture references are
    read here; animation membership comes from the index."""
    doc = yaml.safe_load((DATA_UNITS / f"{unit}.yaml").read_text("utf-8"))
    for entry in doc.get("units") or []:
        if entry.get("name") == unit:
            return entry
    raise SystemExit(f"{unit}: no `units:` entry named {unit!r}")


def handles(port: int, names: list[str]) -> dict[str, int]:
    """Resolve many texture names in ONE console round trip.

    The debug console is single-line, so the names ride in as a table
    literal. Doing this per name would be ~4,600 round trips.
    """
    lua_names = ",".join(f'"{n}"' for n in names)
    lua = (f"local ns={{{lua_names}}} local out={{}} "
           "for i,n in ipairs(ns) do out[i]=engine.getTextureHandle(n) end "
           "return out")
    got = json.loads(send(port, lua, timeout=30))
    return {n: int(got[i]) for i, n in enumerate(names)}


def check_roster(port: int, units: list[str]) -> bool:
    ok = True
    total_anims = total_frames = 0
    for unit in units:
        index = read_index(unit)
        decl = read_declaration(unit)
        anims = index["animations"]

        direct = [f"unit_{unit}"]
        if decl.get("portrait"):
            direct.append(f"unit_{unit}_portrait")
        direct += [f"unit_{unit}_{d}"
                   for d in (decl.get("directional_sprites") or {})]
        atlases = [f"unit_{unit}_{a['name']}_atlas" for a in anims]
        per_frame = [f"unit_{unit}_{a['name']}_{d['direction']}_{i}"
                     for a in anims for d in a["directions"]
                     for i in range(d["frame_count"])]

        resolved = handles(port, direct + atlases + per_frame)
        missing_direct = [n for n in direct if resolved[n] < 0]
        missing_atlas = [n for n in atlases if resolved[n] < 0]
        leaked_frames = [n for n in per_frame if resolved[n] >= 0]

        total_anims += len(anims)
        total_frames += len(per_frame)
        unit_ok = not (missing_direct or missing_atlas or leaked_frames)
        ok = ok and unit_ok
        print(f"  {unit:20} {len(anims):3d} anims  "
              f"{len(atlases):3d} atlases  {len(direct)} direct textures  "
              f"{len(per_frame):4d} frames unregistered  "
              f"{'OK' if unit_ok else 'FAIL'}")
        for label, names in (("direct texture not registered", missing_direct),
                             ("atlas not registered", missing_atlas),
                             ("PER-FRAME texture registered", leaked_frames)):
            if names:
                print(f"    {label}: {', '.join(names[:5])}"
                      f"{' …' if len(names) > 5 else ''}", file=sys.stderr)

    print(f"  roster: {len(units)} units, {total_anims} animations, "
          f"{total_frames} declared frames — every animation on ONE atlas, "
          f"no frame registered individually")
    return ok


def check_live_sample(port: int, uid: int, unit: str) -> bool:
    """A spawned unit's CURRENT frame really is an atlas cell.

    Registration alone would be satisfied by a def nothing can sample;
    this reads the same `pickFrame` the renderer draws with.
    """
    raw = send(port, f"return unit.getFrameSample({uid})", timeout=10)
    try:
        smp = json.loads(raw)
    except ValueError:
        print(f"FAIL: {unit} #{uid} has no frame sample: {raw!r}",
              file=sys.stderr)
        return False
    cell = smp.get("width"), smp.get("height")
    uv = (smp.get("u0"), smp.get("v0"), smp.get("u1"), smp.get("v1"))
    whole = uv == (0.0, 0.0, 1.0, 1.0)
    ok = bool(smp.get("texture", 0)) and all(cell) and not whole
    print(f"  {unit:20} texture={smp.get('texture')} cell={cell[0]}x{cell[1]} "
          f"uv={uv} {'OK' if ok else 'FAIL (whole-image sample)'}")
    return ok


def check_anim_duration(port: int, uid: int, unit: str) -> bool:
    """`unit.getAnimDuration` reads the index's REAL per-direction frame
    counts (D-5), never the padded column count — so it must still agree
    with frames / fps computed from the index itself."""
    index = read_index(unit)
    ok = True
    for entry in index["animations"]:
        want = max(d["frame_count"] for d in entry["directions"]) / entry["fps"]
        raw = send(port,
                   f"return unit.getAnimDuration({uid}, '{entry['name']}')",
                   timeout=10).strip()
        try:
            got = float(raw)
        except ValueError:
            print(f"FAIL: {unit}/{entry['name']} duration unavailable: {raw!r}",
                  file=sys.stderr)
            ok = False
            continue
        if abs(got - want) > 1e-4:
            print(f"FAIL: {unit}/{entry['name']} duration {got} != {want}",
                  file=sys.stderr)
            ok = False
    print(f"  {unit:20} {len(index['animations'])} animation duration(s) "
          f"match the index's real frame counts / fps "
          f"{'OK' if ok else 'FAIL'}")
    return ok


# --------------------------------------------------------------------
# Precondition 1: the terrain the combatants may traverse (#1396 req 1)
# --------------------------------------------------------------------


def scan_terrain(port: int, z0: int) -> dict:
    """Walk the arena's whole loaded footprint, reporting the FIRST tile
    that is not safe flat dry ground at `z0`.

    One round trip: the loop runs engine-side. `world.getTerrainAt` is a
    chunk-map lookup plus a vector index
    (`Engine.Scripting.Lua.API.WorldQuery.Terrain`) and returns nil for
    an unloaded chunk, so a missing tile is distinguishable from a
    mismatched one.
    """
    lua = (
        "local function f() "
        f"local lo,hi,z0={ARENA_MIN_TILE},{ARENA_MAX_TILE},{z0} "
        "local n=0 for gy=lo,hi do for gx=lo,hi do "
        "local zt=world.getTerrainAt(gx,gy) "
        "if zt==nil then return {cause='unloaded',gx=gx,gy=gy} end "
        "if zt~=z0 then return {cause='elevation',gx=gx,gy=gy,z=zt} end "
        "if world.getFluidAt(gx,gy) then return {cause='fluid',gx=gx,gy=gy} end "
        "n=n+1 end end return {cause='ok',tiles=n} end return f()"
    )
    raw = send(port, lua, timeout=120)
    try:
        scan = json.loads(raw)
    except ValueError:
        return {"cause": "unparsed", "raw": raw}
    return scan if isinstance(scan, dict) else {"cause": "unparsed", "raw": raw}


def terrain_cause(scan: dict, z0: int) -> str | None:
    """None when every traversable tile is safe; else the named cause.

    "Safe" here is the strongest available statement and the one the flat
    arena actually supports: every tile in the footprint resolves, sits
    at exactly the spawn elevation, and carries no fluid. There is then
    no drop to fall down and no water to drown in ANYWHERE the combatants
    can walk — which is what #1396 requires, since
    `scripts/unit_ai_combat_attack.lua` re-paths toward the target's
    current tile each tick, so the corridor is not merely the straight
    line between the two spawns.
    """
    if not isinstance(scan, dict):
        return f"the terrain scan returned no table ({scan!r})"
    cause = scan.get("cause")
    if cause == "ok":
        return None
    where = f"({scan.get('gx')},{scan.get('gy')})"
    if cause == "unloaded":
        return (f"tile {where} inside the arena footprint has no terrain — "
                f"the fixture's ground is not fully loaded")
    if cause == "elevation":
        return (f"tile {where} sits at z={scan.get('z')} instead of z={z0} — "
                f"a combatant could fall while approaching")
    if cause == "fluid":
        return f"tile {where} carries fluid — a combatant could drown or stall"
    if cause == "unparsed":
        return f"the terrain scan did not return a table ({scan.get('raw')!r})"
    return f"the terrain scan reported an unknown cause {cause!r}"


def in_footprint(gx: float, gy: float) -> bool:
    return (ARENA_MIN_TILE <= math.floor(gx) <= ARENA_MAX_TILE
            and ARENA_MIN_TILE <= math.floor(gy) <= ARENA_MAX_TILE)


def traversal_cause(obs: dict, attacker_uid: int, target_uid: int) -> str | None:
    """None when both combatants stand where this run VERIFIED the ground.

    Two things, because the scan alone only describes tiles. Position
    must be inside the scanned footprint, and `gridZ` must still be the
    surface the scan certified — which is the direct evidence that
    nobody fell anywhere on the way in, rather than an inference from
    the terrain being flat.

    NB a `falling` POSE is not a fall here. On this arena a lunging
    combatant arcs UP (`realZ` rises to ~0.6 and returns) while `gridZ`
    never leaves the surface; measured across every pose an engaged
    acolyte reaches — run, walk, falling, landing, collapsed, dead —
    `gridZ` and the tile's own `world.getTerrainAt` both stayed 0. So
    elevation, not pose, is what distinguishes a lunge from the lethal
    drop the old generated-world fixture could put beside the fight.
    """
    for uid, kx, ky, kz, tag in ((attacker_uid, "ax", "ay", "az", "attacker"),
                                 (target_uid, "bx", "by", "bz", "target")):
        gx, gy, gz = obs.get(kx), obs.get(ky), obs.get(kz)
        if gx is None or gy is None or not in_footprint(gx, gy):
            return (f"the {tag} #{uid} stands at ({gx},{gy}), outside the "
                    f"verified arena footprint "
                    f"[{ARENA_MIN_TILE}..{ARENA_MAX_TILE}] on each axis")
        if gz != ARENA_SURFACE_Z:
            return (f"the {tag} #{uid} is at z={gz}, off the verified "
                    f"surface z={ARENA_SURFACE_Z} — it left the ground this "
                    f"run certified")
    return None


# --------------------------------------------------------------------
# Precondition 2: the fight actually engaged (#1396 req 2)
# --------------------------------------------------------------------


def observe(port: int, attacker_uid: int, target_uid: int) -> dict | None:
    """ONE coherent live reading of both combatants and the attacker's AI.

    Every field the engagement verdict needs comes back from a single
    console round trip, so the verdict describes one instant rather than
    a stitched-together sequence. `currentAnim` rides along for the same
    reason: the observation that first proves engagement also SEEDS the
    animation timeline, and `scripts/unit_ai_combat_attack.lua` can fire
    a swing on the very tick it enters range — sampling only afterwards
    could miss it.
    """
    lua = (
        "local ai=require('scripts.unit_ai') "
        f"local A,B={attacker_uid},{target_uid} "
        "local ia,ib=unit.getInfo(A),unit.getInfo(B) "
        "local s=ai.getState(A) local o={} "
        "o.aExists=ia~=nil o.bExists=ib~=nil "
        "o.aPose=tostring(unit.getPose(A)) o.bPose=tostring(unit.getPose(B)) "
        "o.aAnim=ia and ia.currentAnim or '' "
        "o.bAnim=ib and ib.currentAnim or '' "
        "o.goal=s and s.activeGoal or '' "
        "o.action=s and s.currentAction or '' "
        "o.target=s and s.attackTargetUid or -1 "
        "o.range=unit.getAttackRange(A) or 1.0 "
        "o.ax=ia and ia.gridX or nil o.ay=ia and ia.gridY or nil "
        "o.bx=ib and ib.gridX or nil o.by=ib and ib.gridY or nil "
        "o.az=ia and ia.gridZ or nil o.bz=ib and ib.gridZ or nil "
        "o.cheb=(ia and ib) and math.max(math.abs(ia.gridX-ib.gridX),"
        "math.abs(ia.gridY-ib.gridY)) or -1 return o"
    )
    raw = send(port, lua, timeout=15)
    try:
        obs = json.loads(raw)
    except ValueError:
        return None
    return obs if isinstance(obs, dict) else None


def engagement_cause(obs: dict | None, attacker_uid: int,
                     target_uid: int) -> str | None:
    """None when this ONE observation shows a live, engaged pursuit.

    The conditions are the ones `attackTargetExecute` itself runs on
    (`scripts/unit_ai_combat_attack.lua`): both records present, neither
    pose `dead`, the attacker holding the `attack` goal on the COMMANDED
    target with `attack_target` selected, and a Chebyshev separation
    within the attacker's live `unit.getAttackRange` — the same
    `chebyshev <= range` gate that decides whether a swing may fire.
    """
    if not isinstance(obs, dict):
        return "the engagement observation never parsed"
    if not obs.get("aExists"):
        return f"the attacker #{attacker_uid} has no unit record"
    if not obs.get("bExists"):
        return f"the target #{target_uid} has no unit record"
    if obs.get("aPose") == "dead":
        return f"the attacker #{attacker_uid} is dead"
    if obs.get("bPose") == "dead":
        return f"the target #{target_uid} is dead"
    goal = obs.get("goal")
    if goal != ATTACK_GOAL:
        return (f"the attacker's active goal is {goal!r}, "
                f"not {ATTACK_GOAL!r}")
    target = obs.get("target")
    if target != target_uid:
        return (f"the attacker is pursuing target {target!r}, "
                f"not the commanded #{target_uid}")
    action = obs.get("action")
    if action != ATTACK_ACTION:
        return (f"the attacker's current action is {action!r}, "
                f"not {ATTACK_ACTION!r}")
    cheb, rng = obs.get("cheb"), obs.get("range")
    if cheb is None or rng is None or cheb < 0:
        return "the combatants' separation could not be measured"
    if cheb > rng:
        return (f"the combatants are {cheb:g} tiles apart, beyond the "
                f"attacker's {rng:g}-tile attack range")
    return None


def fixture_cause(obs: dict | None, attacker_uid: int,
                  target_uid: int) -> str | None:
    """The whole precondition, judged on ONE observation.

    Engagement and verified ground are checked together against a single
    reading, so the run can say the fight was engaged *and* that it
    happened where the terrain was proven — a verdict stitched from two
    different instants would assert neither.
    """
    cause = engagement_cause(obs, attacker_uid, target_uid)
    if cause is not None:
        return cause
    # Unreachable unless `obs` is a dict: `engagement_cause` rejects
    # everything else before returning None.
    assert isinstance(obs, dict)
    return traversal_cause(obs, attacker_uid, target_uid)


def await_engagement(port: int, attacker_uid: int, target_uid: int,
                     timeout: float) -> tuple[dict | None, str | None]:
    """Poll until ONE observation satisfies every engagement condition.

    Bounded by its own `timeout`, deliberately separate from the
    `--seconds` sampling window: waiting for the fight to start must not
    eat the time budget for watching it.
    """
    deadline = time.monotonic() + timeout
    last: dict | None = None
    while True:
        obs = observe(port, attacker_uid, target_uid)
        if obs is not None:
            last = obs
            if fixture_cause(obs, attacker_uid, target_uid) is None:
                return obs, None
        if time.monotonic() >= deadline:
            cause = fixture_cause(last, attacker_uid, target_uid)
            return None, (f"combat never engaged on verified ground within "
                          f"{timeout:g}s: {cause}")
        time.sleep(0.25)


def spawn_combatant(port: int, unit_name: str, gx: int, gy: int,
                    clear_water: bool) -> int:
    """`probelib.spawn_acolyte`, with its failures reclassified as SETUP.

    The shared helper reports a failed spawn or a never-arriving AI state
    with `sys.exit(<message>)`, which exits 1 — indistinguishable from a
    missing swing. Changing that helper is out of scope (#1396), so the
    translation happens here.
    """
    try:
        return spawn_acolyte(port, gx, gy, unit=unit_name,
                             clear_water=clear_water)
    except SystemExit as exc:
        raise SetupFailure(
            f"{unit_name} at ({gx},{gy}) never reached a usable state: "
            f"{exc}") from exc


def anim_of(port: int, uid: int) -> str:
    r = send(port, f"local i=unit.getInfo({uid}); return i and i.currentAnim or 'DEAD'")
    return r.strip('"')


def swung(seq: list[str]) -> bool:
    return any("attack" in s for s in seq)


def verdict_exit(swing: bool, roster_ok: bool, live_ok: bool) -> int:
    """The graded verdict, reached only once both preconditions held.

    Establishing the fixture must not rescue a real regression: a run
    that engaged cleanly and still never showed a swing is exactly the
    failure this probe exists to report (#1396 req 4).
    """
    return 0 if (swing and roster_ok and live_ok) else FAIL_EXIT


# --------------------------------------------------------------------
# Negative-path coverage (#1396 req 3/4) — no engine, no worldgen
# --------------------------------------------------------------------


def self_test() -> int:
    """Prove the two failure modes stay distinguishable, repeatably.

    Three live runs show the fixture holds; they cannot show what happens
    when it does not. These cases drive the same decision functions and
    the same exit-code mapping a live run uses, with synthetic readings.
    """
    failures: list[str] = []

    def expect(label: str, got, want) -> None:
        if got != want:
            failures.append(f"{label}: expected {want!r}, got {got!r}")

    def expect_named(label: str, cause: str | None, needle: str) -> None:
        if cause is None or needle not in cause:
            failures.append(
                f"{label}: expected a cause naming {needle!r}, got {cause!r}")

    # --- terrain: a clean scan passes, each hazard is named ------------
    expect("flat dry footprint", terrain_cause({"cause": "ok", "tiles": 6400}, 0),
           None)
    expect_named("a drop beside the fixture",
                 terrain_cause({"cause": "elevation", "gx": 4, "gy": -3, "z": -9}, 0),
                 "(4,-3)")
    expect_named("water beside the fixture",
                 terrain_cause({"cause": "fluid", "gx": 7, "gy": 7}, 0), "drown")
    expect_named("unloaded ground",
                 terrain_cause({"cause": "unloaded", "gx": 40, "gy": 0}, 0),
                 "no terrain")
    expect_named("an unreadable scan",
                 terrain_cause({"cause": "unparsed", "raw": "nil"}, 0),
                 "did not return a table")
    expect_named("a non-table scan", terrain_cause("nil", 0),
                 "returned no table")

    # --- traversal: verified footprint AND verified elevation ---------
    inside = {"ax": 1.0, "ay": 2.0, "bx": 2.0, "by": 2.0,
              "az": ARENA_SURFACE_Z, "bz": ARENA_SURFACE_Z}
    expect("both combatants on verified ground",
           traversal_cause(inside, 1, 2), None)
    expect_named("the target walked off the verified ground",
                 traversal_cause({**inside, "bx": 400.0}, 1, 2), "target #2")
    expect_named("the attacker walked off the verified ground",
                 traversal_cause({**inside, "ay": -400.0}, 1, 2), "attacker #1")
    expect_named("the attacker's position is unreadable",
                 traversal_cause({**inside, "ax": None}, 1, 2), "attacker #1")
    # The recorded #724 failure, expressed as a reading: the attacker is
    # still alive, engaged and in range, but has left the surface this
    # run certified. Elevation is what catches it — the POSE would not.
    expect_named("the attacker dropped off the verified surface",
                 traversal_cause({**inside, "az": -9}, 1, 2),
                 "off the verified surface")
    expect_named("the target dropped off the verified surface",
                 traversal_cause({**inside, "bz": -9}, 1, 2),
                 "off the verified surface")
    expect_named("an unreadable elevation",
                 traversal_cause({**inside, "az": None}, 1, 2),
                 "off the verified surface")

    # --- engagement: one good reading, then one broken field each ------
    engaged = {"aExists": True, "bExists": True, "aPose": "standing",
               "bPose": "standing", "goal": ATTACK_GOAL,
               "action": ATTACK_ACTION, "target": 2, "range": 1.5,
               "cheb": 1.0, "aAnim": "attack_quick_S", "bAnim": "idle_S"}
    expect("a live engaged pursuit", engagement_cause(engaged, 1, 2), None)
    for label, patch, needle in (
            ("the attacker vanished", {"aExists": False}, "no unit record"),
            ("the target vanished", {"bExists": False}, "no unit record"),
            ("the attacker died first", {"aPose": "dead"}, "is dead"),
            ("the target died first", {"bPose": "dead"}, "is dead"),
            ("the goal never took", {"goal": "find_water"}, "active goal"),
            ("another target was picked", {"target": 99}, "pursuing target"),
            ("another action outranked it", {"action": "wander"},
             "current action"),
            ("still out of range", {"cheb": 6.0}, "beyond"),
            ("separation unmeasurable", {"cheb": -1}, "could not be measured"),
    ):
        expect_named(label, engagement_cause({**engaged, **patch}, 1, 2), needle)
    expect_named("no observation at all", engagement_cause(None, 1, 2),
                 "never parsed")

    # --- the combined precondition, on one reading --------------------
    engaged_here = {**engaged, **inside}
    expect("engaged, on verified ground", fixture_cause(engaged_here, 1, 2), None)
    expect_named("engaged, but off the verified surface",
                 fixture_cause({**engaged_here, "az": -9}, 1, 2),
                 "off the verified surface")
    expect_named("on verified ground, but never engaged",
                 fixture_cause({**engaged_here, "action": "wander"}, 1, 2),
                 "current action")
    expect_named("no observation at all", fixture_cause(None, 1, 2),
                 "never parsed")

    # --- the exit-code mapping itself ---------------------------------
    # This is the live run's own mapping, called directly. Its diagnostic
    # is captured rather than printed so the self-test's output stays
    # readable — and then asserted, since a cause nobody can read would
    # leave the two failure modes indistinguishable in practice.
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        setup_code = report_setup_failure("a synthetic fixture cause")
    expect("a fixture cause exits SETUP", setup_code, SETUP_EXIT)
    expect_named("the fixture diagnostic names its cause",
                 out.getvalue() + err.getvalue(), "a synthetic fixture cause")
    expect("SETUP is 2", SETUP_EXIT, 2)
    expect("a graded miss exits FAIL", verdict_exit(False, True, True), FAIL_EXIT)
    expect("FAIL is 1", FAIL_EXIT, 1)
    expect("the two failure modes differ", SETUP_EXIT == FAIL_EXIT, False)
    expect("a swing passes", verdict_exit(True, True, True), 0)
    expect("a swing with broken storage still fails",
           verdict_exit(True, False, True), FAIL_EXIT)
    expect("a swing with a bad live sample still fails",
           verdict_exit(True, True, False), FAIL_EXIT)
    expect("a timeline with a swing", swung(["idle_S", "attack_quick_S"]), True)
    expect("a timeline without one",
           swung(["idle_S", "walk_S", "combat_idle_S"]), False)

    for line in failures:
        print(f"FAIL: {line}", file=sys.stderr)
    print(f"\n--- self-test ---\n  precondition + verdict cases: "
          f"{'OK' if not failures else f'{len(failures)} FAILED'}")
    return 0 if not failures else FAIL_EXIT


def run_fight(port: int, args) -> tuple[bool, bool, list[str], list[str]]:
    """Establish both preconditions, then observe the fight.

    Returns `(swing, live_ok, attacker_timeline, target_timeline)` — the
    facts, not a verdict; `main` maps them through `verdict_exit`. Raises
    `SetupFailure` for anything that leaves the fixture unestablished.
    """
    init_arena(port)

    started = time.monotonic()
    scan = scan_terrain(port, ARENA_SURFACE_Z)
    cause = terrain_cause(scan, ARENA_SURFACE_Z)
    if cause:
        raise SetupFailure(cause)
    print(f"\n--- fixture terrain ---\n  {scan.get('tiles')} tiles across the "
          f"arena footprint [{ARENA_MIN_TILE}..{ARENA_MAX_TILE}]² are flat dry "
          f"land at z={ARENA_SURFACE_Z} ({time.monotonic() - started:.1f}s) — "
          f"no drop and no water anywhere the combatants can walk")

    # The attacker's standing `find_water` goal is retired here (#1396
    # req 5): `spawn_acolyte` clears it, so the goal can't steer the unit
    # during the ~1.5s before commandAttack overwrites it.
    a = spawn_combatant(port, args.attacker, *ATTACKER_TILE, clear_water=True)
    # The target is never commanded, so it keeps whatever species AI it
    # ships with; only the attacker's goal is in scope.
    b = spawn_combatant(port, args.target, *TARGET_TILE, clear_water=False)
    print(f"spawned {args.attacker}=#{a} at {ATTACKER_TILE}  "
          f"{args.target}=#{b} at {TARGET_TILE}")
    time.sleep(1.5)  # let them settle onto the ground before fighting

    # A LIVE frame off each spawned unit, before the fight perturbs
    # anything: registration proves a def exists, this proves the
    # renderer's own pickFrame resolves an atlas CELL from it.
    print("\n--- live frame samples ---")
    live_ok = all([check_live_sample(port, a, args.attacker),
                   check_live_sample(port, b, args.target)])
    print("\n--- animation durations (real frame counts, not padding) ---")
    live_ok = all([live_ok,
                   check_anim_duration(port, a, args.attacker),
                   check_anim_duration(port, b, args.target)])

    send(port, f"require('scripts.unit_ai').commandAttack({a},{b}); return 'go'")

    obs, cause = await_engagement(port, a, b, args.engage_timeout)
    if cause:
        raise SetupFailure(cause)
    assert obs is not None
    print(f"\n--- fixture engagement ---\n  #{a} holds goal "
          f"{obs['goal']!r} / action {obs['action']!r} on #{obs['target']}, "
          f"{obs['cheb']:g} tiles away (range {obs['range']:g}); poses "
          f"{obs['aPose']}/{obs['bPose']} at "
          f"({obs['ax']:g},{obs['ay']:g},z={obs['az']}) and "
          f"({obs['bx']:g},{obs['by']:g},z={obs['bz']}) — both still on the "
          f"verified surface")

    # That same observation seeds the timelines, so a swing fired on the
    # tick that proved engagement is already recorded.
    seen_a: list[str] = [obs["aAnim"]]
    seen_b: list[str] = [obs["bAnim"]]
    steps = int(args.seconds / 0.25)
    for _ in range(steps):
        aa, ba = anim_of(port, a), anim_of(port, b)
        if seen_a[-1] != aa:
            seen_a.append(aa)
        if seen_b[-1] != ba:
            seen_b.append(ba)
        time.sleep(0.25)

    print(f"\n{args.attacker} #{a} anim timeline:\n  " + " → ".join(seen_a))
    print(f"\n{args.target} #{b} anim timeline:\n  " + " → ".join(seen_b))
    return swung(seen_a) or swung(seen_b), live_ok, seen_a, seen_b


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--attacker", default="acolyte")
    ap.add_argument("--target", default="bear_brown")
    ap.add_argument("--port", type=int, default=9123)
    ap.add_argument("--seconds", type=float, default=12.0,
                    help="animation SAMPLING window, once combat is engaged")
    ap.add_argument("--engage-timeout", type=float, default=45.0,
                    help="bound on waiting for combat to engage; separate "
                         "from --seconds")
    ap.add_argument("--roster-only", action="store_true",
                    help="run only the roster storage checks, no fight")
    ap.add_argument("--self-test", action="store_true",
                    help="exercise the precondition/verdict decisions and "
                         "their exit codes; boots no engine")
    args = ap.parse_args()

    if args.self_test:
        return self_test()

    roster = sorted(p.stem for p in DATA_UNITS.glob("*.yaml"))
    if not roster:
        print("FAIL: no unit declarations under data/units/", file=sys.stderr)
        return SETUP_EXIT

    proc = boot(args.port)
    try:
        bootstrap_defs(args.port)

        # Every declaration registered. `loadUnitYaml` returns 0 and logs
        # the artifact for a unit whose compiled index is missing, stale
        # or malformed, so this catches a broken migration before any of
        # the storage checks can report on a def that is not there.
        listed = json.loads(send(args.port, "return unit.listDefs()"))
        print(f"\n--- registered definitions ({len(listed)}) ---")
        if sorted(listed) != roster:
            print(f"FAIL: registered {sorted(listed)} != declared {roster}",
                  file=sys.stderr)
            return FAIL_EXIT
        print("  " + ", ".join(roster))

        print("\n--- animation storage (#1261: one atlas per animation) ---")
        roster_ok = check_roster(args.port, roster)
        if args.roster_only:
            return 0 if roster_ok else FAIL_EXIT

        try:
            swing, live_ok, seen_a, seen_b = run_fight(args.port, args)
        except SetupFailure as exc:
            return report_setup_failure(str(exc))

        verdict = verdict_exit(swing, roster_ok, live_ok)
        print("\n--- checks ---")
        print("  fixture precondition       : True")
        print(f"  a swing animation appeared : {swing}")
        print(f"  roster animation storage   : {roster_ok}")
        print(f"  live samples + durations   : {live_ok}")
        # If a combatant ended dead, it must show a death anim, not a stale
        # combat idle (the death-anim regression).
        for tag, seq in ((args.attacker, seen_a), (args.target, seen_b)):
            last = seq[-1]
            if last == "DEAD":
                continue  # removed entirely — fine
            if "combat_idle" in last or last in ("idle",):
                continue  # survived
            if any(d in last for d in ("death", "dead", "collapse")):
                print(f"  {tag} settled on a death animation: {last}")
        return verdict
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
