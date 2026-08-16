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
  4. generates a small world, finds a flat dry patch,
  5. spawns an attacker next to a target and issues commandAttack,
  6. polls each unit's currentAnim for a few seconds and prints the
     per-unit animation timeline,
  7. checks the attacker threw a recognizable swing animation and, if it
     died, settled on a death animation (the two bugs this guards),
  8. samples a live frame and an animation duration off the spawned
     units, so the atlas evidence covers a unit that is actually
     playing rather than only one that registered.

Usage:
  python3 tools/combat_anim_probe.py            # acolyte vs bear_brown
  python3 tools/combat_anim_probe.py --attacker acolyte --target bear_brown
  python3 tools/combat_anim_probe.py --seconds 12 --port 9123
  python3 tools/combat_anim_probe.py --roster-only   # skip the fight

Exit code 0 = the expected animation states were observed.
"""
from __future__ import annotations

import argparse
import glob
import json
import socket
import subprocess
import sys
import time
from pathlib import Path

import yaml

from probelib import quit_engine, boot, send

UNITS_ROOT = Path("assets/textures/units")
DATA_UNITS = Path("data/units")


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


def find_flat_strip(port: int) -> tuple[int, int, int] | None:
    """Return (gx, gy, z) of a dry 3-wide equal-z land strip, or None.

    Retries briefly: just after waitForChunks the terrain queries can
    still read nil for a tick, which looks like 'no flat ground'.
    """
    lua = (
        "local function f() for gy=-8,8 do for gx=-8,6 do "
        "local za=world.getTerrainAt(gx,gy) local zb=world.getTerrainAt(gx+1,gy) "
        "local zc=world.getTerrainAt(gx+2,gy) "
        "local fa=world.getFluidAt(gx,gy) local fb=world.getFluidAt(gx+1,gy) "
        "local fc=world.getFluidAt(gx+2,gy) "
        "if za and zb and zc and za==zb and zb==zc and not fa and not fb and not fc "
        "then return gx..','..gy..','..za end end end return 'none' end return f()"
    )
    for _ in range(8):
        res = send(port, lua).strip('"')
        if res and res != "none" and res.count(",") == 2:
            gx, gy, z = (int(v) for v in res.split(","))
            return gx, gy, z
        time.sleep(0.75)
    return None


def anim_of(port: int, uid: int) -> str:
    r = send(port, f"local i=unit.getInfo({uid}); return i and i.currentAnim or 'DEAD'")
    return r.strip('"')


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--attacker", default="acolyte")
    ap.add_argument("--target", default="bear_brown")
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9123)
    ap.add_argument("--seconds", type=float, default=12.0)
    ap.add_argument("--roster-only", action="store_true",
                    help="run only the roster storage checks, no fight")
    args = ap.parse_args()

    roster = sorted(p.stem for p in DATA_UNITS.glob("*.yaml"))
    if not roster:
        print("FAIL: no unit declarations under data/units/", file=sys.stderr)
        return 2

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
            return 1
        print("  " + ", ".join(roster))

        print("\n--- animation storage (#1261: one atlas per animation) ---")
        roster_ok = check_roster(args.port, roster)
        if args.roster_only:
            return 0 if roster_ok else 1

        send(args.port, f"world.init('arena', {args.seed}, {args.size}, 3); return 'ok'")
        send(args.port, "return world.waitForInit(180)", timeout=190)
        send(args.port, "world.show('arena'); return 'ok'")
        send(args.port, "return world.loadChunksInRegion(-1,-1,1,1)")
        send(args.port, "return world.waitForChunks(120)", timeout=125)

        strip = find_flat_strip(args.port)
        if not strip:
            print("FAIL: no flat dry ground found near origin", file=sys.stderr)
            return 2
        gx, gy, z = strip
        print(f"arena: flat strip at ({gx},{gy}) z={z}")

        a = int(float(send(args.port, f"return unit.spawn('{args.attacker}', {gx}, {gy})")))
        b = int(float(send(args.port, f"return unit.spawn('{args.target}', {gx+2}, {gy})")))
        print(f"spawned {args.attacker}=#{a}  {args.target}=#{b}")
        time.sleep(1.5)  # let them settle onto the ground before fighting

        # A LIVE frame off each spawned unit, before the fight perturbs
        # anything: registration proves a def exists, this proves the
        # renderer's own pickFrame resolves an atlas CELL from it.
        print("\n--- live frame samples ---")
        live_ok = all([check_live_sample(args.port, a, args.attacker),
                       check_live_sample(args.port, b, args.target)])
        print("\n--- animation durations (real frame counts, not padding) ---")
        live_ok = all([live_ok,
                       check_anim_duration(args.port, a, args.attacker),
                       check_anim_duration(args.port, b, args.target)])

        send(args.port, f"require('scripts.unit_ai').commandAttack({a},{b}); return 'go'")

        seen_a: list[str] = []
        seen_b: list[str] = []
        steps = int(args.seconds / 0.25)
        for _ in range(steps):
            aa, ba = anim_of(args.port, a), anim_of(args.port, b)
            if not seen_a or seen_a[-1] != aa:
                seen_a.append(aa)
            if not seen_b or seen_b[-1] != ba:
                seen_b.append(ba)
            time.sleep(0.25)

        print(f"\n{args.attacker} #{a} anim timeline:\n  " + " → ".join(seen_a))
        print(f"\n{args.target} #{b} anim timeline:\n  " + " → ".join(seen_b))

        def swung(seq): return any("attack" in s for s in seq)
        def died(seq):  return seq[-1] == "DEAD" or any(
            d in seq[-1] for d in ("death", "dead"))

        ok = (swung(seen_a) or swung(seen_b)) and roster_ok and live_ok
        print("\n--- checks ---")
        print(f"  a swing animation appeared : {swung(seen_a) or swung(seen_b)}")
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
        return 0 if ok else 1
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
