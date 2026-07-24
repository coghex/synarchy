#!/usr/bin/env python3
"""Headless probe for issue #607: impact blood from fresh wounds.

Boots headless, spawns one acolyte on a flat arena, and drives the
debug `unit.injure(...)` path (Engine.Scripting.Lua.API.Units) end to
end against the blood.* debug surface (#604/#606) to verify the
wound-kind/severity -> impact-blood mapping (Blood.Impact).

Checks:
  1. `stab` creates pool/drop-style blood near the unit.
  2. a high-severity `stab` creates a stronger request than a
     low-severity `stab`.
  3. `slash` creates spatter/streak-style blood.
  4. ordinary `blunt` creates no blood.
  5. ordinary `fracture`/`concussion` below catastrophic thresholds
     create no direct blood.
  6. crushed/pulverized/pulped/destruction-level blunt trauma
     (blunt/fracture/concussion at/above their catastrophic
     thresholds) creates blood.
  7. `arterial` and `severed` create high-volume (never "minor") blood
     even at a low nominal severity.
  8. `internal` creates no direct blood, regardless of severity.
  9. clearing blood between cases leaves no stale decals.

PASS  = all checks hold.
FAIL  = any check violated (bug in the impact mapping or its wiring).
"""
from __future__ import annotations
import argparse
import glob
import sys
from probelib import quit_engine, boot, init_arena, send, send_json, spawn_acolyte

PORT = 9010
LOG = "/tmp/blood_impact_probe_engine.log"


def bootstrap_defs(port: int) -> None:
    """Load the substance/item/equipment/material/unit YAML defs the
    loading screen would normally load (it doesn't run headless) —
    unit.spawn fails without them. Mirrors tools/combat_anim_probe.py /
    tools/disarm_probe.py."""
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
    # This probe reuses ONE unit across every case (never destroyed) and
    # each case's wounds keep bleeding externally between checks — #882's
    # bleeding-trail emitter would otherwise let unit_ai's wander tick add
    # movement-triggered trail decals that contaminate expect_blood's exact
    # decal-count assertions. Neutralise wander (movement_probe.py's
    # technique) so this probe's own explicit calls are the only activity.
    send(port,
         "pcall(function() require('scripts.unit_ai').update = function() end end); "
         "return 'ai-off'")


def reset_blood() -> None:
    cleared = send(PORT, "return blood.clear()")
    if cleared.lower() != "true":
        print(f"FAIL (setup): blood.clear() returned {cleared!r}")
        sys.exit(2)
    remaining = send_json(PORT, "return blood.listDecals()") or []
    if remaining:
        print(f"FAIL: blood.clear() left stale decals: {remaining!r}")
        sys.exit(1)


def injure(uid: int, part: str, kind: str, sev: float) -> bool:
    ok = send(PORT, f"return unit.injure({uid}, '{part}', '{kind}', {sev})")
    if ok.lower() not in ("true", "false"):
        print(f"FAIL (setup): unit.injure(...) -> {ok!r}")
        sys.exit(2)
    return ok.lower() == "true"


def decals() -> list:
    return send_json(PORT, "return blood.listDecals()") or []


def style_of(decal: dict) -> str:
    tex = send_json(PORT, f"return blood.getTexture({decal['texture']})")
    if not tex or "style" not in tex:
        print(f"FAIL: blood.getTexture({decal['texture']}) -> {tex!r}")
        sys.exit(1)
    return tex["style"]


def expect_no_blood(uid: int, kind: str, sev: float, label: str) -> None:
    reset_blood()
    injure(uid, "torso", kind, sev)
    got = decals()
    if got:
        print(f"FAIL: {label} ({kind} sev={sev}) unexpectedly created "
              f"blood: {got!r}")
        sys.exit(1)
    print(f"PASS: {label} ({kind} sev={sev}) created no blood")


def expect_blood(uid: int, kind: str, sev: float, label: str,
                  styles: tuple[str, ...] | None = None) -> dict:
    reset_blood()
    injure(uid, "torso", kind, sev)
    got = decals()
    if len(got) != 1:
        print(f"FAIL: {label} ({kind} sev={sev}) expected exactly 1 "
              f"decal, got {len(got)}: {got!r}")
        sys.exit(1)
    d = got[0]
    if d["woundKind"] != kind:
        print(f"FAIL: {label} decal woundKind={d['woundKind']!r}, "
              f"expected {kind!r}")
        sys.exit(1)
    if styles is not None:
        st = style_of(d)
        if st not in styles:
            print(f"FAIL: {label} style={st!r}, expected one of {styles!r}")
            sys.exit(1)
    print(f"PASS: {label} ({kind} sev={sev}) created blood "
          f"(style={style_of(d) if styles is None else st})")
    return d


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9010)
    args = ap.parse_args()
    global PORT
    PORT = args.port

    proc = boot(PORT, log=LOG)
    try:
        bootstrap_defs(PORT)
        init_arena(PORT)
        uid = spawn_acolyte(PORT, 10, 10, clear_water=False)

        # --- 1/2. stab: pool/drops style, scales with severity --------
        lo = expect_blood(uid, "stab", 0.1, "low-severity stab",
                           styles=("pool", "drops"))
        hi = expect_blood(uid, "stab", 0.9, "high-severity stab",
                           styles=("pool", "drops"))
        if not (hi["opacity"] > lo["opacity"]):
            print(f"FAIL: high-severity stab opacity ({hi['opacity']}) is "
                  f"not stronger than low-severity stab ({lo['opacity']})")
            return 1
        print(f"PASS: high-severity stab (opacity={hi['opacity']:.3f}) is "
              f"stronger than low-severity stab (opacity={lo['opacity']:.3f})")

        # --- 3. slash: spatter/streak style -----------------------------
        expect_blood(uid, "slash", 0.5, "slash", styles=("spatter", "streak"))

        # --- 4/5. ordinary blunt-family + fracture: no blood ------------
        expect_no_blood(uid, "blunt", 0.5, "ordinary blunt")
        expect_no_blood(uid, "blunt", 0.84, "REGRESSION: bashed/slammed "
                         "blunt (just below the crushing/pulverizing/"
                         "pulping tier)")
        expect_no_blood(uid, "fracture", 0.5, "ordinary fracture")
        expect_no_blood(uid, "concussion", 0.5, "ordinary concussion")

        # --- 6. catastrophic blunt-family trauma: blood -----------------
        expect_blood(uid, "blunt", 0.9, "crushing/pulverizing/pulping blunt")
        expect_blood(uid, "fracture", 1.0, "destruction-level (crushed "
                     "skull/ribcage) fracture")
        expect_blood(uid, "concussion", 0.9, "pulverized-brain-level "
                     "concussion")

        # --- 7. arterial/severed: always high-volume --------------------
        for kind in ("arterial", "severed"):
            d = expect_blood(uid, kind, 0.05, f"low-nominal-severity {kind}")
            if d["severity"] == "minor":
                print(f"FAIL: {kind} at low nominal severity still reads "
                      f"'minor' — expected a high-volume floor")
                return 1
            print(f"PASS: {kind} floors at severity={d['severity']!r} "
                  f"(never 'minor')")

        # --- 8. internal: no direct blood, even at max severity ---------
        expect_no_blood(uid, "internal", 1.0, "internal (max severity)")

        # --- 9. clearing between cases leaves no stale decals -----------
        # Already exercised by every reset_blood() call above (each
        # asserts the list is empty right after clear); one more
        # explicit check for good measure.
        reset_blood()
        print("PASS: clearing blood between cases leaves no stale decals")

        print("\nPASS: all impact blood checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
