#!/usr/bin/env python3
"""Headless probe for issue #607: impact blood from fresh wounds.

Boots headless, spawns a FRESH acolyte per case on a flat arena, and
drives the debug `unit.injure(...)` path
(Engine.Scripting.Lua.API.Units) end to end against the blood.* debug
surface (#604/#606) to verify the wound-kind/severity -> impact-blood
mapping (Blood.Impact).

One unit per case, destroyed afterwards, because every case's exact
decal-count assertion ("exactly 1", "none") is about the ONE-SHOT
impact mark: since #883 a unit standing still with an externally
bleeding wound also grows a local pool (Blood.Pool), so a unit reused
across cases would keep accumulating wounds and start layering pool
marks into a later case's count. A brand-new unit's accumulator is
stamped at its first wound tick and cannot emit its first layer for
another Blood.Pool.ptMinCadence seconds — far longer than the couple
of console round-trips between the injure and the count.

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

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps human-readable
per-check output.
"""
from __future__ import annotations
import argparse
import glob
import sys

import probe_protocol
from probelib import quit_engine, boot, init_arena, send, send_json, spawn_acolyte

PORT = 9010
LOG = "/tmp/blood_impact_probe_engine.log"
LOG_NAME = "blood_impact_probe_engine.log"
PROBE_KEY = "blood_impact"

CHECKS = [
    ("stab_style", "stab wounds create pool/drop-style impact blood"),
    ("stab_severity_scaling",
     "high-severity stab creates a stronger request than low-severity stab"),
    ("slash_style", "slash wounds create spatter/streak-style impact blood"),
    ("ordinary_blunt_dry", "ordinary blunt trauma creates no impact blood"),
    ("ordinary_fracture_concussion_dry",
     "ordinary fracture and concussion create no impact blood"),
    ("catastrophic_blunt_family_blood",
     "catastrophic blunt, fracture and concussion trauma create impact blood"),
    ("arterial_severed_volume_floor",
     "arterial and severed wounds never produce minor-volume impact blood"),
    ("internal_dry", "internal wounds create no direct impact blood"),
    ("clear_removes_decals", "clearing blood leaves no stale decals"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)


class ProbeSetupError(RuntimeError):
    """A fixture failure that preserves this probe's standalone exit 2."""


class ProbeCheckError(RuntimeError):
    """One current behavior group failed before it could return evidence."""

    def __init__(self, message: str, detail: dict | None = None):
        super().__init__(message)
        self.detail = detail or {}


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
    # Each case uses a fresh unit, but a wound can still emit before that unit
    # is destroyed — #882's bleeding-trail emitter would otherwise let
    # unit_ai's wander tick add a movement-triggered trail decal that
    # contaminates expect_blood's exact count. Neutralise wander
    # (movement_probe.py's technique) so the probe's explicit calls are the
    # only activity.
    send(port,
         "pcall(function() require('scripts.unit_ai').update = function() end end); "
         "return 'ai-off'")


def reset_blood() -> dict:
    cleared = send(PORT, "return blood.clear()")
    if cleared.lower() != "true":
        raise ProbeSetupError(f"blood.clear() returned {cleared!r}")
    remaining = send_json(PORT, "return blood.listDecals()") or []
    if remaining:
        raise ProbeCheckError(
            f"blood.clear() left stale decals: {remaining!r}",
            {"remaining_decals": remaining})
    return {"cleared": True, "remaining_count": 0}


def injure(uid: int, part: str, kind: str, sev: float) -> bool:
    ok = send(PORT, f"return unit.injure({uid}, '{part}', '{kind}', {sev})")
    if ok.lower() not in ("true", "false"):
        raise ProbeSetupError(f"unit.injure(...) -> {ok!r}")
    return ok.lower() == "true"


def decals() -> list:
    return send_json(PORT, "return blood.listDecals()") or []


def style_of(decal: dict) -> str:
    tex = send_json(PORT, f"return blood.getTexture({decal['texture']})")
    if not tex or "style" not in tex:
        raise ProbeCheckError(
            f"blood.getTexture({decal['texture']}) -> {tex!r}",
            {"decal": decal, "texture_record": tex})
    return tex["style"]


def spawn_fresh() -> int:
    """A brand-new unit per case (see the module docstring) — never
    reused, so no prior case's ongoing bleed can layer a Blood.Pool
    mark into this case's exact decal count."""
    return spawn_acolyte(PORT, 10, 10, clear_water=False)


def destroy(uid: int) -> None:
    send(PORT, f"unit.destroy({uid}); return 'ok'", expect_result=False)


def expect_no_blood(kind: str, sev: float, label: str,
                    rep: probe_protocol.Reporter) -> dict:
    reset_blood()
    uid = spawn_fresh()
    injure(uid, "torso", kind, sev)
    got = decals()
    destroy(uid)
    if got:
        raise ProbeCheckError(
            f"{label} ({kind} sev={sev}) unexpectedly created blood: {got!r}",
            {"kind": kind, "severity": sev, "decals": got})
    rep.note(f"PASS: {label} ({kind} sev={sev}) created no blood")
    return {"kind": kind, "severity": sev, "decal_count": 0}


def expect_blood(kind: str, sev: float, label: str,
                 rep: probe_protocol.Reporter,
                 styles: tuple[str, ...] | None = None) -> dict:
    reset_blood()
    uid = spawn_fresh()
    injure(uid, "torso", kind, sev)
    got = decals()
    destroy(uid)
    if len(got) != 1:
        raise ProbeCheckError(
            f"{label} ({kind} sev={sev}) expected exactly 1 decal, got "
            f"{len(got)}: {got!r}",
            {"kind": kind, "severity": sev, "decals": got})
    d = got[0]
    if d["woundKind"] != kind:
        raise ProbeCheckError(
            f"{label} decal woundKind={d['woundKind']!r}, expected {kind!r}",
            {"kind": kind, "severity": sev, "decal": d})
    st = style_of(d)
    if styles is not None and st not in styles:
        raise ProbeCheckError(
            f"{label} style={st!r}, expected one of {styles!r}",
            {"kind": kind, "severity": sev, "style": st,
             "expected_styles": list(styles), "decal": d})
    rep.note(f"PASS: {label} ({kind} sev={sev}) created blood (style={st})")
    return d


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9010)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 check declaration and "
                         "exit without booting an engine")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args.port, rep)
    finally:
        rep.close()


def _run(port: int, rep: probe_protocol.Reporter) -> int:
    global PORT
    PORT = port

    proc = boot(PORT, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    try:
        bootstrap_defs(PORT)
        init_arena(PORT)

        def report_group(check_id: str, action) -> tuple[int, object | None]:
            try:
                human, detail, value = action()
            except ProbeSetupError as error:
                rep.abort(str(error), {"check": check_id})
                return 2, None
            except ProbeCheckError as error:
                rep.check(check_id, False, str(error), error.detail)
                return 1, None
            rep.check(check_id, True, human, detail)
            return 0, value

        # --- 1/2. stab: pool/drops style, scales with severity --------
        def stab_style():
            lo = expect_blood("stab", 0.1, "low-severity stab", rep,
                              styles=("pool", "drops"))
            hi = expect_blood("stab", 0.9, "high-severity stab", rep,
                              styles=("pool", "drops"))
            return ("low- and high-severity stab created pool/drop-style blood",
                    {"low": lo, "high": hi}, (lo, hi))

        rc, stab_pair = report_group("stab_style", stab_style)
        if rc:
            return rc
        lo, hi = stab_pair

        scaling = hi["opacity"] > lo["opacity"]
        scaling_human = (
            f"high-severity stab (opacity={hi['opacity']:.3f}) is stronger "
            f"than low-severity stab (opacity={lo['opacity']:.3f})" if scaling
            else f"high-severity stab opacity ({hi['opacity']}) is not "
                 f"stronger than low-severity stab ({lo['opacity']})")
        rep.check("stab_severity_scaling", scaling, scaling_human,
                  {"low_opacity": lo["opacity"],
                   "high_opacity": hi["opacity"]})
        if not scaling:
            return 1

        # --- 3. slash: spatter/streak style -----------------------------
        def slash_style():
            decal = expect_blood(
                "slash", 0.5, "slash", rep,
                styles=("spatter", "streak"))
            return ("slash created spatter/streak-style impact blood",
                    {"decal": decal}, decal)

        rc, _ = report_group("slash_style", slash_style)
        if rc:
            return rc

        # --- 4/5. ordinary blunt-family + fracture: no blood ------------
        def ordinary_blunt():
            cases = [
                expect_no_blood("blunt", 0.5, "ordinary blunt", rep),
                expect_no_blood(
                    "blunt", 0.84,
                    "REGRESSION: bashed/slammed blunt (just below the "
                    "crushing/pulverizing/pulping tier)", rep),
            ]
            return ("ordinary blunt cases created no impact blood",
                    {"cases": cases}, cases)

        rc, _ = report_group("ordinary_blunt_dry", ordinary_blunt)
        if rc:
            return rc

        def ordinary_fracture_concussion():
            cases = [
                expect_no_blood("fracture", 0.5, "ordinary fracture", rep),
                expect_no_blood("concussion", 0.5, "ordinary concussion", rep),
            ]
            return ("ordinary fracture and concussion created no impact blood",
                    {"cases": cases}, cases)

        rc, _ = report_group("ordinary_fracture_concussion_dry",
                             ordinary_fracture_concussion)
        if rc:
            return rc

        # --- 6. catastrophic blunt-family trauma: blood -----------------
        def catastrophic_blunt_family():
            cases = [
                expect_blood("blunt", 0.9,
                             "crushing/pulverizing/pulping blunt", rep),
                expect_blood("fracture", 1.0,
                             "destruction-level (crushed skull/ribcage) fracture",
                             rep),
                expect_blood("concussion", 0.9,
                             "pulverized-brain-level concussion", rep),
            ]
            return ("catastrophic blunt-family trauma created impact blood",
                    {"decals": cases}, cases)

        rc, _ = report_group("catastrophic_blunt_family_blood",
                             catastrophic_blunt_family)
        if rc:
            return rc

        # --- 7. arterial/severed: always high-volume --------------------
        def volume_floor():
            cases = []
            for kind in ("arterial", "severed"):
                d = expect_blood(kind, 0.05,
                                 f"low-nominal-severity {kind}", rep)
                if d["severity"] == "minor":
                    raise ProbeCheckError(
                        f"{kind} at low nominal severity still reads 'minor' "
                        "— expected a high-volume floor",
                        {"kind": kind, "decal": d})
                rep.note(f"PASS: {kind} floors at severity={d['severity']!r} "
                         "(never 'minor')")
                cases.append(d)
            return ("arterial and severed wounds held their high-volume floor",
                    {"decals": cases}, cases)

        rc, _ = report_group("arterial_severed_volume_floor", volume_floor)
        if rc:
            return rc

        # --- 8. internal: no direct blood, even at max severity ---------
        def internal_dry():
            case = expect_no_blood(
                "internal", 1.0, "internal (max severity)", rep)
            return ("max-severity internal wound created no direct impact blood",
                    {"case": case}, case)

        rc, _ = report_group("internal_dry", internal_dry)
        if rc:
            return rc

        # --- 9. clearing between cases leaves no stale decals -----------
        # Already exercised by every reset_blood() call above (each
        # asserts the list is empty right after clear); one more
        # explicit check for good measure.
        rc, _ = report_group("clear_removes_decals", lambda: (
            "clearing blood between cases leaves no stale decals",
            reset_blood(), None))
        if rc:
            return rc

        rep.note("\nPASS: all impact blood checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
