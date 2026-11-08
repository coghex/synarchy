#!/usr/bin/env python3
"""Per-unit expected values for the physiology probe (#2960).

`tools/physiology_probe.py` used to judge two edge-of-variation checks against
fixed population thresholds (`circ min >= 0.75`, humid-heat mean rise
`> 0.1 C`). The rolled population legitimately reaches under both (#2944), so
the probe now judges each acolyte against what the engine's own rule predicts
from THAT unit's recorded inputs. This module is the pure, engine-free half:
every function is a judgement over plain observation dicts, so
tools/test_physiology_probe.py proves it with no engine.

Circulation
-----------
`circulation_rule` is a Python port of `Unit.Physiology.Circulation.
circulationRule` (src/Unit/Physiology/Circulation.hs), the one production
implementation. The port cannot drift silently: tools/fixtures/
physiology_circulation_parity.json holds input/output cases that BOTH this port
(tools/test_physiology_probe.py) and the Haskell rule
(test-headless Test.Headless.Unit.PhysiologyCirculationParity) must reproduce,
including every clamp, the non-positive body-mass fallback and the Nominal
(no heart rate) branch. Change the Haskell rule and the Haskell side of that
test fails until the fixture is regenerated, which then fails this port.

Humid-heat core-temperature rise
--------------------------------
There is NO Python port of the thermoregulation rule. The expectation comes
from the engine's own pure read, `thermo.readHeatBalance` (scripts/thermo.lua),
which `thermo.tick` itself runs. The probe samples it at its two snapshots and
the window's executed simulated time comes from a ledger of the thermo steps
the engine actually executed (see `LEDGER_LUA` in the probe): the wall-clock
sleep is NOT the simulated duration (the scheduler passes a fixed
scriptTickRate as dt and drops missed passes).

    expected rise = T * (rate_base + rate_final) / 2

with T the executed simulated seconds between the snapshots and `rate` the
balance's unclamped core-temperature rate (degrees C per second) at each
snapshot: the trapezoid rule over the window. The core temperature moves by a
fraction of a degree in the window, so the state-dependent terms (core,
circulation, sweat) drift by well under a percent and the trapezoid is exact to
that order; the engine itself steps explicitly with a fixed dt, so the
discretisation is the engine's, not ours. Stated tolerance: `HEAT_TOL_ABS`
plus `HEAT_TOL_REL` of the expected rise (see their comments).
"""
from __future__ import annotations

import math
import os
from typing import Iterable, Optional

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REGISTRY_PATH = os.path.join(REPO_ROOT, "data", "physiology", "systems.yaml")


def load_registry_constants(path: str = REGISTRY_PATH) -> dict:
    """The registry's `constants:` block (RESTING_HR, THERMO_BASELINE): the
    authority the Haskell rule is handed, never a copy of 70 or 37 here."""
    import yaml
    with open(path, encoding="utf-8") as fh:
        doc = yaml.safe_load(fh)
    return {k: float(v) for k, v in (doc.get("constants") or {}).items()}


def _clamp(lo: float, hi: float, x: float) -> float:
    return max(lo, min(hi, x))


# ---------------------------------------------------------------- circulation

#: Absolute floor of a rolled acolyte's circulation, owner ruling
#: "Per-unit expected values" (2026-11-08): the least the rolled population can
#: reach is 0.75 * (1 - 0.7 * 0.18) = 0.656, so 0.65 sits just under it.
CIRC_FLOOR = 0.65
#: Tolerance of engine-vs-rule agreement. The engine computes in binary32 over
#: stats stored as binary32 and the probe reads them back as doubles; the
#: rule is a short product of five clamped factors, so rounding is a few
#: 1e-7. 1e-4 is two orders of magnitude over that and three under the
#: smallest real disagreement (one factor dropped), so a wrong rule or input
#: fails and rounding never does.
CIRC_TOL = 1e-4


def circulation_rule(resting: float, blood_fraction: float, constitution: float,
                     fat_mass: float, body_mass: float, sepsis: float,
                     core: float, heart_rate: Optional[float]) -> float:
    """Port of `circulationRule resting CirculationInputs{..}`.

    `heart_rate` None is a Nominal cardio: its factor is exactly 1.0 (SD-21).
    """
    blood = _clamp(0.1, 1.0, blood_fraction * 1.1)
    fat_frac = fat_mass / body_mass if body_mass > 0 else 0.2
    fitness = _clamp(0.3, 1.1, (0.5 + 0.5 * constitution)
                     * (1 - 0.7 * max(0.0, fat_frac - 0.20)))
    sickness = _clamp(0.4, 1.0, 1 - 0.5 * sepsis)
    vaso = _clamp(0.45, 1.0, 1 - 0.06 * max(0.0, 36.0 - core))
    hr = 1.0 if heart_rate is None else _clamp(0.5, 1.0, heart_rate / resting)
    return _clamp(0.05, 1.0, blood * fitness * sickness * vaso * hr)


def finite(v) -> bool:
    return isinstance(v, (int, float)) and not isinstance(v, bool) \
        and math.isfinite(v)


def observation_problems(units, expected_ids: Iterable[int]) -> list:
    """The per-unit observation list must be exactly the spawned population:
    a list, one record per spawned id, no duplicates, no strangers, no
    missing unit. (`measure` reports a failed observation as `unitsError`
    and no list; that arrives here as a non-list.)"""
    want = list(expected_ids)
    if not isinstance(units, list):
        return [f"per-unit observation unavailable ({type(units).__name__})"]
    bad = []
    seen, dup = set(), set()
    for rec in units:
        uid = rec.get("id") if isinstance(rec, dict) else None
        if uid is None:
            bad.append("an observation carries no unit id")
            continue
        (dup if uid in seen else seen).add(uid)
    if dup:
        bad.append(f"duplicate observations for unit(s) {sorted(dup)}")
    missing = sorted(set(want) - seen)
    stray = sorted(seen - set(want))
    if missing:
        bad.append(f"missing observation for spawned unit(s) {missing}")
    if stray:
        bad.append(f"observation for unit(s) {stray} that were not spawned")
    if len(want) != len(set(want)):
        bad.append(f"spawned id list has duplicates {sorted(want)}")
    return bad


def _stat(rec: dict, key: str, fallback: float):
    """An effective stat with the rule's fallback when the (valid) stats
    container simply lacks it, or None for a present value that is not a
    finite number (the observation marks those)."""
    stats = rec.get("stats")
    stats = stats if isinstance(stats, dict) else {}
    if key not in stats or stats[key] is None:
        return fallback
    v = stats[key]
    return v if finite(v) else None


#: The mode words the observation reports (`unit.getPhysioMode`, nil -> 'none').
MODE_WORDS = ("simulated", "nominal", "none")


def circulation_inputs(rec: dict, constants: dict) -> tuple:
    """(inputs dict, problems) for one unit record: the arguments the Haskell
    `circulationFor` gathers, read from the observation of that same unit.

    The observation's containers and mode words are validated FIRST. The rule's
    own fallbacks (an absent constitution reads 1.0, fat 0, body mass 70, sepsis
    0; a Nominal or Unsupported thermo reads the baseline; a Nominal cardio has
    no heart-rate factor) apply only to what a complete observation can
    legitimately lack, never to a record that is itself incomplete: a record with
    no `stats`, no `modes`, an unknown mode word, an unreadable core under a
    Simulated thermo or an unreadable heart rate under a Simulated cardio is
    refused rather than defaulted to a healthy unit."""
    bad = []
    modes = rec.get("modes")
    stats = rec.get("stats")
    if not isinstance(modes, dict):
        bad.append("observation carries no modes")
        modes = {}
    if not isinstance(stats, dict):
        bad.append("observation carries no stats")
        stats = {}
    # Both mode keys are required even when the map is empty: a missing mode
    # must never silently select the Nominal (no heart-rate factor) branch.
    for system in ("cardio", "thermo"):
        word = modes.get(system)
        if word not in MODE_WORDS:
            bad.append(f"mode of {system} is {word!r}, not one of {MODE_WORDS}")
    inputs = {
        "blood_fraction": rec.get("bloodFraction"),
        "constitution": _stat(rec, "constitution", 1.0),
        "fat_mass": _stat(rec, "fat_mass", 0.0),
        "body_mass": _stat(rec, "body_mass", 70.0),
        "sepsis": _stat(rec, "sepsis", 0.0),
        "core": rec.get("core"),
        "heart_rate": None,
    }
    if modes.get("cardio") == "simulated":
        inputs["heart_rate"] = rec.get("heartRate")
        if not finite(inputs["heart_rate"]):
            bad.append(f"heart rate {inputs['heart_rate']!r} is not finite "
                       "under a Simulated cardio")
    if modes.get("thermo") in ("nominal", "none") and inputs["core"] is None:
        inputs["core"] = constants["THERMO_BASELINE"]
    for k, v in inputs.items():
        if k == "heart_rate" and modes.get("cardio") != "simulated":
            continue
        if not finite(v):
            bad.append(f"input {k} {v!r} is not a finite number")
    return inputs, bad


def expected_circulation(rec: dict, constants: Optional[dict] = None) -> tuple:
    """(expected circulation or None, problems) for one unit record."""
    constants = constants or load_registry_constants()
    inputs, bad = circulation_inputs(rec, constants)
    if bad:
        return None, bad
    return circulation_rule(constants["RESTING_HR"], **inputs), []


def circulation_problems(units, expected_ids: Iterable[int],
                         constants: Optional[dict] = None,
                         floor: float = CIRC_FLOOR,
                         tol: float = CIRC_TOL) -> list:
    """Judge every spawned unit's circulation: it must match the rule's value
    from its own inputs within `tol`, and be at least `floor`."""
    constants = constants or load_registry_constants()
    bad = observation_problems(units, expected_ids)
    if not isinstance(units, list):
        return bad
    for rec in units:
        if not isinstance(rec, dict) or rec.get("id") is None:
            continue
        uid = rec["id"]
        circ = rec.get("circ")
        if not finite(circ):
            bad.append(f"unit {uid}: circulation reads {circ!r}")
            continue
        want, why = expected_circulation(rec, constants)
        if want is None:
            bad.append(f"unit {uid}: cannot form the expected circulation: "
                       + "; ".join(why))
        elif abs(circ - want) > tol:
            bad.append(f"unit {uid}: circulation {circ:.5f} differs from the "
                       f"rule's {want:.5f} for its own inputs (tol {tol:g})")
        if circ < floor:
            bad.append(f"unit {uid}: circulation {circ:.4f} under the "
                       f"absolute floor {floor}")
    return bad


# ----------------------------------------------------------------- humid heat

#: A unit "heats" when its own core rose by at least this much (degrees C) or
#: its hyperthermia meter is above zero. The lowest rise recorded over the 24
#: #2944 runs (120 units) was 0.060 and the float32 resolution of the stored
#: core is 4e-6, so 0.02 rejects "did not heat" without ever bounding the
#: population from below by a fixed fraction of the mean.
HEAT_MIN_RISE = 0.02
#: Tolerance of observed vs expected rise: HEAT_TOL_ABS plus HEAT_TOL_REL of
#: the expected rise. Calibrated on the first complete run (5 humid-heat units,
#: 303 executed steps of 0.1 s): observed minus expected was within 1.9e-4 C
#: and 0.2 % of the rise at every unit, so this is a >= 10x margin on that,
#: while a unit whose rule inputs were wrong by 10 % (a 0.007-0.017 C error on
#: the 0.07-0.17 C rises seen) still fails. The post-change batch's measured
#: spread is in docs/archive/physiology_probe_per_unit_2960.md.
HEAT_TOL_ABS = 0.002
HEAT_TOL_REL = 0.02
HEAT_TOLERANCE_NOTE = (
    "tolerance = HEAT_TOL_ABS + HEAT_TOL_REL * |expected rise|: the absolute "
    "term covers binary32 storage of the core temperature (4e-6 per step, "
    "well under 1e-3 over a 30 s window); the relative term covers the "
    "trapezoid over the window (state-dependent terms drift by < 1%).")


def window_seconds(base: dict, final: dict) -> Optional[float]:
    """Executed simulated seconds between a unit's two snapshots, from the
    engine-side ledger of thermo steps (None when either is absent)."""
    b, f = base.get("ledger"), final.get("ledger")
    if not isinstance(b, dict) or not isinstance(f, dict):
        return None
    if not (finite(b.get("t")) and finite(f.get("t"))):
        return None
    return f["t"] - b["t"]


def predicted_rise(base: dict, final: dict) -> tuple:
    """(expected core rise or None, problems): the trapezoid over the executed
    window of the engine's own heat-balance rate at the two snapshots."""
    bh, fh = base.get("heat"), final.get("heat")
    if not isinstance(bh, dict) or not isinstance(fh, dict):
        return None, ["no heat-balance read at one of the snapshots"]
    bad = []
    if not (finite(bh.get("rate")) and finite(fh.get("rate"))):
        bad.append(f"heat-balance rate not finite ({bh.get('rate')!r}, "
                   f"{fh.get('rate')!r})")
    secs = window_seconds(base, final)
    if secs is None or not finite(secs):
        bad.append("no executed-step ledger at one of the snapshots")
    elif secs <= 0:
        bad.append(f"no thermo step executed between the snapshots ({secs!r} s)")
    if bad:
        return None, bad
    return secs * (bh["rate"] + fh["rate"]) / 2.0, []


def heat_tolerance(expected: float) -> float:
    return HEAT_TOL_ABS + HEAT_TOL_REL * abs(expected)


def humid_heat_problems(base_units, final_units, expected_ids: Iterable[int],
                        min_rise: float = HEAT_MIN_RISE) -> list:
    """Judge every spawned unit's core rise over the window against the rise
    the engine's heat-balance rule predicts from its own recorded state, and
    require that unit itself to heat (rise or hyperthermia). One unit's
    hyperthermia or agreement never excuses another's failure."""
    ids = list(expected_ids)
    bad = [f"baseline: {p}" for p in observation_problems(base_units, ids)]
    bad += [f"final: {p}" for p in observation_problems(final_units, ids)]
    if not isinstance(base_units, list) or not isinstance(final_units, list):
        return bad
    base_by = {r.get("id"): r for r in base_units if isinstance(r, dict)}
    for rec in final_units:
        if not isinstance(rec, dict) or rec.get("id") not in base_by:
            continue
        uid, base = rec["id"], base_by[rec["id"]]
        c0, c1 = base.get("core"), rec.get("core")
        if not (finite(c0) and finite(c1)):
            bad.append(f"unit {uid}: core temperature unreadable "
                       f"({c0!r} -> {c1!r})")
            continue
        rise = c1 - c0
        want, why = predicted_rise(base, rec)
        if want is None:
            bad.append(f"unit {uid}: no expected rise: " + "; ".join(why))
            continue
        tol = heat_tolerance(want)
        if abs(rise - want) > tol:
            bad.append(f"unit {uid}: core rose {rise:+.4f}C but the heat "
                       f"balance predicts {want:+.4f}C (tol {tol:.4f})")
        hyper = (rec.get("stats") or {}).get("hyperthermia")
        hyperthermic = finite(hyper) and hyper > 0
        if not (rise >= min_rise or hyperthermic):
            bad.append(f"unit {uid}: not heating (core {rise:+.4f}C, "
                       f"hyperthermia {hyper!r})")
    return bad
