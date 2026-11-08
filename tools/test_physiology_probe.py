#!/usr/bin/env python3
"""Engine-free self-test of the physiology probe's squirrel predicate (#2833).

`tools/physiology_probe.py` is manual-only: its real acceptance is a long
engine run. What must not regress silently is the cheap half: the predicate
that decides whether the SHIPPED red_squirrel is the `simple` profile of
owner decision SD-10 (hydration, salt and nutrition Nominal, thermo
Simulated). It boots nothing: the predicate is a pure function over one
observation, and the console wrapper is driven over a fake `send`.

What is pinned, because each is a way the squirrel could silently stay
wrong or a probe could pass vacuously:

  * the healthy control passes;
  * a squirrel left on the old `reduced` profile (salt Simulated, the salt
    pool and store stored, a live salt_conc) FAILS, each way it differs:
    the wrong salt mode, leaked salt keys, and a salt_conc that is a real
    simulated value rather than the declared healthy 1.0;
  * a missing, non-finite or zero salt_conc fails: an absent Nominal key
    must not be read as 0 (F-9 of the design: the old aggregate's
    `unit.getStat(u,s) or 0`);
  * Nominal state that reappears in storage (a pool, the derived value, the
    imbalance meter, or hydration/nutrition state) fails;
  * hydration or nutrition Simulated, or thermo Nominal, fails;
  * a probe-written override fails (the expected default must come from the
    species definition, never from a pin);
  * a non-zero salt_imbalance or a cramp multiplier below 1 fails;
  * the console wrapper judges an unreadable reply as a failure, and the
    observation chunk names exactly the read verbs the access audit allows
    in the probe (no raw registry-key read).

Per-unit expected values (#2960) are pinned too, with no engine:

  * the Python port of the circulation rule reproduces the shared parity
    fixture (the Haskell rule is held to the same file by
    Test.Headless.Unit.PhysiologyCardioCirculationModes.Rule) and 600 real
    engine readings from the 24 #2944 runs, 120 of them the temperate
    acolytes, including the edge unit that failed `circ min >= 0.75`;
  * a unit that is off its prediction fails, a unit under the 0.65 floor
    fails even when it matches the rule, and a unit at the old edge
    (constitution 0.5) passes;
  * a missing, duplicate, unexpected, unreadable or non-finite observation
    fails, and so does a missing executed-step ledger;
  * each humid-heat unit must agree with the heat-balance rule AND heat on its
    own: another unit's agreement or hyperthermia never excuses it.

Usage:
  python3 tools/test_physiology_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import copy
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import physiology_expectations as pe  # noqa: E402
import physiology_probe as probe  # noqa: E402

FIXTURES = os.path.join(os.path.dirname(os.path.abspath(__file__)), "fixtures")

#: A shipped `simple` squirrel as the observation chunk reports it.
HEALTHY = {
    "modes": {"thermo": "simulated", "salt": "nominal",
              "hydration": "nominal", "nutrition": "nominal"},
    "pins": {"thermo": "none", "salt": "none", "hydration": "none",
             "nutrition": "none"},
    "stored": {k: None for k in probe.SQUIRREL_ABSENT_KEYS},
    "conc": 1.0, "concMode": "nominal", "imbalance": 0.0, "speed": 1.0,
    "core": 37.0, "coreStored": 37.0,
}

failures = []


def expect(condition, message):
    if not condition:
        failures.append(message)


def changed(**edits):
    """HEALTHY with dotted-path edits, e.g. changed(**{"modes.salt": "simulated"})."""
    out = copy.deepcopy(HEALTHY)
    for path, value in edits.items():
        node = out
        parts = path.split(".")
        for part in parts[:-1]:
            node = node[part]
        node[parts[-1]] = value
    return out


def fails(reading, needle, why):
    problems = probe.squirrel_nominal_problems(reading)
    expect(any(needle in p for p in problems),
           f"{why}: wanted a problem naming {needle!r}, got {problems}")


def main():
    # 1. The control.
    expect(probe.squirrel_nominal_problems(HEALTHY) == [],
           "the healthy simple squirrel must pass: "
           f"{probe.squirrel_nominal_problems(HEALTHY)}")

    # 2. A squirrel accidentally left `reduced`: salt Simulated, the pool and
    #    store stored, a simulated concentration, a salt state that can move.
    reduced = changed(**{"modes.salt": "simulated", "concMode": "simulated",
                         "conc": 0.93,
                         "stored.salt": 0.28, "stored.salt_store": 0.55})
    problems = probe.squirrel_nominal_problems(reduced)
    expect(problems != [], "a squirrel left reduced must fail")
    for needle in ("salt resolves 'simulated'", "Nominal state is stored",
                   "not its healthy value 1.0", "read mode 'simulated'"):
        expect(any(needle in p for p in problems),
               f"a reduced squirrel's failure must say {needle!r}: {problems}")
    # each difference alone is enough
    fails(changed(**{"modes.salt": "simulated"}), "salt resolves", "wrong salt mode alone")
    fails(changed(**{"stored.salt": 0.3}), "salt", "a stored salt pool alone")
    fails(changed(**{"stored.salt_store": 0.6}), "salt_store", "a stored salt store alone")
    fails(changed(conc=0.93), "healthy value", "a non-healthy concentration alone")

    # 3. A missing, non-finite or zero salt_conc, never read as healthy.
    fails(changed(conc=None), "not a finite number", "a missing concentration")
    fails(changed(conc="nonfinite"), "not a finite number", "a non-finite concentration")
    fails(changed(conc=float("nan")), "not a finite number", "a NaN concentration")
    fails(changed(conc=float("inf")), "not a finite number", "an infinite concentration")
    fails(changed(conc=True), "not a finite number", "a boolean concentration")
    fails(changed(conc=0), "zero stands in", "a zero substituted for the absent key")
    fails(changed(conc=0.0), "zero stands in", "a float zero substituted for the absent key")
    fails(changed(concMode=None), "read mode", "a concentration read with no mode")

    # 4. Nominal state that reappears in storage.
    for key in probe.SQUIRREL_ABSENT_KEYS:
        fails(changed(**{f"stored.{key}": 1.0}), key, f"leaked {key}")
    fails(changed(**{"stored.salt_imbalance": 0.0}), "salt_imbalance",
          "a stored zero is still stored state, not absence")

    # 5. The other systems.
    fails(changed(**{"modes.hydration": "simulated"}), "hydration resolves", "hydration Simulated")
    fails(changed(**{"modes.nutrition": "simulated"}), "nutrition resolves", "nutrition Simulated")
    fails(changed(**{"modes.thermo": "nominal"}), "thermo resolves", "thermo Nominal")
    fails(changed(**{"modes.thermo": "none"}), "thermo resolves", "thermo Unsupported")
    fails(changed(core=None), "thermo is not Simulated state", "no core temperature")
    fails(changed(coreStored=None), "thermo is not Simulated state", "no stored core temperature")

    # 6. The default must be the species', not a pin.
    fails(changed(**{"pins.salt": "nominal"}), "override", "a probe-written salt pin")
    fails(changed(**{"pins.hydration": "nominal"}), "override", "a hydration pin")

    # 7. No failure contribution, no cramps.
    fails(changed(imbalance=0.2), "salt_imbalance reads", "a non-zero imbalance")
    fails(changed(imbalance=None), "salt_imbalance reads", "an unreadable imbalance")
    fails(changed(speed=0.8), "cramp multiplier", "cramps")
    fails(changed(speed=None), "cramp multiplier", "an unreadable cramp multiplier")

    # 8. Shape: not a dict, or a field missing altogether.
    expect(probe.squirrel_nominal_problems(None) == ["unreadable observation"],
           "no observation is a failure")
    expect(probe.squirrel_nominal_problems({}) != [], "an empty observation fails")

    # 9. The console wrapper, over a fake `send`.
    def fake(reply):
        seen = []

        def send(port, lua):
            seen.append((port, lua))
            return reply
        return send, seen

    send, seen = fake(json.dumps(HEALTHY))
    expect(probe.check_squirrel_nominal(9170, 7, send_fn=send) == [],
           "the wrapper passes the healthy reply")
    expect(seen and "local u=7" in seen[0][1] and seen[0][0] == 9170,
           "the wrapper observes the unit it was given, on the probe's port")
    send, _ = fake(json.dumps(reduced))
    expect(probe.check_squirrel_nominal(9170, 7, send_fn=send) != [],
           "the wrapper fails a reduced squirrel")
    for junk in ("", "nil", "{not json", None):
        send, _ = fake(junk)
        got = probe.check_squirrel_nominal(9170, 7, send_fn=send)
        expect(got and "unreadable observation" in got[0],
               f"an unreadable reply {junk!r} is a failure, got {got}")

    # 10. The chunk itself: read verbs only (the access audit refuses a raw
    #     registry-key read in the probe), and it covers every key the
    #     predicate judges.
    chunk = probe.squirrel_observation_lua(3)
    for verb in ("unit.getPhysioMode", "unit.getPhysioOverride",
                 "unit.readPhysio(u,'salt_conc')", "unit.getAllStats"):
        expect(verb in chunk, f"the observation uses {verb}")
    for raw in ("unit.getStat(", "unit.getStatBase(", "unit.setStat("):
        expect(raw not in chunk, f"the observation makes no raw call {raw}")
    for key in probe.SQUIRREL_ABSENT_KEYS:
        expect(f"'{key}'" in chunk, f"the observation reads {key}")
    expect(set(probe.SQUIRREL_MODES) == set(probe.SYSTEMS),
           "the expected modes cover every system")
    expect(probe.SQUIRREL_MODES == {"thermo": "simulated", "salt": "nominal",
                                    "hydration": "nominal",
                                    "nutrition": "nominal"},
           "the expected modes are SD-10's simple profile")

    # 11. The per-unit diagnostics (#2944) observe only: one console call
    #     carries the unchanged aggregate and the unit list, the chunk uses
    #     read verbs only, and the extra key never reaches an assertion.
    sent = []
    agg = {"n": 1, "dead": 0, "core": {"min": 37, "max": 37, "avg": 37},
           "conc": {"min": 1, "max": 1, "avg": 1},
           "circ": {"min": 0.9, "max": 0.9, "avg": 0.9}}
    reply = dict(agg, units=[{"id": 4, "circ": 0.9}])

    def fake_send(port, lua):
        sent.append(lua)
        return json.dumps(agg if lua == probe.AGG else reply)

    real_send = probe.send
    probe.send = fake_send
    try:
        plain = probe.measure(9170)
        diag = probe.measure(9170, units=True, species="acolyte")
    finally:
        probe.send = real_send
    expect(sent[0] == probe.AGG, "measure() without units sends exactly AGG")
    expect(probe.AGG in sent[1] and probe.DIAG in sent[1],
           "measure(units=True) sends AGG and DIAG in ONE console call")
    expect(diag.get("units") == reply["units"] and diag["species"] == "acolyte",
           "the unit list and species come back with the aggregate")
    expect("units" not in plain, "the plain aggregate carries no unit list")
    expect(probe.temperate_ok(diag, [4])[1] != probe.temperate_ok(plain, [4])[1],
           "the per-unit key now feeds the temperate circulation check "
           "(#2960): a plain aggregate has no observation to judge")
    for verb in ("unit.getPhysioMode", "unit.readPhysio(u,'circulation')",
                 "unit.readBloodView", "unit.getAllStats"):
        expect(verb in probe.DIAG, f"the diagnostics use {verb}")
    for raw in ("unit.getStat(", "unit.getStatBase(", "unit.setStat(",
                "unit.spawn(", "debugAmbient"):
        expect(raw not in probe.DIAG, f"the diagnostics make no call {raw}")

    # 12. The squirrel section emits a baseline AND a final per-unit record for
    #     its three squirrels (#2944), and the optional sink gets every record,
    #     all without changing its checks (it still passes on a healthy fake).
    import contextlib
    import io
    import tempfile
    squirrel_units = [{"id": i, "circ": 1.0, "conc": 1.0, "concMode": "nominal",
                       "modes": {"salt": "nominal"}, "stats": {"body_mass": 0.25}}
                      for i in (11, 12, 13)]
    spawned = iter([11, 12, 13, 21, 22, 23, 31, 32, 33])
    sq_agg = {"n": 3, "dead": 0, "core": {"min": 36.5, "max": 36.9, "avg": 36.7},
              "conc": {"min": 1.0, "max": 1.0, "avg": 1.0},
              "circ": {"min": 1.0, "max": 1.0, "avg": 1.0}}

    def sq_send(port, lua):
        if "unit.spawn" in lua:
            return str(next(spawned))
        if lua.startswith("_U="):
            return "3"
        if "body_mass" in lua and "string.format" in lua:
            return '"0.250000,0.050000,0.150000"'
        if "getPhysioOverride" in lua:
            return json.dumps(HEALTHY)
        if "pcall(function()" in lua and "unit.readBloodView" in lua:
            return json.dumps(dict(sq_agg, units=squirrel_units))
        return json.dumps(sq_agg)

    real_send, real_sleep = probe.send, probe.time.sleep
    probe.send, probe.time.sleep = sq_send, lambda _s: None
    sink = tempfile.NamedTemporaryFile(suffix=".jsonl", delete=False)
    sink.close()
    os.environ[probe.DIAG_SINK_ENV] = sink.name
    out = io.StringIO()
    try:
        with contextlib.redirect_stdout(out):
            probe.small_creature_section(9170, 30.0)
    finally:
        probe.send, probe.time.sleep = real_send, real_sleep
        del os.environ[probe.DIAG_SINK_ENV]
    printed = [json.loads(ln.split("[DIAG] ", 1)[1])
               for ln in out.getvalue().splitlines() if "[DIAG]" in ln]
    sunk = [json.loads(ln) for ln in open(sink.name).read().splitlines()]
    os.unlink(sink.name)
    got = [(r["scenario"], r["phase"], r["species"], len(r["units"] or []))
           for r in printed if r["scenario"] == "small creature temperate"]
    expect(got == [("small creature temperate", "baseline", "red_squirrel", 3),
                   ("small creature temperate", "final", "red_squirrel", 3)],
           f"the squirrel section emits a 3-unit baseline then final record: {got}")
    expect(sunk == printed, "the sink receives exactly the printed records")
    expect("[PASS] small creature temperate" in out.getvalue()
           and "[PASS] squirrel simple profile" in out.getvalue(),
           "the squirrel section still judges a healthy fake as before "
           "(its cold-comparison check is not satisfiable by this fake)")

    circulation_cases()
    heat_cases()
    probe_wiring_cases()

    if failures:
        print("physiology probe self-test: FAILED")
        for failure in failures:
            print(f"  - {failure}")
        return 1
    print("physiology probe self-test: all cases pass")
    return 0


# ---------------------------------------------------------------- #2960

def load_fixture(name):
    with open(os.path.join(FIXTURES, name), encoding="utf-8") as fh:
        return json.load(fh)


CONSTANTS = pe.load_registry_constants()


def recorded_unit(constitution=1.0, fat_frac=0.2, body=70.0, sepsis=0.0,
                  core=37.0, hr=70.0, blood=1.0, uid=1, circ=None, cardio="simulated"):
    """A unit record in the probe's DIAG shape, with `circ` defaulting to the
    rule's own answer for its inputs (an agreeing engine)."""
    rec = {"id": uid, "modes": {"cardio": cardio, "thermo": "simulated"},
           "stats": {"constitution": constitution, "fat_mass": fat_frac * body,
                     "body_mass": body, "sepsis": sepsis},
           "core": core, "heartRate": hr, "bloodFraction": blood}
    want, _ = pe.expected_circulation(rec, CONSTANTS)
    rec["circ"] = want if circ is None else circ
    return rec


def circulation_cases():
    # 1. The port reproduces the shared parity fixture (the Haskell rule is held
    #    to the same file).
    parity = load_fixture("physiology_circulation_parity.json")["cases"]
    expect(len(parity) >= 25, f"the parity fixture is thin: {len(parity)} cases")
    for c in parity:
        got = pe.circulation_rule(c["resting"], c["bloodFraction"],
                                  c["constitution"], c["fatMass"], c["bodyMass"],
                                  c["sepsis"], c["core"], c["heartRate"])
        expect(abs(got - c["expected"]) < 1e-9,
               f"port vs parity fixture, {c['name']!r}: {got} vs {c['expected']}")
    expect(CONSTANTS["RESTING_HR"] == parity[0]["resting"],
           "the fixture rests at the registry's RESTING_HR")
    named = {c["name"]: c["expected"] for c in parity}
    expect(abs(named["old edge: constitution 0.5, fat 0.2"] - 0.75) < 1e-12,
           "the old population edge is 0.75")
    expect(abs(named["population floor: constitution 0.5, fat 0.38"] - 0.6555) < 1e-4,
           "the population floor is 0.656")
    expect(named["everything bad hits the 0.05 floor"] == 0.05,
           "the rule's own lower clamp is 0.05")

    # 2. It reproduces what the real engine read for 600 recorded units, with a
    #    margin of at least three orders under the tolerance.
    rec = load_fixture("physiology_circulation_recordings.json")["units"]
    worst = 0.0
    for u in rec:
        want, why = pe.expected_circulation(u, CONSTANTS)
        expect(want is not None, f"recorded unit unreadable: {why}")
        if want is not None:
            worst = max(worst, abs(want - u["circ"]))
    expect(worst < pe.CIRC_TOL / 100,
           f"the port is {worst:.2e} from the engine on recorded units; "
           f"tolerance {pe.CIRC_TOL} must keep a 100x margin")
    temperate = [u for u in rec if u["scenario"].startswith("temperate")]
    expect(len(temperate) == 120, f"{len(temperate)} temperate recordings, wanted 120")

    # 3. Every recorded temperate unit passes on its own, including the unit
    #    that failed the old circ-min-0.75 check: it is just the rule's value.
    runs = {}
    for u in temperate:
        runs.setdefault((u["batch"], u["run"]), []).append(u)
    expect(len(runs) == 24 and all(len(v) == 5 for v in runs.values()),
           "24 recorded temperate runs of five acolytes")
    for key, units in sorted(runs.items(), key=str):
        got = pe.circulation_problems(units, [u["id"] for u in units], CONSTANTS)
        expect(got == [], f"recorded temperate run {key} must pass: {got}")
    edge = [u for u in temperate if u["circ"] < 0.75]
    expect(len(edge) >= 1, "the recordings include a unit under the old 0.75 edge")
    for u in edge:
        expect(pe.circulation_problems([u], [u["id"]], CONSTANTS) == [],
               f"unit under 0.75 (circ {u['circ']:.4f}) is the rule's value")
    expect(min(u["circ"] for u in temperate) >= pe.CIRC_FLOOR,
           "every recorded temperate unit is over the 0.65 floor")

    # 4. A unit that disagrees with its own inputs fails; so does each way of
    #    dropping or adding a factor.
    ok = recorded_unit(constitution=0.9, fat_frac=0.3, core=34.0, hr=60.0, blood=0.9)
    expect(pe.circulation_problems([ok], [1], CONSTANTS) == [], "an agreeing unit passes")
    for label, bad in (("too high", ok["circ"] + 0.01), ("too low", ok["circ"] - 0.01),
                       ("a factor dropped", ok["circ"] / 0.88)):
        got = pe.circulation_problems([dict(ok, circ=bad)], [1], CONSTANTS)
        expect(any("differs from the rule's" in p for p in got),
               f"a unit off its prediction ({label}) must fail: {got}")
    # An engine value just inside tolerance passes; just outside fails.
    inside = dict(ok, circ=ok["circ"] + pe.CIRC_TOL * 0.9)
    outside = dict(ok, circ=ok["circ"] + pe.CIRC_TOL * 1.2)
    expect(pe.circulation_problems([inside], [1], CONSTANTS) == [], "inside tolerance passes")
    expect(pe.circulation_problems([outside], [1], CONSTANTS) != [], "outside tolerance fails")

    # 5. The absolute floor: a unit that matches its rule but sits under 0.65
    #    still fails; the population's lowest (constitution 0.5, fat 0.38) and
    #    the old edge (constitution 0.5, fat 0.2) pass.
    low = recorded_unit(constitution=0.5, fat_frac=0.5)
    expect(low["circ"] < 0.65, f"fixture is under the floor ({low['circ']})")
    got = pe.circulation_problems([low], [1], CONSTANTS)
    expect(got and all("absolute floor" in p for p in got),
           f"a unit under 0.65 fails on the floor alone: {got}")
    edge_unit = recorded_unit(constitution=0.5, fat_frac=0.2)
    expect(abs(edge_unit["circ"] - 0.75) < 1e-9
           and pe.circulation_problems([edge_unit], [1], CONSTANTS) == [],
           "a unit at the old edge (constitution 0.5) passes")
    floor_unit = recorded_unit(constitution=0.5, fat_frac=0.38)
    expect(floor_unit["circ"] > 0.65
           and pe.circulation_problems([floor_unit], [1], CONSTANTS) == [],
           "the rolled population's floor (0.656) passes")

    # 6. Nominal cardio: no heart-rate factor, whatever the stored rate says.
    nominal = recorded_unit(cardio="nominal", hr=20.0)
    expect(pe.circulation_problems([nominal], [1], CONSTANTS) == [],
           "a Nominal cardio ignores the heart rate")
    expect(pe.expected_circulation(nominal, CONSTANTS)[0]
           > pe.expected_circulation(recorded_unit(hr=20.0), CONSTANTS)[0],
           "a Simulated slow heart lowers the expectation")

    # 7. Observation integrity.
    u1, u2, u3 = (recorded_unit(uid=i) for i in (1, 2, 3))
    def says(units, ids, needle, why):
        got = pe.circulation_problems(units, ids, CONSTANTS)
        expect(any(needle in p for p in got), f"{why}: wanted {needle!r}, got {got}")
    expect(pe.circulation_problems([u1, u2, u3], [1, 2, 3], CONSTANTS) == [],
           "a complete observation passes")
    says([u1, u2], [1, 2, 3], "missing observation", "a missing unit")
    says([u1, u2, dict(u2)], [1, 2], "duplicate", "a duplicated unit")
    says([u1, u2, u3], [1, 2], "were not spawned", "an unexpected unit")
    says([u1, u2, recorded_unit(uid=9)], [1, 2, 3], "missing observation",
         "a swapped id")
    says(None, [1], "unavailable", "an unreadable observation (unitsError)")
    says([], [1], "missing observation", "an empty observation")
    says([u1], [], "were not spawned", "no spawned ids")
    says([u1], [1, 1], "duplicates", "duplicate spawned ids")
    says([dict(u1, circ="nonfinite")], [1], "circulation reads", "a non-finite reading")
    says([dict(u1, circ=None)], [1], "circulation reads", "an absent reading")
    says([dict(u1, circ=float("nan"))], [1], "circulation reads", "a NaN reading")
    says([dict(u1, bloodFraction="nonfinite")], [1], "blood_fraction", "a non-finite input")
    says([dict(u1, core=None)], [1], "core", "an unreadable core temperature")
    says([{"circ": 1.0}], [1], "no unit id", "a record with no id")
    # one bad unit among good ones fails the batch, and names the unit
    got = pe.circulation_problems([u1, dict(u2, circ=u2["circ"] - 0.1), u3],
                                  [1, 2, 3], CONSTANTS)
    expect(len(got) == 1 and "unit 2" in got[0],
           f"one off-prediction unit fails the batch, named: {got}")

    # 8. The check function the scenario table calls.
    agg = {"dead": 0, "core": {"min": 36.9, "max": 37.1},
           "conc": {"min": 1.0, "max": 1.0}, "units": [u1, u2, u3]}
    ok_, detail = probe.temperate_check(agg, {}, [1, 2, 3])
    expect(ok_ and detail == "all normal", f"temperate check passes: {detail}")
    bad_units = dict(agg, units=[u1, dict(u2, circ=0.5), u3])
    ok_, detail = probe.temperate_check(bad_units, {}, [1, 2, 3])
    expect(not ok_ and "unit 2" in detail, f"temperate check names the unit: {detail}")
    expect(not probe.temperate_check(dict(agg, units=None), {}, [1, 2, 3])[0],
           "temperate check fails with no per-unit observation")
    expect(not probe.temperate_check(dict(agg, dead=1), {}, [1, 2, 3])[0],
           "the other temperate checks are unchanged (deaths)")
    expect(not probe.temperate_check(dict(agg, core={"min": 30, "max": 37}), {},
                                     [1, 2, 3])[0],
           "the other temperate checks are unchanged (core band)")
    expect(not probe.temperate_check(dict(agg, conc={"min": 0.5, "max": 1.0}), {},
                                     [1, 2, 3])[0],
           "the other temperate checks are unchanged (salt band)")


def heat_unit(uid, core0, core1, rate0, rate1, t0, t1, hyper=0.0):
    """A (baseline, final) pair of per-unit observations: core temperatures, the
    engine heat-balance rate (C/s) at each snapshot and the cumulative
    executed-step seconds."""
    base = {"id": uid, "core": core0, "stats": {"hyperthermia": 0.0},
            "heat": {"rate": rate0}, "ledger": {"n": 10, "t": t0}}
    final = {"id": uid, "core": core1, "stats": {"hyperthermia": hyper},
             "heat": {"rate": rate1}, "ledger": {"n": 160, "t": t1}}
    return base, final


def heat_cases():
    # 120 executed steps of 0.25 s = 30 s; rate 0.0042 C/s -> +0.126 C.
    def pair(uid, rise=0.126, rate0=0.0042, rate1=0.0042, secs=30.0, hyper=0.0):
        return heat_unit(uid, 37.0, 37.0 + rise, rate0, rate1, 2.0, 2.0 + secs, hyper)
    def judge(pairs, ids=None):
        base = [b for b, _ in pairs]
        final = [f for _, f in pairs]
        ids = [b["id"] for b in base] if ids is None else ids
        return pe.humid_heat_problems(base, final, ids)
    good = [pair(i) for i in range(1, 6)]
    expect(judge(good) == [], f"agreeing units pass: {judge(good)}")

    # The expectation is the trapezoid over the executed window.
    want, why = pe.predicted_rise(*pair(1, rate0=0.004, rate1=0.005))
    expect(want is not None and abs(want - 30.0 * 0.0045) < 1e-12,
           f"the expected rise is T * mean(rate): {want} {why}")
    # The executed seconds come from the ledger, NOT from the sleep: half the
    # steps means half the rise, so a unit whose rise matches a 30 s window
    # fails against a 15 s ledger.
    got = judge([pair(1, secs=15.0)])
    expect(any("predicts" in p for p in got),
           f"an unexecuted half-window is not excused by the sleep: {got}")
    expect(judge([pair(1, rise=0.063, secs=15.0)]) == [],
           "a unit that ran half the steps and rose half as much passes")

    # A low-rise unit that matches its own rule passes (the #2944 case): 0.086.
    slow = pair(1, rise=0.086, rate0=0.00287, rate1=0.00287)
    expect(judge([slow]) == [], f"a healthy +0.086 unit passes: {judge([slow])}")

    # Disagreement with the rule fails, in either direction.
    for label, bad in (("hotter", pair(1, rise=0.20)), ("cooler", pair(1, rise=0.06))):
        got = judge([bad])
        expect(any("predicts" in p for p in got), f"{label} than predicted fails: {got}")
    # ... and the tolerance is stated and two-sided.
    tol = pe.heat_tolerance(0.126)
    expect(judge([pair(1, rise=0.126 + 0.9 * tol)]) == [], "inside tolerance passes")
    expect(judge([pair(1, rise=0.126 + 1.2 * tol)]) != [], "outside tolerance fails")

    # One unit's agreement or hyperthermia never excuses another's failure.
    mixed = [pair(1), pair(2, rise=0.30, hyper=0.4), pair(3)]
    got = judge(mixed)
    expect(len(got) == 1 and "unit 2" in got[0],
           f"the disagreeing unit alone fails: {got}")
    # Hyperthermia excuses neither disagreement nor... : agreement + hyper passes.
    hot = pair(1, rise=0.126, hyper=0.4)
    expect(judge([hot]) == [], "an agreeing hyperthermic unit passes")

    # Each unit must itself heat: a prediction of ~0 matched by a flat core fails
    # "not heating" even though it agrees with the rule.
    flat = pair(1, rise=0.001, rate0=0.00003, rate1=0.00003)
    got = judge([flat])
    expect(any("not heating" in p for p in got), f"a unit that does not heat fails: {got}")
    # ... unless it is hyperthermic.
    expect(not any("not heating" in p
                   for p in judge([pair(1, rise=0.001, rate0=0.00003, rate1=0.00003,
                                        hyper=0.2)])),
           "a hyperthermic unit counts as heating")
    # A cooling unit that matches a cooling prediction still fails.
    got = judge([pair(1, rise=-0.1, rate0=-0.0033, rate1=-0.0033)])
    expect(any("not heating" in p for p in got), f"a cooling unit fails: {got}")

    # Observation integrity.
    b1, f1 = pair(1)
    b2, f2 = pair(2)
    def says(base, final, ids, needle, why):
        got = pe.humid_heat_problems(base, final, ids)
        expect(any(needle in p for p in got), f"{why}: wanted {needle!r}, got {got}")
    says([b1], [f1, f2], [1, 2], "baseline: missing", "a missing baseline unit")
    says([b1, b2], [f1], [1, 2], "final: missing", "a missing final unit")
    says([b1, b2], [f1, f2, dict(f2)], [1, 2], "duplicate", "a duplicated final unit")
    says([b1, b2], [f1, f2], [1], "were not spawned", "an unexpected unit")
    says(None, [f1], [1], "unavailable", "an unreadable baseline")
    says([b1], None, [1], "unavailable", "an unreadable final")
    says([b1], [f1], [1, 2], "missing observation", "fewer units than --count")
    says([b1], [dict(f1, heat=None)], [1], "no heat-balance read", "no heat read")
    says([b1], [dict(f1, ledger=None)], [1], "ledger", "no ledger")
    says([dict(b1, ledger=None)], [f1], [1], "ledger", "no baseline ledger")
    says([b1], [dict(f1, ledger={"t": b1["ledger"]["t"]})], [1], "no thermo step executed",
         "a window with no executed step")
    says([b1], [dict(f1, ledger={"t": b1["ledger"]["t"] - 1.0})], [1],
         "no thermo step executed", "a ledger that went backwards")
    says([b1], [dict(f1, heat={"rate": "nonfinite"})], [1], "not finite", "a non-finite rate")
    says([b1], [dict(f1, heat={"rate": None})], [1], "not finite", "an absent rate")
    says([dict(b1, core=None)], [f1], [1], "core temperature unreadable", "no baseline core")
    says([b1], [dict(f1, core="nonfinite")], [1], "core temperature unreadable",
         "a non-finite core")
    says([b1], [dict(f1, core=float("nan"))], [1], "core temperature unreadable", "NaN core")

    # The scenario-table check.
    m = {"hyper": 0.0, "units": [f for _, f in good]}
    base = {"units": [b for b, _ in good]}
    ok_, detail = probe.humid_heat_check(m, base, [1, 2, 3, 4, 5])
    expect(ok_ and "each unit matches the heat-balance rule" in detail,
           f"humid heat check passes: {detail}")
    ok_, detail = probe.humid_heat_check(
        {"hyper": 0.0, "units": [f for _, f in good[:4]]}, base, [1, 2, 3, 4, 5])
    expect(not ok_ and "missing" in detail, f"a missing unit fails the check: {detail}")
    ok_, detail = probe.humid_heat_check({"hyper": 0.0}, {}, [1])
    expect(not ok_ and "unavailable" in detail, f"no per-unit data fails: {detail}")
    # No aggregate hyperthermia can excuse a unit (the old rule's `or hyper > 0`).
    ok_, _ = probe.humid_heat_check(
        {"hyper": 0.9, "units": [f for _, f in [pair(1, rise=0.30)]]},
        {"units": [pair(1)[0]]}, [1])
    expect(not ok_, "aggregate hyperthermia no longer excuses a disagreeing unit")


def probe_wiring_cases():
    # The observation chunk reads the pure verb and the ledger, and makes no
    # raw call the access audit refuses.
    for needle in ("th.readHeatBalance", "th.debugLedger", "heat=heat", "ledger=ledger"):
        expect(needle in probe.DIAG, f"the diagnostics carry {needle}")
    for raw in ("unit.getStat(", "unit.setStat(", "th.tick", "thermo.tick"):
        expect(raw not in probe.DIAG, f"the diagnostics never call {raw}")
    # The ledger wrapper forwards the call and its result and only counts.
    led = probe.LEDGER_LUA
    for needle in ("t.tick=function(uid,info,dt)", "orig(uid,info,dt)", "return s",
                   "s.dt", "s.delta", "s.applied", "debugLedger"):
        expect(needle in led, f"the ledger wrapper has {needle!r}")
    for raw in ("unit.setStat", "debugAmbient", "unit.spawn"):
        expect(raw not in led, f"the ledger wrapper makes no call to {raw}")
    expect(led.count("end") >= 4, "the ledger chunk is closed")
    # bootstrap installs it, once, after the scripts it wraps are loaded.
    sent = []
    real_send = probe.send
    probe.send = lambda port, lua: (sent.append(lua), "ok")[1]
    try:
        probe.bootstrap(9170)
    finally:
        probe.send = real_send
    expect(sent.count(probe.LEDGER_LUA) == 1, "bootstrap installs the ledger exactly once")
    expect(sent.index(probe.LEDGER_LUA)
           > max(i for i, c in enumerate(sent) if "unit_resources.lua" in c),
           "the ledger is installed after unit_resources is loaded")
    # Every scenario check is called the way main() calls it: (m, base, spawned).
    # (The first smoke run of #2960 died on a check that took two arguments.)
    import inspect
    for name, _a, _h, check in probe.SCENARIOS:
        params = list(inspect.signature(check).parameters)
        expect(len(params) == 3, f"{name}: check takes (m, base, spawned), got {params}")
    # The scenario table still names the same stimuli (the issue's scope).
    expect([(n, a, h) for n, a, h, _ in probe.SCENARIOS] == [
        ("temperate (22C/0.5)", 22, 0.5), ("mild cold (5C/0.5)", 5, 0.5),
        ("deep arctic (-55C/0.6)", -55, 0.6), ("dry heat (45C/0.15)", 45, 0.15),
        ("humid heat (46C/0.95)", 46, 0.95)],
        "no scenario climate changed")


if __name__ == "__main__":
    sys.exit(main())
