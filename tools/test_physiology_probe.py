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
import physiology_probe as probe  # noqa: E402

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

    if failures:
        print("physiology probe self-test: FAILED")
        for failure in failures:
            print(f"  - {failure}")
        return 1
    print("physiology probe self-test: all cases pass")
    return 0


if __name__ == "__main__":
    sys.exit(main())
