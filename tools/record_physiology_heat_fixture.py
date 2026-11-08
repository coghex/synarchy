#!/usr/bin/env python3
"""Record real humid-heat observations for tools/test_physiology_probe.py (#2960).

Boots one headless engine through the physiology probe's own bootstrap and
runs ONLY the probe's humid-heat scenario, `--repeats` times: same climate
(46 C / 0.95), same `spawn_batch`, same 1 s settle, same `--seconds` window and
the same per-unit observation (`measure(units=True)`, which carries the
heat-balance read and the executed-step ledger). Each repeat spawns a fresh set
of acolytes, so every record is an independent rolled unit. The trimmed
baseline/final pairs are written to tools/fixtures/ with the revision and
command that produced them, and the engine-free self-test then holds the
probe's judgement to them (prediction agreement and its stated error bound).

Usage:
  python3 tools/record_physiology_heat_fixture.py --port 9173 --repeats 12 \\
      [--seconds 30] [--count 5] [--out tools/fixtures/physiology_humid_heat_recordings.json]
"""
from __future__ import annotations
import argparse, json, os, subprocess, sys, time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import physiology_probe as probe  # noqa: E402
from probelib import boot, quit_engine  # noqa: E402

DEFAULT_OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "fixtures", "physiology_humid_heat_recordings.json")


def trim(rec):
    """The fields the humid-heat judgement reads, nothing else."""
    return {"id": rec["id"], "core": rec["core"],
            "hyperthermia": (rec.get("stats") or {}).get("hyperthermia"),
            "heat": rec["heat"], "ledger": rec["ledger"]}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9173)
    ap.add_argument("--repeats", type=int, default=12)
    ap.add_argument("--seconds", type=float, default=30.0)
    ap.add_argument("--count", type=int, default=5)
    ap.add_argument("--out", default=DEFAULT_OUT)
    args = ap.parse_args()
    name, ambient, humidity, _check = next(
        sc for sc in probe.SCENARIOS if sc[0].startswith("humid heat"))
    revision = subprocess.run(["git", "rev-parse", "HEAD"], text=True,
                              capture_output=True).stdout.strip()
    proc = boot(args.port, log="/tmp/physiology_heat_fixture_engine.log")
    runs = []
    try:
        probe.bootstrap(args.port)
        probe.set_climate(args.port, ambient, humidity)
        for i in range(args.repeats):
            spawned = probe.spawn_batch(args.port, args.count)
            time.sleep(1.0)
            base = probe.measure(args.port, units=True, species="acolyte")
            time.sleep(args.seconds)
            final = probe.measure(args.port, units=True, species="acolyte")
            problems = [p for p in (base, final)
                        if not isinstance(p.get("units"), list)]
            if problems or len(spawned) != args.count:
                sys.exit(f"repeat {i + 1}: unusable observation "
                         f"({len(spawned)} spawned, {problems!r:.200})")
            runs.append({"repeat": i + 1, "spawned": spawned,
                         "baseline": [trim(u) for u in base["units"]],
                         "final": [trim(u) for u in final["units"]]})
            print(f"repeat {i + 1}/{args.repeats}: {len(spawned)} units recorded")
    finally:
        quit_engine(args.port, proc)
    doc = {
        "about": ("Real humid-heat (46 C / 0.95) baseline/final observations of "
                  f"{args.count} acolytes x {args.repeats} repeats, recorded with "
                  "tools/record_physiology_heat_fixture.py at the revision below. "
                  "Each unit carries its core temperature, hyperthermia meter, "
                  "the engine heat-balance read (thermo.readHeatBalance) and the "
                  "cumulative executed-step ledger at both snapshots. Used by "
                  "tools/test_physiology_probe.py to prove that the probe's "
                  "expected rise agrees with the rise the engine produced."),
        "revision": revision,
        "command": ("python3 tools/record_physiology_heat_fixture.py "
                    f"--port {args.port} --repeats {args.repeats} "
                    f"--seconds {args.seconds:g} --count {args.count}"),
        "scenario": {"name": name, "ambient": ambient, "humidity": humidity,
                     "seconds": args.seconds, "count": args.count},
        "runs": runs}
    with open(args.out, "w") as fh:
        json.dump(doc, fh, separators=(",", ":"))
    print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
