#!/usr/bin/env python3
"""Bounded location-placement frequency sweep (#997, requirement 1).

Measures how often world generation places ZERO locations, over a fixed,
reproducible set of 21 DISTINCT generation tuples at the GUI/default
configuration (10 plates, `config/world_gen_default.yaml` unchanged).

This is a one-off MEASUREMENT tool, not a gate: it is deliberately kept
out of `tools/ci_probes.py` (which only enumerates `*_probe.py`) and out
of `tools/location_overlay_probe.py`, whose placement matrix stays small
enough to run by hand. Re-run it when the placement pass changes and you
want a fresh frequency number; the acceptance gates live elsewhere.

Sweep shape (issue #997 as amended by its approved review): for base seed
`s` in {0, 1, 2}, run index `r` within each (base seed, size) cell uses
`seed = s + 3*r`, so every run is a DISTINCT world — generation is a pure
function of the complete tuple, so repeating a tuple would regenerate an
identical world rather than sample a new one.

    size 256 -> 1 run  per base seed -> seeds 0,1,2
    size 128 -> 2 runs per base seed -> seeds 0..5
    size  64 -> 4 runs per base seed -> seeds 0..11

Each world is generated in its OWN engine process, so a run is
independently reproducible from its printed command line and no world
can influence the next.

Usage:
  python3 tools/location_placement_sweep.py
  python3 tools/location_placement_sweep.py --json /tmp/sweep.json
  python3 tools/location_placement_sweep.py --only 64        # one size
  python3 tools/location_placement_sweep.py --seed 7 --size 128 --single

Exit code 0 = the sweep completed (regardless of how many zeros it
found); non-zero = a run failed to generate.
"""
from __future__ import annotations

import argparse
import json
import subprocess
import sys
import time

from probelib import boot, quit_engine, send

LOG = "/tmp/location_placement_sweep_engine.log"

# Runs per base seed at each size, per requirement 1's shape.
RUNS_PER_SEED = {256: 1, 128: 2, 64: 4}
BASE_SEEDS = (0, 1, 2)
PLATES = 10

# waitForInit budget per size, seconds. A w256 generation is the slow one.
INIT_TIMEOUT = {64: 240, 128: 480, 256: 1200}


def sweep_tuples(sizes: tuple[int, ...] = (256, 128, 64)) -> list[dict]:
    """The fixed enumeration, in a stable order. Every tuple is distinct."""
    out: list[dict] = []
    for size in sizes:
        for r in range(RUNS_PER_SEED[size]):
            for s in BASE_SEEDS:
                out.append({
                    "base_seed": s,
                    "run_index": r,
                    "seed": s + 3 * r,
                    "size": size,
                    "plates": PLATES,
                })
    return out


def revision() -> str:
    try:
        return subprocess.run(["git", "rev-parse", "HEAD"], capture_output=True,
                              text=True, check=True).stdout.strip()
    except Exception:
        return "unknown"


def dirty() -> bool:
    try:
        r = subprocess.run(["git", "status", "--porcelain"], capture_output=True,
                           text=True, check=True)
        return bool(r.stdout.strip())
    except Exception:
        return False


class SweepError(RuntimeError):
    """A run that cannot be counted — never silently folded into the sample."""


def parse_placements(raw: str) -> list[dict]:
    """The placed-location list, or SweepError.

    An empty overlay comes back as the empty Lua table `{}`. That is a
    LEGITIMATE zero-placement sample — the exact thing this tool
    measures — so it has to stay distinguishable from a query that
    failed. Everything else must parse as a JSON array: the debug
    console reports a Lua failure as plain `error: ...` text rather than
    raising, so mapping an unreadable reply to `[]` would file a broken
    read as a genuine zero and overstate the measured defect rate.
    """
    if raw in ("{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError as exc:
        raise SweepError(
            f"could not read the placed-location list ({exc}); reply was {raw!r}"
        ) from exc
    if not isinstance(data, list):
        raise SweepError(
            f"placed-location query returned a {type(data).__name__}, not a list; "
            f"reply was {raw!r}")
    return data


def run_one(port: int, seed: int, size: int, plates: int) -> dict:
    """Generate one world in a fresh engine and count its placements.

    The page is generated but never shown: `world.listPlacedLocations`
    takes the page id, so the overlay is readable straight off the gen
    params without activating the world (same trick the overlay probe's
    hidden-page phase uses).
    """
    page = "sweep"
    proc = boot(port, log=LOG)
    try:
        # Items FIRST: since #917 a location's guaranteed significant
        # content must resolve against the item registry, so
        # engine.loadLocationYaml rejects the whole file when the item
        # it names is unregistered — the same order
        # scripts/startup_loader.lua uses and data/locations/*.yaml's
        # own header states.
        send(port, "local fs = engine.listFiles('data/items', '.yaml') or {}; "
                   "for _, f in ipairs(fs) do "
                   "engine.loadItemYaml('data/items/' .. f) end; return #fs",
             timeout=30.0)
        send(port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); return 'ok'")
        send(port, f"world.init('{page}', {seed}, {size}, {plates}); return 'ok'")
        budget = INIT_TIMEOUT[size]
        t0 = time.time()
        # world.waitForInit returns the CURRENT progress whether or not it
        # actually reached LoadDone -- on timeout it just stops polling and
        # reports where generation got to. Silently discarding that would
        # let an unfinished (or never-started) generation be queried as
        # empty and recorded as a genuine zero-placement world, which is
        # precisely the measurement this tool exists to make. `local phase
        # = ...` keeps the first of its four return values; 3 == done.
        phase = send(port, f"local phase = world.waitForInit({budget}); return phase",
                     timeout=budget + 30).strip()
        elapsed = time.time() - t0
        if phase != "3":
            raise SweepError(
                f"generation did not finish: world.waitForInit returned phase "
                f"{phase or '<no reply>'} after {budget}s (3 = done). A placement "
                f"count from this run would be meaningless, so the sweep stops "
                f"rather than record it.")
        entries = parse_placements(
            send(port, f"return world.listPlacedLocations('{page}')", timeout=30).strip())
        ruins = [e for e in entries if e.get("id") == "ruin_small"]
        return {
            "placed": len(entries),
            "ruin_small": len(ruins),
            "gen_seconds": round(elapsed, 1),
            "chunks": sorted((e["cx"], e["cy"]) for e in ruins),
        }
    finally:
        quit_engine(port, proc)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9191)
    ap.add_argument("--json", default=None, help="write the full record here")
    ap.add_argument("--only", type=int, action="append", choices=[64, 128, 256],
                    help="restrict to one or more sizes (repeatable)")
    ap.add_argument("--single", action="store_true",
                    help="run exactly one tuple from --seed/--size")
    ap.add_argument("--seed", type=int, default=0, help="with --single")
    ap.add_argument("--size", type=int, default=128, help="with --single")
    args = ap.parse_args()

    if args.single:
        tuples = [{"base_seed": args.seed, "run_index": 0, "seed": args.seed,
                   "size": args.size, "plates": PLATES}]
    else:
        sizes = tuple(args.only) if args.only else (256, 128, 64)
        tuples = sweep_tuples(sizes)

    rev = revision()
    record = {
        "repo_revision": rev,
        "repo_dirty": dirty(),
        "tool": "tools/location_placement_sweep.py",
        "config": "config/world_gen_default.yaml (unchanged)",
        "plates": PLATES,
        "definitions": "data/locations/ruin_small.yaml (the only registered def)",
        "runs": [],
    }
    print(f"revision {rev}{' (DIRTY)' if record['repo_dirty'] else ''}, "
          f"{len(tuples)} world(s), {PLATES} plates, default gen config")
    print("-" * 68)

    zeros = 0
    for i, t in enumerate(tuples, 1):
        print(f"[{i:2d}/{len(tuples)}] seed {t['seed']:2d} size {t['size']:3d} "
              f"plates {t['plates']} ... ", end="", flush=True)
        try:
            r = run_one(args.port, t["seed"], t["size"], t["plates"])
        except (SystemExit, SweepError) as exc:
            print(f"FAILED ({exc})")
            record["runs"].append({**t, "error": str(exc)})
            with open(args.json or "/tmp/location_placement_sweep.json", "w") as fh:
                json.dump(record, fh, indent=2)
            return 1
        row = {**t, **r}
        record["runs"].append(row)
        if r["ruin_small"] == 0:
            zeros += 1
            print(f"ZERO locations  ({r['gen_seconds']}s)")
        else:
            print(f"{r['ruin_small']:2d} ruin_small   ({r['gen_seconds']}s)")

    total = len(record["runs"])
    record["total_worlds"] = total
    record["zero_placement_worlds"] = zeros
    record["zero_placement_frequency"] = round(zeros / total, 4) if total else 0.0
    record["zero_placement_tuples"] = [
        {"seed": r["seed"], "size": r["size"], "plates": r["plates"]}
        for r in record["runs"] if r.get("ruin_small") == 0
    ]

    print("-" * 68)
    print(f"{zeros}/{total} world(s) placed ZERO locations "
          f"({record['zero_placement_frequency'] * 100:.1f}%)")
    for z in record["zero_placement_tuples"]:
        print(f"  zero: seed={z['seed']} size={z['size']} plates={z['plates']}")

    path = args.json or "/tmp/location_placement_sweep.json"
    with open(path, "w") as fh:
        json.dump(record, fh, indent=2)
    print(f"record written to {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
