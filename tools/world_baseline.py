#!/usr/bin/env python3
"""Capture baseline outputs for the world generation regression suite.

For each seed in seeds.json, runs the dump multiple times to:
  1. Detect whether the seed is currently deterministic
  2. Capture the audit output (or a representative thereof)
  3. Record the bug count envelope across runs

Writes one baseline file per seed — except for a seed whose strictly
compared invariants varied across its own capture runs, which is refused
rather than sampled (see check_strict_invariants).
"""

from __future__ import annotations

import argparse
import hashlib
import json
import statistics
import subprocess
import sys
from collections import Counter
from pathlib import Path
from typing import Any

# Reuse utilities from the audit and determinism scripts
sys.path.insert(0, str(Path(__file__).resolve().parent))
from world_audit import audit_dump  # type: ignore
from world_determinism import canonical_dump, hash_dump, run_dump  # type: ignore


REPO_ROOT = Path(__file__).resolve().parent.parent
BASELINE_DIR = Path(__file__).resolve().parent / "baselines"
SEEDS_FILE = BASELINE_DIR / "_seeds.json"


def baseline_path(seed: int, world_size: int,
                  region: tuple[int, int, int, int]) -> Path:
    cx1, cy1, cx2, cy2 = region
    name = f"seed{seed}_size{world_size}_region_{cx1}_{cy1}_{cx2}_{cy2}.json"
    return BASELINE_DIR / name


def envelope_of(values: list[int]) -> dict[str, int]:
    """Compute min/max/median of a list of int values."""
    return {
        "min": min(values),
        "max": max(values),
        "median": int(statistics.median(values)),
    }


# The invariants world_check.py compares for EXACT equality with no
# envelope (world_check.py:432-451): the tile count and these four
# elevation statistics. Anything not in this set is modelled as an
# envelope or a recorded-variation policy on both sides instead.
STRICT_ELEVATION_KEYS = ("min", "max", "median", "count")


def _observation_sort_key(value: Any) -> tuple[int, Any]:
    """Order ints numerically and anything else by its rendering.

    The elevation statistics are int-or-None: world_audit.compute_stats
    yields None for min/max/median when a region holds no real terrain.
    sorted() over a set mixing the two raises TypeError, which would
    replace the required failure with a crash on exactly the input that
    most needs reporting.
    """
    if isinstance(value, int) and not isinstance(value, bool):
        return (0, value)
    return (1, repr(value))


def distinct_observations(values: list[Any]) -> list[Any]:
    """The distinct values in `values`, ordered stably for reporting."""
    return sorted(set(values), key=_observation_sort_key)


def check_strict_invariants(seed: int, audit_results: list[Any]) -> None:
    """Refuse the baseline when an exactly-compared invariant varied.

    world_check.py compares the tile count and every key in
    STRICT_ELEVATION_KEYS for exact equality against the recorded
    baseline, so publishing one arbitrary run of a field that varied
    would make every later check either flaky or a false regression
    report. Raising is the honest outcome: main() catches RuntimeError
    per seed, so that seed's existing baseline is left byte-identical,
    the remaining seeds are still captured, and the run exits non-zero.

    Every violated invariant is reported with all of its distinct
    observed values, so a capture that varied in several fields at once
    can be acted on without re-running.

    This is deliberately narrower than "everything must be stable": the
    fluid and audit-issue envelopes, and the recorded-hash racy policy,
    model their own variation and are untouched. A hash-racy seed whose
    strict invariants held is still captured normally.
    """
    violations: list[str] = []

    tile_counts = distinct_observations([r.tile_count for r in audit_results])
    if len(tile_counts) > 1:
        violations.append(
            f"tileCount varies across capture runs: "
            f"{', '.join(repr(v) for v in tile_counts)}")

    for key in STRICT_ELEVATION_KEYS:
        observed = distinct_observations(
            [r.elevation_stats.get(key) for r in audit_results])
        if len(observed) > 1:
            violations.append(
                f"elevationStats.{key} varies across capture runs: "
                f"{', '.join(repr(v) for v in observed)}")

    if violations:
        raise RuntimeError(
            f"seed {seed}: strict invariant(s) varied, baseline NOT written "
            f"(world_check.py compares these exactly): " + "; ".join(violations))


def capture_seed(seed: int, world_size: int,
                 region: tuple[int, int, int, int],
                 runs: int) -> dict[str, Any]:
    """Run the dump N times for a single seed and summarize."""
    print(f"  capturing seed={seed} worldSize={world_size} region={region} ...",
          file=sys.stderr, end=" ", flush=True)

    dumps = []
    hashes = []
    audit_results = []
    for _ in range(runs):
        data = run_dump(seed, world_size, region)
        dumps.append(data)
        hashes.append(hash_dump(data))
        result = audit_dump(data, seed=seed, world_size=world_size, region=region)
        audit_results.append(result)

    deterministic = len(set(hashes)) == 1
    print(f"{'DET' if deterministic else 'RACY'} "
          f"({len(set(hashes))} distinct outputs)", file=sys.stderr)

    # A field the checker compares exactly must not be sampled: refuse the
    # seed instead of publishing one arbitrary observation of a varying
    # invariant.
    check_strict_invariants(seed, audit_results)

    # Pick a representative dump for the audit baseline (run 0). The
    # strictly compared fields it supplies — tileCount and
    # elevationStats — are no longer a sample: the check above proved
    # every run agreed on them.
    representative = audit_results[0].to_dict()

    # Compute bug count envelope across all runs
    all_categories: set[str] = set()
    for r in audit_results:
        all_categories.update(r.summary().keys())

    issue_envelope: dict[str, dict[str, int]] = {}
    for cat in sorted(all_categories):
        counts = [r.summary().get(cat, 0) for r in audit_results]
        e = envelope_of(counts)
        e["all"] = counts
        issue_envelope[cat] = e

    # Compute fluid stat envelope across all runs
    all_fluids: set[str] = set()
    for r in audit_results:
        all_fluids.update(r.fluid_stats.keys())

    fluid_envelope: dict[str, dict[str, int]] = {}
    for fluid in sorted(all_fluids):
        counts = [r.fluid_stats.get(fluid, 0) for r in audit_results]
        e = envelope_of(counts)
        e["all"] = counts
        fluid_envelope[fluid] = e

    return {
        "metadata": {
            "seed": seed,
            "worldSize": world_size,
            "region": list(region),
            "captureRuns": runs,
        },
        "determinism": {
            "deterministic": deterministic,
            "distinctHashes": len(set(hashes)),
            "hashes": hashes,
        },
        "tileCount": representative["tileCount"],
        "elevationStats": representative["elevationStats"],
        "fluidStats": representative["fluidStats"],
        "fluidEnvelope": fluid_envelope,
        "auditEnvelope": issue_envelope,
        "representativeAudit": {
            "summary": representative["summary"],
            "issues": representative["issues"],
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs", type=int, default=3,
                        help="Number of dump runs per seed (default: 3)")
    parser.add_argument("--seeds-file", type=Path, default=SEEDS_FILE,
                        help=f"Seed config file (default: {SEEDS_FILE})")
    parser.add_argument("--seed", type=int,
                        help="Capture only this specific seed (must be in seeds-file)")
    args = parser.parse_args()

    if not args.seeds_file.exists():
        print(f"error: seeds file not found: {args.seeds_file}", file=sys.stderr)
        return 1

    config = json.loads(args.seeds_file.read_text())
    seeds = config["seeds"]

    if args.seed is not None:
        seeds = [s for s in seeds if s["seed"] == args.seed]
        if not seeds:
            print(f"error: seed {args.seed} not in seeds file", file=sys.stderr)
            return 1

    print(f"Capturing baselines for {len(seeds)} seed(s) "
          f"with {args.runs} runs each", file=sys.stderr)

    BASELINE_DIR.mkdir(exist_ok=True)

    captured = 0
    failed = 0
    for entry in seeds:
        seed = entry["seed"]
        world_size = entry["world_size"]
        region = tuple(entry["region"])
        try:
            baseline = capture_seed(seed, world_size, region, args.runs)
            path = baseline_path(seed, world_size, region)
            path.write_text(json.dumps(baseline, indent=2, sort_keys=False) + "\n")
            captured += 1
        except RuntimeError as exc:
            print(f"  FAILED: {exc}", file=sys.stderr)
            failed += 1

    print(f"\nCaptured {captured} baselines, {failed} failures",
          file=sys.stderr)
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
