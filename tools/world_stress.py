#!/usr/bin/env python3
"""World generation stress tester.

Sweeps many random seeds across configurable world sizes and regions,
runs the audit on each, and reports anomalies. Designed to find edge
cases the curated baseline set might miss.

Output is summary statistics + a list of "interesting" seeds (ones with
unusually high issue counts or rare bug categories).
"""

from __future__ import annotations

import argparse
import json
import random
import statistics
import sys
import time
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any

# Reuse audit and dump utilities
sys.path.insert(0, str(Path(__file__).resolve().parent))
from world_audit import audit_dump  # type: ignore
from world_determinism import run_dump  # type: ignore


# ----- Sweep configuration -------------------------------------------------

DEFAULT_WORLD_SIZES = [32, 64]
DEFAULT_REGION = (-2, -2, 2, 2)
DEFAULT_SEED_COUNT = 30

# A seed is "interesting" if any single category exceeds this many issues
INTERESTING_THRESHOLD = 10


# ----- Result aggregation --------------------------------------------------

def run_one(seed: int, world_size: int,
            region: tuple[int, int, int, int]) -> dict[str, Any] | None:
    """Run dump+audit for one seed/size/region combination."""
    try:
        data = run_dump(seed, world_size, region)
    except RuntimeError as exc:
        return {"seed": seed, "world_size": world_size,
                "region": list(region), "error": str(exc)[:200]}
    result = audit_dump(data, seed=seed, world_size=world_size, region=region)
    summary = result.summary()
    return {
        "seed": seed,
        "world_size": world_size,
        "region": list(region),
        "tile_count": result.tile_count,
        "fluid_stats": dict(sorted(result.fluid_stats.items(),
                                    key=lambda kv: (kv[0] is None, kv[0] or ""))),
        "elevation_stats": result.elevation_stats,
        "summary": summary,
        "total_issues": sum(summary.values()),
    }


def is_interesting(record: dict[str, Any]) -> bool:
    if "error" in record:
        return True
    return any(count >= INTERESTING_THRESHOLD
               for count in record["summary"].values())


def aggregate(records: list[dict[str, Any]]) -> dict[str, Any]:
    """Compute summary statistics across all records."""
    successful = [r for r in records if "error" not in r]
    errors = [r for r in records if "error" in r]

    # Count by world size
    by_size: dict[int, list[dict[str, Any]]] = defaultdict(list)
    for r in successful:
        by_size[r["world_size"]].append(r)

    # Aggregate issue counts per category
    all_categories: Counter[str] = Counter()
    issue_per_record: list[int] = []
    for r in successful:
        for cat, count in r["summary"].items():
            all_categories[cat] += count
        issue_per_record.append(r["total_issues"])

    # Per-size aggregates
    size_stats = {}
    for sz, rs in sorted(by_size.items()):
        cat_totals: Counter[str] = Counter()
        for r in rs:
            for cat, count in r["summary"].items():
                cat_totals[cat] += count
        size_stats[str(sz)] = {
            "seeds": len(rs),
            "total_issues": sum(r["total_issues"] for r in rs),
            "max_issues": max(r["total_issues"] for r in rs) if rs else 0,
            "median_issues": int(statistics.median([r["total_issues"] for r in rs])) if rs else 0,
            "by_category": dict(sorted(cat_totals.items(),
                                        key=lambda kv: -kv[1])),
        }

    return {
        "total_runs": len(records),
        "successful": len(successful),
        "errors": len(errors),
        "total_issues": sum(issue_per_record),
        "median_issues": int(statistics.median(issue_per_record)) if issue_per_record else 0,
        "max_issues": max(issue_per_record) if issue_per_record else 0,
        "by_world_size": size_stats,
        "by_category": dict(sorted(all_categories.items(),
                                    key=lambda kv: -kv[1])),
    }


# ----- Output --------------------------------------------------------------

def format_report(records: list[dict[str, Any]],
                  agg: dict[str, Any]) -> str:
    lines = []
    lines.append("=" * 60)
    lines.append("World gen stress test report")
    lines.append("=" * 60)
    lines.append("")
    lines.append(f"Total runs:    {agg['total_runs']}")
    lines.append(f"Successful:    {agg['successful']}")
    lines.append(f"Errors:        {agg['errors']}")
    lines.append(f"Total issues:  {agg['total_issues']}")
    lines.append(f"Median issues: {agg['median_issues']} per run")
    lines.append(f"Max issues:    {agg['max_issues']} (single run)")
    lines.append("")
    lines.append("By world size:")
    for sz, stats in agg["by_world_size"].items():
        lines.append(f"  worldSize {sz}: {stats['seeds']} runs, "
                     f"{stats['total_issues']} issues "
                     f"(median {stats['median_issues']}, max {stats['max_issues']})")
        if stats["by_category"]:
            top_cats = list(stats["by_category"].items())[:5]
            cat_str = ", ".join(f"{c}={n}" for c, n in top_cats)
            lines.append(f"    top: {cat_str}")
    lines.append("")
    lines.append("By category (all sizes):")
    for cat, count in agg["by_category"].items():
        lines.append(f"  {cat}: {count}")
    lines.append("")

    interesting = [r for r in records if is_interesting(r)]
    if interesting:
        lines.append(f"Interesting runs ({len(interesting)}):")
        for r in interesting:
            if "error" in r:
                lines.append(f"  ERROR seed={r['seed']} size={r['world_size']}: "
                             f"{r['error']}")
            else:
                top_cats = sorted(r["summary"].items(),
                                  key=lambda kv: -kv[1])[:3]
                cat_str = ", ".join(f"{c}={n}" for c, n in top_cats)
                lines.append(f"  seed={r['seed']:6d} size={r['world_size']:3d} "
                             f"total={r['total_issues']:4d}  {cat_str}")
    else:
        lines.append("No interesting runs (all under threshold).")

    return "\n".join(lines)


# ----- Main ----------------------------------------------------------------

def parse_region(s: str) -> tuple[int, int, int, int]:
    parts = s.split(",")
    if len(parts) != 4:
        raise argparse.ArgumentTypeError(
            f"region must be cx1,cy1,cx2,cy2 (got {s!r})"
        )
    return (int(parts[0]), int(parts[1]), int(parts[2]), int(parts[3]))


def parse_world_sizes(s: str) -> list[int]:
    return [int(x) for x in s.split(",")]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--seeds", type=int, default=DEFAULT_SEED_COUNT,
                        help=f"Number of random seeds to test (default: {DEFAULT_SEED_COUNT})")
    parser.add_argument("--seed-source", type=int, default=2026,
                        help="Seed for the random number generator (default: 2026)")
    parser.add_argument("--world-sizes", type=parse_world_sizes,
                        default=DEFAULT_WORLD_SIZES,
                        help=f"Comma-separated world sizes (default: {','.join(map(str, DEFAULT_WORLD_SIZES))})")
    parser.add_argument("--region", type=parse_region,
                        default=DEFAULT_REGION,
                        help=f"Chunk region cx1,cy1,cx2,cy2 (default: {DEFAULT_REGION})")
    parser.add_argument("--output", type=Path,
                        help="Write JSON results to FILE")
    parser.add_argument("--format", choices=("json", "text"),
                        default="text",
                        help="Output format (default: text)")
    args = parser.parse_args()

    # Generate the seed list deterministically from seed_source
    rng = random.Random(args.seed_source)
    seeds = [rng.randrange(1, 1000000) for _ in range(args.seeds)]

    total_runs = len(seeds) * len(args.world_sizes)
    print(f"Stress test: {len(seeds)} seeds × {len(args.world_sizes)} sizes "
          f"= {total_runs} runs", file=sys.stderr)
    print(f"Region: {args.region}", file=sys.stderr)
    print(f"World sizes: {args.world_sizes}", file=sys.stderr)
    print("", file=sys.stderr)

    records = []
    started = time.time()
    for i, seed in enumerate(seeds):
        for size in args.world_sizes:
            run_idx = i * len(args.world_sizes) + len(records) % len(args.world_sizes) + 1
            elapsed = time.time() - started
            print(f"  [{len(records)+1:3d}/{total_runs}] seed={seed:6d} "
                  f"size={size:3d} (elapsed {elapsed:.0f}s)",
                  file=sys.stderr, end="", flush=True)
            r = run_one(seed, size, args.region)
            records.append(r)
            if r and "error" in r:
                print(f"  ERROR", file=sys.stderr)
            elif r:
                print(f"  issues={r['total_issues']}", file=sys.stderr)
            else:
                print(f"  no result", file=sys.stderr)

    elapsed_total = time.time() - started
    print("", file=sys.stderr)
    print(f"Done in {elapsed_total:.0f}s "
          f"({elapsed_total / total_runs:.1f}s per run)", file=sys.stderr)
    print("", file=sys.stderr)

    agg = aggregate(records)

    if args.format == "json":
        output = json.dumps({"records": records, "aggregate": agg},
                            indent=2, sort_keys=False)
    else:
        output = format_report(records, agg)

    if args.output:
        args.output.write_text(output + "\n")
    else:
        print(output)

    # Exit code: 0 if no errors, 1 if any errors
    return 0 if agg["errors"] == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
