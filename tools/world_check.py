#!/usr/bin/env python3
"""World generation regression check.

Runs the dump pipeline against all seeds in tools/baselines/_seeds.json,
compares to stored baselines, and reports pass/fail.

For each seed:
  1. Runs the dump N times (default 1)
  2. Runs the audit on the first dump
  3. Compares to the stored baseline:
     - The canonical content hash of every dump must equal the hash the
       baseline recorded (see check_baseline_hash). This is the only
       check here that sees the dump's full content: matId, ore bands,
       ice mode and the water table appear in none of the statistics
       below, so a change that shuffles one of them while preserving
       elevation and tile counts is caught here and nowhere else. It
       covers exactly what the bare `--dump` schema emits and no more —
       the opt-in slope layer, flora, structures and magma are not in
       the dump, so they are not fingerprinted by this or any other
       comparison in this tool. A baseline that recorded more than one
       distinct hash is racy: content identity cannot be gated against
       it, and the seed's own output line says so.
     - Tile count and elevation stats must match exactly
     - Fluid stats must stay within the baseline envelope (with a
       tile-count-scaled tolerance for racy shuffling)
     - Issue summary:
         * every category must be classified in world_audit.py, and every
           QUALITY category must have an explicit threshold — an
           unclassified category or a missing threshold FAILS the seed by
           name, at any count including zero
         * BUG categories must be zero (always)
         * QUALITY categories must stay under their threshold (always),
           and for a deterministic seed must additionally MATCH the
           baseline summary exactly — a count above baseline fails as a
           regression, below baseline is reported as an improvement
         * racy seeds only get an informational envelope-drift note
     - Determinism status: improvement is OK, regression is a failure.
       Only active at --runs 2+; at the default --runs 1 a single dump
       cannot be compared with itself, and the content coverage comes
       from the baseline-hash comparison above instead.

Exit codes:
  0 = all checks passed
  1 = one or more failures
  2 = bad invocation
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

# Reuse utilities
sys.path.insert(0, str(Path(__file__).resolve().parent))
from world_audit import (  # type: ignore
    audit_dump, classify_category, QUALITY_THRESHOLDS, BUG_CATEGORIES,
)
from world_determinism import hash_dump, run_dump  # type: ignore
from world_baseline import baseline_path, BASELINE_DIR, SEEDS_FILE  # type: ignore


# ----- Result types --------------------------------------------------------

PASS = "PASS"
FAIL = "FAIL"
IMPROVED = "IMPROVED"
SKIP = "SKIP"


@dataclass
class CheckResult:
    seed: int
    world_size: int
    region: tuple[int, int, int, int]
    status: str
    notes: list[str] = field(default_factory=list)
    failures: list[str] = field(default_factory=list)
    improvements: list[str] = field(default_factory=list)
    # Banners ride the seed's own summary line, so they survive a PASS.
    # `notes` are suppressed unless the seed fails or -v is passed, which
    # makes them the wrong carrier for "this seed is not being gated".
    banners: list[str] = field(default_factory=list)


def seed_label(result: CheckResult) -> str:
    """Full identity of a seed entry — seed alone is not unique.

    The corpus runs seed 137 at both worldSize 32 and 64, so a failure
    message naming only the seed cannot be traced to a baseline file.
    """
    cx1, cy1, cx2, cy2 = result.region
    return (f"seed={result.seed} worldSize={result.world_size} "
            f"region={cx1},{cy1},{cx2},{cy2}")


def fmt_status(status: str) -> str:
    pad = f"{status:<8}"
    if status == PASS:
        return pad
    elif status == IMPROVED:
        return pad
    elif status == FAIL:
        return pad
    return pad


# ----- Check logic ---------------------------------------------------------

def check_envelope(label: str, current: int, env: dict[str, int] | None,
                   result: CheckResult, tolerance: int = 0) -> None:
    """Check a single metric against its envelope.

    Records a failure if current exceeds env['max'] + tolerance, or an
    improvement if current is below env['min'] - tolerance. No envelope
    means any new nonzero value is a failure.

    The tolerance is additive above the envelope max and below the
    envelope min; it accounts for the baseline not capturing the full
    race space when the pipeline is non-deterministic.
    """
    if env is None:
        if current > 0:
            result.status = FAIL
            result.failures.append(
                f"{label}: new/unseen value {current} (no baseline envelope)"
            )
        return
    env_max = env["max"]
    env_min = env["min"]
    if current > env_max + tolerance:
        result.status = FAIL
        result.failures.append(
            f"{label}: {current} exceeds envelope [{env_min}..{env_max}] "
            f"(+{current - env_max}, tolerance={tolerance})"
        )
    elif current < env_min - tolerance:
        result.improvements.append(
            f"{label}: {current} below envelope [{env_min}..{env_max}] "
            f"(-{env_min - current})"
        )
        if result.status == PASS:
            result.status = IMPROVED


def check_determinism_status(deterministic_baseline: bool, deterministic_now: bool,
                            n_distinct: int, runs: int, result: CheckResult) -> None:
    """Apply the determinism-status rule: improvement is OK, regression fails.

    A seed that was deterministic in the baseline but produced more than
    one distinct output across this run's dumps is a regression (FAIL).
    A seed that was racy and is now deterministic across runs>1 is an
    improvement.

    INACTIVE AT runs == 1 — the default, and the only setting any CI or
    `make ci` path uses. A single dump cannot be compared with itself, so
    `deterministic_now` is structurally True there and neither branch
    below can fire. The guard states that outright rather than leaving a
    dead branch reading as though it gates something.

    Content coverage at runs == 1 comes from check_baseline_hash, which
    compares the one dump against the hash the baseline recorded. That is
    SAMPLED content identity against a fixed reference: it catches a race
    only when the sampled run differs from that reference, and it is not
    equivalent to observing run-to-run determinism. Pass runs>=2 to
    activate this check as well.
    """
    if runs < 2:
        return

    if deterministic_baseline and not deterministic_now:
        result.status = FAIL
        result.failures.append(
            f"determinism regression: baseline deterministic, now "
            f"{n_distinct} distinct outputs across {runs} runs"
        )
    elif not deterministic_baseline and deterministic_now and runs > 1:
        result.improvements.append(
            f"determinism status: was racy, now deterministic across {runs} runs"
        )
        if result.status == PASS:
            result.status = IMPROVED


def check_baseline_hash(current_hashes: list[str],
                        determinism: dict[str, Any],
                        result: CheckResult) -> None:
    """Compare this run's dump content hashes against the baseline's.

    Every baseline capture already records the canonical content hash of
    each of its runs (world_baseline.py::capture_seed). Comparing one
    current dump against that fixed reference is the cheapest coverage
    available — it needs no extra world generation, so it applies at the
    default --runs 1 that CI uses — and it is the only comparison in this
    tool that sees the dump's whole content rather than a derived
    statistic.

    Three baseline shapes, all handled explicitly:

      one distinct recorded hash (the entire current corpus)
        Strict content identity: every current dump must equal it. Any
        mismatch is a FAIL naming the seed and both hashes.

      more than one distinct recorded hash (a genuinely racy baseline)
        The recorded hashes are three samples of a race, not an
        enumeration of its outcomes, so a current dump matching none of
        them proves nothing and matching one proves nothing either.
        Picking one historical run as the reference would be arbitrary.
        The established racy-envelope policy therefore stands alone for
        such a seed: no content-identity gate, and a banner on the seed's
        own output line saying so, since a silent pass would read exactly
        like a gated one. No tracked baseline is in this state today.

      no recorded hashes at all
        A baseline predating hash capture. Same treatment as racy: no
        gate, and a banner saying the gate is unavailable.

    A baseline whose `deterministic` flag disagrees with its own hash
    count is malformed — capture_seed derives the flag from exactly that
    count — and fails, because the two rules would otherwise disagree
    about the same seed: this function keys the strict/racy split on the
    hashes while check_determinism_status keys on the flag.
    """
    recorded = determinism.get("hashes")
    if not isinstance(recorded, list) or not recorded:
        result.banners.append(
            "no recorded baseline hash: content-identity gate unavailable"
        )
        return

    distinct = sorted(set(recorded))
    deterministic_baseline = bool(determinism.get("deterministic"))
    if (len(distinct) == 1) != deterministic_baseline:
        result.status = FAIL
        result.failures.append(
            f"malformed baseline ({seed_label(result)}): determinism.deterministic "
            f"is {deterministic_baseline} but determinism.hashes holds "
            f"{len(distinct)} distinct value(s) — the flag is derived from that "
            f"count, so the baseline was hand-edited or written by an older tool"
        )
        return

    if len(distinct) > 1:
        result.banners.append(
            f"racy baseline ({len(distinct)} distinct recorded hashes): "
            f"content-identity gate unavailable, envelope checks only"
        )
        return

    expected = distinct[0]
    mismatched = sorted({h for h in current_hashes if h != expected})
    if mismatched:
        result.status = FAIL
        result.failures.append(
            f"content hash mismatch ({seed_label(result)}): baseline "
            f"{expected}, current {', '.join(mismatched)} — the dump's content "
            f"changed in a field the statistic comparisons do not model, or "
            f"worldgen is no longer reproducing this seed"
        )


def check_issue_summary(current_summaries: list[dict[str, int]],
                        baseline_summary: dict[str, int],
                        issue_env: dict[str, Any],
                        strict: bool, result: CheckResult) -> None:
    """Check the audit issue summary against the baseline.

    Severity rules (see world_audit.py::BUG_CATEGORIES / QUALITY_CATEGORIES):

      UNCLASSIFIED — a category in neither set fails outright, naming the
                category. This is the fail-closed gate: a category added to
                a check function but classified nowhere must not slip
                through as a quality metric under an implicit default.

      BUG     — any non-zero count fails, always. A baseline cannot bless
                corruption, so this overrides the match/threshold logic
                regardless of determinism.

      QUALITY — first, an absolute threshold cap (QUALITY_THRESHOLDS),
                which must be declared explicitly: a QUALITY category with
                no entry fails, naming it. The cap itself fails
                unconditionally. Under the cap:
                  strict (deterministic baseline AND deterministic current)
                    → the count must MATCH the baseline summary exactly.
                      A count above baseline is a regression (FAIL); below
                      baseline is an improvement (re-baseline, IMPROVED).
                  racy
                    → only an informational envelope-drift note, since the
                      baseline cannot capture the full race space.

    `strict` should be `deterministic_baseline and deterministic_now`. In
    that case every current summary is identical, so the max over runs is
    the single deterministic value.

    The classification and threshold checks run over the union of the
    current summaries, the baseline summary and the audit envelope, so a
    category that is only present in the baseline or the envelope is
    rejected too, even though its current count is zero.
    """
    all_categories: set[str] = set(issue_env.keys()) | set(baseline_summary.keys())
    for cs in current_summaries:
        all_categories.update(cs.keys())

    for cat in sorted(all_categories):
        current_values = [cs.get(cat, 0) for cs in current_summaries]
        cur_max = max(current_values)
        sev = classify_category(cat)
        if sev is None:
            result.status = FAIL
            result.failures.append(
                f"UNCLASSIFIED issue.{cat}: category is in neither "
                f"BUG_CATEGORIES nor QUALITY_CATEGORIES in world_audit.py "
                f"({cur_max} occurrence(s)) — classify it before it can be "
                f"checked"
            )
            continue

        if sev == "BUG":
            if cur_max > 0:
                result.status = FAIL
                result.failures.append(
                    f"BUG issue.{cat}: {cur_max} occurrence(s) — must be 0"
                )
            continue

        if cat not in QUALITY_THRESHOLDS:
            result.status = FAIL
            result.failures.append(
                f"QUALITY issue.{cat}: no explicit threshold in "
                f"QUALITY_THRESHOLDS in world_audit.py ({cur_max} "
                f"occurrence(s)) — add one; there is no implicit default"
            )
            continue

        threshold = QUALITY_THRESHOLDS[cat]
        if cur_max > threshold:
            result.status = FAIL
            result.failures.append(
                f"QUALITY issue.{cat}: {cur_max} exceeds threshold {threshold}"
            )
            continue

        if strict:
            base_val = baseline_summary.get(cat, 0)
            if cur_max > base_val:
                result.status = FAIL
                result.failures.append(
                    f"QUALITY issue.{cat}: {cur_max} regressed above baseline "
                    f"{base_val} (deterministic seed — summary must match baseline)"
                )
            elif cur_max < base_val:
                result.improvements.append(
                    f"issue.{cat}: {cur_max} below baseline {base_val} "
                    f"(deterministic improvement — re-baseline to lock in)"
                )
                if result.status == PASS:
                    result.status = IMPROVED
        else:
            env = issue_env.get(cat)
            if env is not None:
                env_max = env.get("max", 0)
                if cur_max > env_max + max(50, env_max // 4):
                    result.notes.append(
                        f"quality drift issue.{cat}: {cur_max} (baseline max {env_max})"
                    )


def check_seed(entry: dict[str, Any], runs: int) -> CheckResult:
    seed = entry["seed"]
    world_size = entry["world_size"]
    region = tuple(entry["region"])
    result = CheckResult(seed=seed, world_size=world_size, region=region, status=PASS)

    bpath = baseline_path(seed, world_size, region)
    if not bpath.exists():
        result.status = SKIP
        result.notes.append(f"no baseline at {bpath.name}")
        return result

    baseline = json.loads(bpath.read_text())

    # Run dumps
    try:
        dumps = [run_dump(seed, world_size, region) for _ in range(runs)]
    except RuntimeError as exc:
        result.status = FAIL
        result.failures.append(f"dump command failed: {exc}")
        return result

    hashes = [hash_dump(d) for d in dumps]
    deterministic_now = len(set(hashes)) == 1
    deterministic_baseline = baseline["determinism"]["deterministic"]

    check_determinism_status(
        deterministic_baseline, deterministic_now, len(set(hashes)), runs, result
    )

    # Content identity against the baseline's own recorded hash. Runs at
    # every --runs setting, including the default 1, and is what gives
    # the gate any coverage of fields no statistic below models.
    check_baseline_hash(hashes, baseline["determinism"], result)

    # Audit every dump to get a current envelope
    current_audits = [
        audit_dump(d, seed=seed, world_size=world_size, region=region)
        for d in dumps
    ]
    current_summaries = [r.summary() for r in current_audits]
    current_fluids = [r.fluid_stats for r in current_audits]

    # Tile count must always match (strict invariant)
    for ca in current_audits:
        if ca.tile_count != baseline["tileCount"]:
            result.status = FAIL
            result.failures.append(
                f"tileCount changed: {baseline['tileCount']} -> {ca.tile_count}"
            )
            break

    # Elevation stats should always be deterministic (strict invariant)
    bes = baseline["elevationStats"]
    for ca in current_audits:
        ces = ca.elevation_stats
        for key in ("min", "max", "median", "count"):
            if bes.get(key) != ces.get(key):
                result.status = FAIL
                result.failures.append(
                    f"elevationStats.{key}: {bes.get(key)} -> {ces.get(key)}"
                )
                break

    # Fluid stats: compare every current run against the baseline envelope.
    # Fluid counts can fluctuate when the pipeline is racy — use a generous
    # tolerance scaled to the tile count to ignore minor shuffling.
    fluid_tolerance = max(50, baseline["tileCount"] // 100)
    fluid_env = baseline.get("fluidEnvelope", {})
    all_fluids: set[str] = set(fluid_env.keys())
    for cf in current_fluids:
        all_fluids.update(cf.keys())

    for fluid in sorted(all_fluids):
        current_values = [cf.get(fluid, 0) for cf in current_fluids]
        env = fluid_env.get(fluid)
        # Check the max (detects upward drift) and min (detects drops)
        # separately, but avoid duplicate messages when they trigger on
        # the same envelope boundary.
        cur_max = max(current_values)
        cur_min = min(current_values)
        check_envelope(f"fluidStats.{fluid}", cur_max, env,
                       result, tolerance=fluid_tolerance)
        if cur_min != cur_max:
            check_envelope(f"fluidStats.{fluid}", cur_min, env,
                           result, tolerance=fluid_tolerance)

    # Issue summary check (see check_issue_summary for the full rule).
    # Deterministic seeds must reproduce the baseline summary exactly;
    # racy seeds fall back to per-category thresholds.
    strict_summary = deterministic_baseline and deterministic_now
    baseline_summary = baseline.get("representativeAudit", {}).get("summary", {})
    issue_env = baseline.get("auditEnvelope", {})
    check_issue_summary(
        current_summaries, baseline_summary, issue_env, strict_summary, result
    )

    return result


# ----- Output --------------------------------------------------------------

def format_result(r: CheckResult) -> str:
    line = f"  {fmt_status(r.status)} seed={r.seed:5d}"
    if r.failures:
        line += f"  ({len(r.failures)} failure(s))"
    elif r.improvements and r.status == IMPROVED:
        line += f"  ({len(r.improvements)} improvement(s))"
    elif r.status == SKIP:
        line += f"  ({'; '.join(r.notes)})"
    # Banners print unconditionally — on PASS too, and without -v. A seed
    # whose content-identity gate is unavailable must not be reportable
    # as an unqualified pass; format_details is suppressed on PASS, so a
    # note there would be exactly that silent pass.
    for banner in r.banners:
        line += f"  [{banner}]"
    return line


def format_details(r: CheckResult) -> list[str]:
    lines = []
    for f in r.failures:
        lines.append(f"      FAIL: {f}")
    for i in r.improvements:
        lines.append(f"      OK:   {i}")
    for n in r.notes:
        lines.append(f"      note: {n}")
    return lines


# ----- Main ----------------------------------------------------------------

def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs", type=int, default=1,
                        help="Dump runs per seed (default: 1). At 1 the "
                             "gate compares each dump against the content "
                             "hash its baseline recorded — sampled content "
                             "identity against a fixed reference, which "
                             "needs no extra world generation. That is NOT "
                             "a determinism check: run-to-run determinism "
                             "cannot be observed from one dump, so the "
                             "determinism-status rule is inactive at this "
                             "setting. Pass 2+ to activate it; routine "
                             "determinism coverage otherwise lives in the "
                             "baseline capture (world_baseline.py) and the "
                             "hspec determinism test, so the gate doesn't "
                             "pay for it on every run.")
    parser.add_argument("--seeds-file", type=Path, default=SEEDS_FILE,
                        help=f"Seed config file (default: {SEEDS_FILE})")
    parser.add_argument("--seed", type=int,
                        help="Check only this specific seed")
    parser.add_argument("--quick", action="store_true",
                        help="Only run seeds tagged \"quick\": true in the "
                             "seeds file (~6 seeds, <1 min) — the iteration "
                             "tier. Run the full set before calling a "
                             "worldgen change done.")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Always show per-seed details, even on PASS")
    args = parser.parse_args()

    if not args.seeds_file.exists():
        print(f"error: seeds file not found: {args.seeds_file}", file=sys.stderr)
        return 2

    config = json.loads(args.seeds_file.read_text())
    seeds = config["seeds"]

    if args.quick:
        seeds = [s for s in seeds if s.get("quick")]
        if not seeds:
            print("error: no seeds tagged \"quick\": true in seeds file",
                  file=sys.stderr)
            return 2

    if args.seed is not None:
        seeds = [s for s in seeds if s["seed"] == args.seed]
        if not seeds:
            print(f"error: seed {args.seed} not in seeds file", file=sys.stderr)
            return 2

    print(f"World gen regression check: {len(seeds)} seed(s), {args.runs} runs each")
    if args.runs == 1:
        print("  content identity: each dump compared against its baseline's "
              "recorded hash")
        print("  determinism status: inactive at --runs 1 (needs 2+ dumps to "
              "observe run-to-run drift)")
    print()

    results: list[CheckResult] = []
    for entry in seeds:
        result = check_seed(entry, args.runs)
        results.append(result)
        print(format_result(result))
        if result.status != PASS or args.verbose:
            for line in format_details(result):
                print(line)

    print()
    counts: dict[str, int] = {}
    for r in results:
        counts[r.status] = counts.get(r.status, 0) + 1
    summary_parts = [f"{k}={v}" for k, v in sorted(counts.items())]
    print(f"Summary: {' '.join(summary_parts)} (total: {len(results)})")

    # Bug vs quality breakdown across all seeds.
    bug_fail = sum(1 for r in results for f in r.failures if f.startswith("BUG "))
    qual_fail = sum(1 for r in results for f in r.failures if f.startswith("QUALITY "))
    other_fail = sum(1 for r in results for f in r.failures
                      if not f.startswith("BUG ") and not f.startswith("QUALITY "))
    if bug_fail or qual_fail or other_fail:
        print(f"  Failure breakdown: bugs={bug_fail} quality={qual_fail} other={other_fail}")

    return 0 if counts.get(FAIL, 0) == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
