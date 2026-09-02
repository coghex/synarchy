#!/usr/bin/env python3
"""The baseline content-hash gate (#1361), split out by #2070.

These drive the real world_check.check_seed over synthetic dumps rather
than re-implementing hash equality here: the baseline fixture is built
by the real world_baseline.capture_seed, and every pass/fail decision
below is made by production code. A test that compared hashes itself
would still pass if check_seed stopped calling check_baseline_hash.

The four groups: an exact match passes; aggregate-preserving tile drift
fails; a racy baseline is announced rather than gated; an internally
inconsistent baseline is rejected. The audit-clean fixtures they share
with two other owners come from `test_audit_support`; the on-disk
`run_check_seed` harness has no other consumer and lives here.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_audit.py
"""
from __future__ import annotations

import json
import sys
import tempfile
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).resolve().parent))
from world_audit import audit_dump  # type: ignore  # noqa: E402
import world_check  # type: ignore  # noqa: E402
from world_check import (  # type: ignore  # noqa: E402
    CheckResult, check_seed, format_result, PASS, FAIL,
)
from test_audit_support import (  # noqa: E402
    HASH_ENTRY, capture_hash_baseline, expect, hash_dump_fixture,
)


def run_check_seed(baseline: dict[str, Any],
                   current: list[list[dict[str, Any]]]) -> CheckResult:
    """Run the production check_seed against a real on-disk baseline."""
    pending = list(current)
    original_run = world_check.run_dump
    original_path = world_check.baseline_path
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "baseline.json"
        path.write_text(json.dumps(baseline, indent=2) + "\n")
        world_check.run_dump = lambda *a, **k: pending.pop(0)
        world_check.baseline_path = lambda *a, **k: path
        try:
            return check_seed(HASH_ENTRY, runs=len(current))
        finally:
            world_check.run_dump = original_run
            world_check.baseline_path = original_path


def test_baseline_hash_match_passes() -> None:
    """A dump reproducing its baseline's recorded hash passes."""
    print("test_baseline_hash_match_passes")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    expect(baseline["determinism"]["distinctHashes"] == 1,
           f"fixture baseline should be deterministic, got "
           f"{baseline['determinism']['distinctHashes']} distinct hashes")

    r = run_check_seed(baseline, [hash_dump_fixture()])
    expect(r.status == PASS, f"matching hash should PASS, got {r.status}: {r.failures}")
    expect(not r.failures, f"no failures expected, got {r.failures}")
    expect(not r.banners,
           f"a gated deterministic seed needs no banner, got {r.banners}")


def test_baseline_hash_mismatch_fails_on_aggregate_preserving_drift() -> None:
    """A matId change no statistic models still fails, naming the seed.

    This is the false-pass class the gate was added for: every existing
    comparison sees identical values, so only the content hash can flag it.
    """
    print("test_baseline_hash_mismatch_fails_on_aggregate_preserving_drift")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    drifted = hash_dump_fixture(matId_at_index=(7, 70))

    # The drift really is invisible to every other comparison.
    a = audit_dump(clean, seed=HASH_ENTRY["seed"])
    b = audit_dump(drifted, seed=HASH_ENTRY["seed"])
    expect(a.tile_count == b.tile_count
           and a.elevation_stats == b.elevation_stats
           and a.fluid_stats == b.fluid_stats
           and a.summary() == b.summary(),
           "matId fixture must be aggregate-preserving for this test to mean "
           f"anything: {a.summary()} vs {b.summary()}")

    r = run_check_seed(baseline, [drifted])
    expect(r.status == FAIL, f"content drift should FAIL, got {r.status}")
    hash_failures = [f for f in r.failures if "content hash mismatch" in f]
    expect(len(hash_failures) == 1,
           f"expected exactly one content-hash failure, got {r.failures}")
    if hash_failures:
        message = hash_failures[0]
        expect(f"seed={HASH_ENTRY['seed']}" in message,
               f"failure must name the seed, got {message!r}")
        expect(baseline["determinism"]["hashes"][0] in message,
               f"failure must expose the expected hash, got {message!r}")
        expect(world_check.hash_dump(drifted) in message,
               f"failure must expose the actual hash, got {message!r}")


def test_baseline_hash_racy_baseline_is_announced_not_gated() -> None:
    """A multi-hash baseline is neither silently passed nor silently failed."""
    print("test_baseline_hash_racy_baseline_is_announced_not_gated")
    clean = hash_dump_fixture()
    variant = hash_dump_fixture(matId_at_index=(7, 70))
    baseline = capture_hash_baseline([clean, variant, clean])
    expect(baseline["determinism"]["distinctHashes"] == 2,
           f"fixture baseline should be racy, got "
           f"{baseline['determinism']['distinctHashes']} distinct hashes")

    # A current dump matching NEITHER recorded hash: three samples of a
    # race do not enumerate its outcomes, so this must not fail.
    unseen = hash_dump_fixture(matId_at_index=(11, 71))
    expect(world_check.hash_dump(unseen)
           not in baseline["determinism"]["hashes"],
           "the racy-case fixture must not accidentally match a recorded hash")

    r = run_check_seed(baseline, [unseen])
    expect(not any("content hash" in f for f in r.failures),
           f"a racy baseline must not gate content identity, got {r.failures}")
    banners = [b for b in r.banners if "racy baseline" in b]
    expect(len(banners) == 1,
           f"expected one racy-baseline banner, got {r.banners}")

    # ...and the banner must survive the normal, non-verbose output, which
    # prints format_result only. Notes are suppressed on a PASS, so a note
    # would be exactly the silent pass this rule forbids.
    line = format_result(r)
    expect(all(b in line for b in banners),
           f"banner must ride the seed's own output line: {line!r}")


def test_baseline_hash_inconsistent_baseline_fails() -> None:
    """A baseline whose deterministic flag contradicts its hashes fails."""
    print("test_baseline_hash_inconsistent_baseline_fails")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    baseline["determinism"]["deterministic"] = False

    r = run_check_seed(baseline, [hash_dump_fixture()])
    expect(r.status == FAIL,
           f"a self-contradictory baseline should FAIL, got {r.status}")
    expect(any("malformed baseline" in f for f in r.failures),
           f"expected a malformed-baseline failure, got {r.failures}")


#: This owner's inventory, in the relative order these groups hold
#: within the aggregate's run sequence. `tools/test_audit.py` composes
#: that sequence from every owner's inventory; nothing here decides
#: when, or whether, it runs.
TESTS = (
    test_baseline_hash_match_passes,
    test_baseline_hash_mismatch_fails_on_aggregate_preserving_drift,
    test_baseline_hash_racy_baseline_is_announced_not_gated,
    test_baseline_hash_inconsistent_baseline_fails,
)
