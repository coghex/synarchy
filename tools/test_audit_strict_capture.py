#!/usr/bin/env python3
"""world_baseline.py's strict capture invariants (#1598), split out by #2070.

world_check.py compares tileCount and elevationStats min/max/median/count
for exact equality with no envelope, so world_baseline.py must refuse to
publish a seed whose capture runs disagreed on any of them rather than
recording run 0. These drive the real world_baseline.capture_seed and the
real world_baseline.main() over synthetic dumps — no world is generated.

All five invariants are pinned individually. The two tools hard-code the
strict set separately (Requirement 7 forbids touching world_check.py), so
only per-invariant coverage catches the two drifting apart again.

The nine groups: a stable seed is captured; a varying tile count and each
varying elevation statistic are refused; every violated invariant is
reported at once; an absent elevation domain is a failure, not a
TypeError; a hash-racy but structurally stable seed is still captured; a
refused single-seed capture writes nothing and exits nonzero; an
existing baseline is left byte-identical; and one refusal does not abort
the other seeds. The `run_world_baseline_main` harness and the strict
fixtures have no other consumer and live here.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_audit.py
"""
from __future__ import annotations

import contextlib
import io
import json
import sys
import tempfile
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).resolve().parent))
from world_audit import audit_dump  # type: ignore  # noqa: E402
import world_baseline  # type: ignore  # noqa: E402
from test_audit_support import (  # noqa: E402
    expect, expect_exit, expect_output_contains, hash_dump_fixture,
)


STRICT_SEED = 5150


STRICT_ENTRY = {"seed": STRICT_SEED, "world_size": 32, "region": [-1, -1, 1, 1],
                "description": "strict invariant fixture", "quick": True}


STRICT_ENTRY_B = {"seed": 5151, "world_size": 32, "region": [-1, -1, 1, 1],
                  "description": "second strict fixture", "quick": True}


def strict_tile(x: int, y: int, terrainZ: int = 10,
                beyondGlacier: bool = False) -> dict[str, Any]:
    """A dry, flat, audit-clean tile at an explicit elevation."""
    return {
        "x": x, "y": y, "v": x + y,
        "terrainZ": terrainZ, "surfaceZ": terrainZ,
        "matId": 64,
        "fluidType": None, "fluidSurf": None,
        "iceSurf": None, "iceMode": None,
        "glacierZone": False, "beyondGlacier": beyondGlacier,
    }


def strict_dump(elevations: list[int],
                excluded: int = 0) -> list[dict[str, Any]]:
    """A dump whose real-terrain elevations are exactly `elevations`.

    `excluded` appends tiles marked beyondGlacier, which
    world_audit.compute_stats filters out of the elevation domain. That
    separation is what lets each invariant be varied on its own: tileCount
    can move while every elevation statistic holds still, and vice versa.
    """
    tiles = [strict_tile(i, 0, terrainZ=z) for i, z in enumerate(elevations)]
    tiles += [strict_tile(i, 1, beyondGlacier=True) for i in range(excluded)]
    return tiles


def capture_strict(dumps: list[list[dict[str, Any]]],
                   seed: int = STRICT_SEED) -> dict[str, Any]:
    """Run the real world_baseline.capture_seed over `dumps`."""
    pending = list(dumps)
    original = world_baseline.run_dump
    world_baseline.run_dump = lambda *a, **k: pending.pop(0)
    try:
        with contextlib.redirect_stderr(io.StringIO()):
            return world_baseline.capture_seed(
                seed, STRICT_ENTRY["world_size"],
                tuple(STRICT_ENTRY["region"]), len(dumps))
    finally:
        world_baseline.run_dump = original


def expect_strict_capture_failure(label: str,
                                  dumps: list[list[dict[str, Any]]],
                                  invariant: str,
                                  values: list[Any]) -> str:
    """Assert capture refuses `dumps`, naming seed, invariant and values."""
    try:
        capture_strict(dumps)
    except RuntimeError as exc:
        message = str(exc)
    else:
        expect(False, f"{label}: varying {invariant} must refuse the baseline")
        return ""

    expect(f"seed {STRICT_SEED}" in message,
           f"{label}: failure must name the seed, got {message!r}")
    expect(invariant in message,
           f"{label}: failure must name {invariant}, got {message!r}")
    for value in values:
        expect(repr(value) in message,
               f"{label}: failure must report observed {value!r}, "
               f"got {message!r}")
    return message


def test_strict_capture_accepts_a_stable_seed() -> None:
    """An unvarying capture is written exactly as before.

    The fixtures below differ from this one in a single invariant each, so
    this case is what proves a refusal is caused by that difference rather
    than by the new check rejecting everything.
    """
    print("test_strict_capture_accepts_a_stable_seed")
    stable = strict_dump([10, 10, 10, 20, 20, 20], excluded=2)
    baseline = capture_strict([stable, stable, stable])
    expect(baseline["tileCount"] == 8,
           f"stable capture records its tile count, got {baseline['tileCount']}")
    expect(baseline["elevationStats"] == {"min": 10, "max": 20,
                                          "median": 15, "count": 6},
           f"stable capture records its elevation stats, got "
           f"{baseline['elevationStats']}")


def test_strict_capture_refuses_varying_tile_count() -> None:
    """tileCount is the first field world_check.py compares exactly."""
    print("test_strict_capture_refuses_varying_tile_count")
    base = strict_dump([10] * 6)
    varied = strict_dump([10] * 6, excluded=1)

    # The extra tile really is invisible to every elevation statistic, so
    # the refusal below can only be attributable to tileCount.
    a = audit_dump(base, seed=STRICT_SEED)
    b = audit_dump(varied, seed=STRICT_SEED)
    expect(a.elevation_stats == b.elevation_stats,
           f"tileCount fixture must isolate tileCount: {a.elevation_stats} "
           f"vs {b.elevation_stats}")

    message = expect_strict_capture_failure(
        "tileCount", [base, varied, base], "tileCount", [6, 7])
    expect("elevationStats" not in message,
           f"only the varying invariant should be reported, got {message!r}")


def test_strict_capture_refuses_each_varying_elevation_statistic() -> None:
    """min, max, median and count each refuse on their own.

    Capture warned about min and max only before #1598; median and count
    reached the baseline as run 0's value with no warning at all. Each key
    is varied in isolation so a check covering three of the four still
    fails here.
    """
    print("test_strict_capture_refuses_each_varying_elevation_statistic")
    cases = [
        ("min", strict_dump([10] * 6), strict_dump([5] + [10] * 5), [5, 10]),
        ("max", strict_dump([10] * 6), strict_dump([15] + [10] * 5), [10, 15]),
        ("median", strict_dump([10, 10, 10, 20, 20, 20]),
         strict_dump([10, 10, 10, 10, 20, 20]), [10, 15]),
        ("count", strict_dump([10] * 6, excluded=1), strict_dump([10] * 7),
         [6, 7]),
    ]
    for key, base, varied, values in cases:
        # Each fixture pair differs in exactly this one statistic.
        a = audit_dump(base, seed=STRICT_SEED).elevation_stats
        b = audit_dump(varied, seed=STRICT_SEED).elevation_stats
        differing = sorted(k for k in ("min", "max", "median", "count")
                           if a.get(k) != b.get(k))
        expect(differing == [key],
               f"elevationStats.{key} fixture must isolate {key}, "
               f"differs in {differing} ({a} vs {b})")
        expect(audit_dump(base, seed=STRICT_SEED).tile_count
               == audit_dump(varied, seed=STRICT_SEED).tile_count,
               f"elevationStats.{key} fixture must hold tileCount still")

        expect_strict_capture_failure(
            key, [base, varied, base], f"elevationStats.{key}", values)


def test_strict_capture_reports_every_violated_invariant() -> None:
    """A capture varying in several invariants names all of them."""
    print("test_strict_capture_reports_every_violated_invariant")
    message = expect_strict_capture_failure(
        "all five", [strict_dump([10] * 6), strict_dump([5] * 7)],
        "tileCount", [6, 7])
    for key in ("min", "max", "median", "count"):
        expect(f"elevationStats.{key}" in message,
               f"every violated invariant must be named, {key} missing from "
               f"{message!r}")


def test_strict_capture_handles_an_absent_elevation_domain() -> None:
    """None and int observations report a failure, not a TypeError.

    world_audit.compute_stats returns None for min/max/median when a
    region holds no real terrain, and sorted() over a set mixing None with
    ints raises TypeError — which would replace the required failure with
    a crash on exactly the capture that most needs reporting.
    """
    print("test_strict_capture_handles_an_absent_elevation_domain")
    empty = strict_dump([], excluded=6)
    expect(audit_dump(empty, seed=STRICT_SEED).elevation_stats
           == {"min": None, "max": None, "median": None, "count": 0},
           "the empty-domain fixture must really produce None statistics")

    message = expect_strict_capture_failure(
        "absent domain", [strict_dump([10] * 6), empty],
        "elevationStats.min", [None, 10])
    expect("elevationStats.count" in message,
           f"count varies here too and must be named, got {message!r}")


def test_strict_capture_still_allows_a_hash_racy_seed() -> None:
    """Recorded-hash raciness is untouched when the strict fields hold.

    #1361's policy records the distinct hashes and lets world_check.py
    downgrade its content-identity gate; that is a different question from
    sampling a field the checker compares exactly.
    """
    print("test_strict_capture_still_allows_a_hash_racy_seed")
    clean = hash_dump_fixture()
    variant = hash_dump_fixture(matId_at_index=(7, 70))
    baseline = capture_strict([clean, variant, clean])
    expect(baseline["determinism"]["distinctHashes"] == 2,
           f"a hash-racy seed is still captured, got "
           f"{baseline['determinism']['distinctHashes']} distinct hashes")


def run_world_baseline_main(seeds: list[dict[str, Any]], argv: list[str],
                            dumps_by_seed: dict[int, list[list[dict[str, Any]]]],
                            baseline_dir: Path) -> tuple[int, str]:
    """Run world_baseline.main() against a temporary seeds file and dir.

    `dumps_by_seed` maps each seed to the dumps its capture runs return,
    so a seed can be made to vary without generating a world. An
    unqueued seed raises AssertionError rather than RuntimeError, which
    main() would otherwise absorb as an ordinary capture failure and hide
    a broken fixture.
    """
    pending = {seed: list(dumps) for seed, dumps in dumps_by_seed.items()}
    original_run = world_baseline.run_dump
    original_dir = world_baseline.BASELINE_DIR
    original_argv = sys.argv

    def fake_run_dump(seed: int, *a: Any, **k: Any) -> list[dict[str, Any]]:
        queue = pending.get(seed)
        if not queue:
            raise AssertionError(f"test: no dump queued for seed {seed}")
        return queue.pop(0)

    with tempfile.TemporaryDirectory() as tmp:
        seeds_file = Path(tmp) / "_seeds.json"
        seeds_file.write_text(json.dumps({"seeds": seeds}))
        world_baseline.run_dump = fake_run_dump
        world_baseline.BASELINE_DIR = baseline_dir
        sys.argv = (["world_baseline.py", "--seeds-file", str(seeds_file)]
                    + argv)
        captured = io.StringIO()
        try:
            with contextlib.redirect_stdout(captured), \
                    contextlib.redirect_stderr(captured):
                code = world_baseline.main()
        finally:
            world_baseline.run_dump = original_run
            world_baseline.BASELINE_DIR = original_dir
            sys.argv = original_argv
    return code, captured.getvalue()


def strict_baseline_file(baseline_dir: Path, entry: dict[str, Any]) -> Path:
    original_dir = world_baseline.BASELINE_DIR
    world_baseline.BASELINE_DIR = baseline_dir
    try:
        return world_baseline.baseline_path(
            entry["seed"], entry["world_size"], tuple(entry["region"]))
    finally:
        world_baseline.BASELINE_DIR = original_dir


def test_strict_capture_single_seed_writes_nothing_and_exits_nonzero() -> None:
    """--seed N on a varying seed leaves no file behind and fails."""
    print("test_strict_capture_single_seed_writes_nothing_and_exits_nonzero")
    with tempfile.TemporaryDirectory() as tmp:
        baseline_dir = Path(tmp)
        target = strict_baseline_file(baseline_dir, STRICT_ENTRY)
        code, output = run_world_baseline_main(
            [STRICT_ENTRY], ["--seed", str(STRICT_SEED), "--runs", "2"],
            {STRICT_SEED: [strict_dump([10] * 6), strict_dump([10] * 7)]},
            baseline_dir)
        expect_exit(code, 1, output, "a varying single seed fails")
        expect(not target.exists(),
               f"no baseline may be created for a refused seed, found {target}")
        expect_output_contains("1 failures", output, "the failure count")
        expect_output_contains(f"seed {STRICT_SEED}", output, "the seed")


def test_strict_capture_leaves_an_existing_baseline_byte_identical() -> None:
    """A refused seed's tracked baseline is not touched."""
    print("test_strict_capture_leaves_an_existing_baseline_byte_identical")
    with tempfile.TemporaryDirectory() as tmp:
        baseline_dir = Path(tmp)
        target = strict_baseline_file(baseline_dir, STRICT_ENTRY)
        sentinel = '{"tileCount": "do not touch"}\n'
        target.write_text(sentinel)

        code, output = run_world_baseline_main(
            [STRICT_ENTRY], ["--runs", "2"],
            {STRICT_SEED: [strict_dump([10] * 6), strict_dump([5] + [10] * 5)]},
            baseline_dir)
        expect_exit(code, 1, output, "a varying seed fails")
        expect(target.read_text() == sentinel,
               f"the existing baseline must be byte-identical, got "
               f"{target.read_text()!r}")


def test_strict_capture_failure_does_not_abort_the_other_seeds() -> None:
    """One refused seed still leaves the rest captured, and the run fails."""
    print("test_strict_capture_failure_does_not_abort_the_other_seeds")
    with tempfile.TemporaryDirectory() as tmp:
        baseline_dir = Path(tmp)
        refused = strict_baseline_file(baseline_dir, STRICT_ENTRY)
        written = strict_baseline_file(baseline_dir, STRICT_ENTRY_B)
        stable = strict_dump([10] * 6)

        code, output = run_world_baseline_main(
            [STRICT_ENTRY, STRICT_ENTRY_B], ["--runs", "2"],
            {STRICT_SEED: [stable, strict_dump([15] + [10] * 5)],
             STRICT_ENTRY_B["seed"]: [stable, stable]},
            baseline_dir)
        expect_exit(code, 1, output, "one refused seed fails the run")
        expect(not refused.exists(),
               f"the refused seed writes nothing, found {refused}")
        expect(written.exists(),
               f"a later seed is still captured, {written} missing")
        expect_output_contains("Captured 1 baselines, 1 failures", output,
                               "the per-run tally")


#: This owner's inventory, in the relative order these groups hold
#: within the aggregate's run sequence. `tools/test_audit.py` composes
#: that sequence from every owner's inventory; nothing here decides
#: when, or whether, it runs.
TESTS = (
    test_strict_capture_accepts_a_stable_seed,
    test_strict_capture_refuses_varying_tile_count,
    test_strict_capture_refuses_each_varying_elevation_statistic,
    test_strict_capture_reports_every_violated_invariant,
    test_strict_capture_handles_an_absent_elevation_domain,
    test_strict_capture_still_allows_a_hash_racy_seed,
    test_strict_capture_single_seed_writes_nothing_and_exits_nonzero,
    test_strict_capture_leaves_an_existing_baseline_byte_identical,
    test_strict_capture_failure_does_not_abort_the_other_seeds,
)
