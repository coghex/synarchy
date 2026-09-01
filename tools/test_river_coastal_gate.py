#!/usr/bin/env python3
"""Self-test for the coastal-parallel gate in both river tools.

``river_thresholds.MAX_COASTAL_PARALLEL`` names a run LENGTH, and both
gates compare that quantity: the observed coastal value is the tile count
of the LONGEST cardinally connected component of high-elevation river
tiles adjacent to ocean — 0 when there are none — and a corpus passes
when that value is ``<=`` the threshold.

Before #1952 both tools got this wrong in different ways.
``test_river_pour.py`` compared the NUMBER of components that exceeded a
separately hard-coded length of 5, so one unbounded coastal run passed;
``test_river_stress.py`` did not measure the metric at all. The fixtures
below pin both directions and, crucially, the count-versus-length
distinction: the passing corpus holds MORE components than the threshold
allows as a count, and the failing corpus holds exactly one.

Every fixture is synthetic and ``--dump``-shaped, so this runs from a
clean checkout with no engine build and no world generation.

Usage:
    python3 tools/test_river_coastal_gate.py

Exit codes:
  0 = all tests passed
  1 = one or more tests failed
"""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

import river_thresholds as rt  # type: ignore
import test_river_pour as pour  # type: ignore
import test_river_stress as stress  # type: ignore

THRESHOLD = rt.MAX_COASTAL_PARALLEL

# A qualifying coastal river tile is a river above SEA_LEVEL + 5 with at
# least one cardinal ocean neighbour. HIGH_Z clears that elevation gate;
# OCEAN_SURF_Z keeps the ocean UNDERGROUND (fluidSurf < terrainZ), which
# is what stops the fixtures from also tripping the visible-drop metric.
HIGH_Z = 10
OCEAN_SURF_Z = 0

# Components are laid out this many rows apart. Each occupies two rows
# (river, then ocean beneath it), so a stride of 4 leaves at least two
# empty rows between any two components and nothing is cardinally
# adjacent across the gap.
COMPONENT_STRIDE = 4


# ── Fixtures ────────────────────────────────────────────────────
def tile(x, y, fluid_type, fluid_surf):
    return {
        "x": x, "y": y, "v": x + y,
        "terrainZ": HIGH_Z,
        "surfaceZ": max(HIGH_Z, fluid_surf),
        "fluidType": fluid_type,
        "fluidSurf": fluid_surf,
    }


def coastal_component(y0, length):
    """One connected run of `length` qualifying coastal river tiles.

    A river row at y0 with an underground ocean row beneath it: every
    tile is high, cardinally ocean-adjacent, and connected to its
    neighbours along the row.
    """
    tiles = []
    for i in range(length):
        tiles.append(tile(i, y0, "river", HIGH_Z))
        tiles.append(tile(i, y0 + 1, "ocean", OCEAN_SURF_Z))
    return tiles


def corpus(lengths):
    """A dump-shaped tile list holding one component per entry.

    Only river and ocean tiles are emitted, so the other three metrics
    read zero and any failure these fixtures produce is the coastal one.
    """
    tiles = []
    for index, length in enumerate(lengths):
        tiles.extend(coastal_component(index * COMPONENT_STRIDE, length))
    return tiles


def passing_lengths(threshold):
    """More components than the threshold allows as a COUNT, every one at
    or below it as a LENGTH, with one exactly AT it.

    This is the mutation pin: a gate that counted components instead of
    measuring the longest one calls this corpus a failure.
    """
    return [threshold] + [1 + (i % threshold) for i in range(threshold + 1)]


PASSING = corpus(passing_lengths(THRESHOLD))
FAILING = corpus([THRESHOLD + 1])
SHORT_RUN = 3  # comfortably under the old hard-coded floor of 5


# ── Harness ─────────────────────────────────────────────────────
FAILURES: list[str] = []


def check(condition, message):
    if not condition:
        FAILURES.append(message)


def run_pour(tiles, *extra_args):
    """Run test_river_pour.py over a fixture. Returns (exit code, stdout)."""
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "dump.json"
        path.write_text(json.dumps(tiles), encoding="utf-8")
        result = subprocess.run(
            [sys.executable, str(TOOLS / "test_river_pour.py"), str(path),
             *extra_args],
            capture_output=True, text=True, timeout=120,
        )
    return result.returncode, result.stdout


def coastal_line(stdout):
    """The gate's own coastal line, so assertions read what it printed."""
    for line in stdout.splitlines():
        if "coastal" in line.lower():
            return line.strip()
    return ""


def run_stress(tiles, *extra_args):
    """Gate a fixture through the stress tool's own flags. (passed, stats)."""
    args = stress.parse_args(list(extra_args))
    return stress.analyze_tiles(1, tiles, stress.thresholds_from_args(args))


# ── Tests: the single-seed gate ─────────────────────────────────
def test_pour_rejects_one_overlong_run():
    print("test_pour_rejects_one_overlong_run")
    code, out = run_pour(FAILING)
    line = coastal_line(out)
    check(code == 1, f"one {THRESHOLD + 1}-tile run should exit 1, got {code}")
    check("FAIL" in line, f"coastal line should FAIL, got: {line!r}")
    check(f"{THRESHOLD + 1} tiles" in line,
          f"coastal line should report {THRESHOLD + 1} tiles, got: {line!r}")
    check("RESULT: FAIL" in out, "verdict should be FAIL")


def test_pour_accepts_many_short_runs():
    print("test_pour_accepts_many_short_runs")
    lengths = passing_lengths(THRESHOLD)
    check(len(lengths) > THRESHOLD,
          "fixture must hold more components than the threshold as a count")
    check(max(lengths) == THRESHOLD,
          "fixture must hold one component exactly at the threshold")
    code, out = run_pour(PASSING)
    line = coastal_line(out)
    check(code == 0, f"{len(lengths)} runs of <= {THRESHOLD} tiles should "
                     f"exit 0, got {code}; coastal line: {line!r}")
    check("PASS" in line, f"coastal line should PASS, got: {line!r}")
    check(f"{THRESHOLD} tiles" in line,
          f"coastal line should report the longest run ({THRESHOLD} tiles), "
          f"not the component count ({len(lengths)}), got: {line!r}")
    check("RESULT: PASS" in out, f"verdict should be PASS, got:\n{out}")


def test_pour_names_the_quantity_it_gates():
    print("test_pour_names_the_quantity_it_gates")
    _, out = run_pour(PASSING)
    line = coastal_line(out)
    check("Longest coastal parallel" in line,
          f"coastal line should name the gated quantity, got: {line!r}")
    check("tiles" in line,
          f"coastal line should name its unit, got: {line!r}")
    check(f"(max {THRESHOLD})" in line,
          f"coastal line should show the threshold, got: {line!r}")


def test_pour_measures_runs_below_any_hardcoded_floor():
    print("test_pour_measures_runs_below_any_hardcoded_floor")
    # The old code discarded any component of 5 tiles or fewer before the
    # threshold was ever consulted. A short run must still be measured,
    # or the configurable threshold cannot tighten below that floor.
    fixture = corpus([SHORT_RUN])
    code, out = run_pour(fixture)
    line = coastal_line(out)
    check(code == 0, f"a {SHORT_RUN}-tile run should pass the default, got {code}")
    check(f"{SHORT_RUN} tiles" in line,
          f"a {SHORT_RUN}-tile run should be reported as {SHORT_RUN} tiles, "
          f"got: {line!r}")
    code, out = run_pour(fixture, "--max-coastal-parallel", str(SHORT_RUN - 1))
    check(code == 1, f"a {SHORT_RUN}-tile run should fail --max-coastal-parallel "
                     f"{SHORT_RUN - 1}, got {code}")


def test_pour_threshold_flag_changes_the_verdict():
    print("test_pour_threshold_flag_changes_the_verdict")
    code, _ = run_pour(FAILING, "--max-coastal-parallel", str(THRESHOLD + 1))
    check(code == 0, f"raising the flag to {THRESHOLD + 1} should pass, got {code}")
    code, _ = run_pour(PASSING, "--max-coastal-parallel", str(THRESHOLD - 1))
    check(code == 1, f"lowering the flag to {THRESHOLD - 1} should fail, got {code}")


def test_pour_reports_zero_without_coastal_tiles():
    print("test_pour_reports_zero_without_coastal_tiles")
    # Rivers beside ocean but BELOW the elevation gate qualify for nothing.
    tiles = []
    for i in range(20):
        tiles.append({"x": i, "y": 0, "v": i, "terrainZ": 1, "surfaceZ": 1,
                      "fluidType": "river", "fluidSurf": 1})
        tiles.append({"x": i, "y": 1, "v": i + 1, "terrainZ": 1, "surfaceZ": 1,
                      "fluidType": "ocean", "fluidSurf": 0})
    code, out = run_pour(tiles)
    line = coastal_line(out)
    check(code == 0, f"no qualifying tiles should pass, got {code}")
    check("0 tiles" in line, f"no qualifying tiles should report 0, got: {line!r}")


def test_pour_stdin_shape_is_unchanged():
    print("test_pour_stdin_shape_is_unchanged")
    result = subprocess.run(
        [sys.executable, str(TOOLS / "test_river_pour.py"), "-"],
        input=json.dumps(FAILING), capture_output=True, text=True, timeout=120,
    )
    check(result.returncode == 1,
          f"stdin should still be read and gated, got {result.returncode}")
    check("RESULT: FAIL" in result.stdout, "stdin verdict should be FAIL")


def test_pour_leaves_the_other_metrics_alone():
    print("test_pour_leaves_the_other_metrics_alone")
    _, out = run_pour(PASSING)
    for name, threshold in [("Visible drops", rt.MAX_VISIBLE_DROPS),
                            ("Dry gaps", rt.MAX_DRY_GAPS),
                            ("Mask consistency", rt.MAX_MASK_DRY)]:
        expected = f"PASS  {name}: 0 (max {threshold})"
        check(expected in out,
              f"the other three metrics must report counts unchanged; "
              f"missing {expected!r} in:\n{out}")


# ── Tests: the multi-seed gate ──────────────────────────────────
def test_stress_gates_the_longest_run():
    print("test_stress_gates_the_longest_run")
    passed, stats = run_stress(FAILING)
    check(not passed, f"one {THRESHOLD + 1}-tile run should fail the stress gate")
    check(stats["coastal"] == THRESHOLD + 1,
          f"stress should report {THRESHOLD + 1}, got {stats['coastal']}")
    passed, stats = run_stress(PASSING)
    check(passed, f"runs of <= {THRESHOLD} tiles should pass, stats={stats}")
    check(stats["coastal"] == THRESHOLD,
          f"stress should report the longest run ({THRESHOLD}), not the "
          f"component count ({len(passing_lengths(THRESHOLD))}), "
          f"got {stats['coastal']}")


def test_stress_threshold_flag_changes_the_verdict():
    print("test_stress_threshold_flag_changes_the_verdict")
    check(stress.parse_args([]).max_coastal_parallel == rt.MAX_COASTAL_PARALLEL,
          "--max-coastal-parallel must default to the shared threshold")
    passed, _ = run_stress(FAILING, "--max-coastal-parallel", str(THRESHOLD + 1))
    check(passed, f"raising the flag to {THRESHOLD + 1} should pass")
    passed, _ = run_stress(PASSING, "--max-coastal-parallel", str(THRESHOLD - 1))
    check(not passed, f"lowering the flag to {THRESHOLD - 1} should fail")


def test_stress_reports_coastal_on_every_surface():
    print("test_stress_reports_coastal_on_every_surface")
    thresholds = stress.thresholds_from_args(stress.parse_args([]))
    _, stats = stress.analyze_tiles(1, FAILING, thresholds)

    banner = stress.format_threshold_banner(thresholds)
    check(f"coastal<={THRESHOLD}" in banner,
          f"threshold banner should name the coastal gate, got: {banner!r}")

    seed_line = stress.format_seed_line(1, 1, 1, stats, False, 0.0)
    check(f"coastal={THRESHOLD + 1}" in seed_line,
          f"per-seed line should report coastal, got: {seed_line!r}")

    failure_line = stress.format_failure_line(stats)
    check(f"coastal={THRESHOLD + 1}" in failure_line,
          f"failure detail should report coastal, got: {failure_line!r}")

    aggregate = "\n".join(stress.format_aggregate([stats]))
    check("coastal" in aggregate.lower(),
          f"aggregate results should report coastal, got:\n{aggregate}")


def test_stress_help_lists_the_flag():
    print("test_stress_help_lists_the_flag")
    result = subprocess.run(
        [sys.executable, str(TOOLS / "test_river_stress.py"), "--help"],
        capture_output=True, text=True, timeout=120,
    )
    check(result.returncode == 0, f"--help should exit 0, got {result.returncode}")
    check("--max-coastal-parallel" in result.stdout,
          f"--help should list --max-coastal-parallel, got:\n{result.stdout}")


def test_both_gates_share_one_measurement():
    print("test_both_gates_share_one_measurement")
    # Not a style preference: two independent implementations are what let
    # the two tools disagree about this metric in the first place.
    check(stress.check_coastal_parallels is pour.check_coastal_parallels,
          "the stress tool must reuse the single-seed component analysis")
    check(stress.longest_coastal_parallel is pour.longest_coastal_parallel,
          "the stress tool must reuse the single-seed observed value")


def test_component_analysis_applies_no_minimum():
    print("test_component_analysis_applies_no_minimum")
    grid = pour.build_grid(corpus([1, SHORT_RUN]))
    runs = pour.check_coastal_parallels(grid)
    check(sorted(r["size"] for r in runs) == [1, SHORT_RUN],
          f"every component must be returned regardless of length, got {runs}")
    check(pour.longest_coastal_parallel(runs) == SHORT_RUN,
          f"observed value should be {SHORT_RUN}, "
          f"got {pour.longest_coastal_parallel(runs)}")
    check(pour.longest_coastal_parallel([]) == 0,
          "no components must read as 0, not as an error")


TESTS = [
    test_pour_rejects_one_overlong_run,
    test_pour_accepts_many_short_runs,
    test_pour_names_the_quantity_it_gates,
    test_pour_measures_runs_below_any_hardcoded_floor,
    test_pour_threshold_flag_changes_the_verdict,
    test_pour_reports_zero_without_coastal_tiles,
    test_pour_stdin_shape_is_unchanged,
    test_pour_leaves_the_other_metrics_alone,
    test_stress_gates_the_longest_run,
    test_stress_threshold_flag_changes_the_verdict,
    test_stress_reports_coastal_on_every_surface,
    test_stress_help_lists_the_flag,
    test_both_gates_share_one_measurement,
    test_component_analysis_applies_no_minimum,
]


def main():
    for test in TESTS:
        test()
    print()
    if FAILURES:
        print(f"FAILED ({len(FAILURES)}):")
        for failure in FAILURES:
            print(f"  - {failure}")
        sys.exit(1)
    print(f"All {len(TESTS)} coastal-gate tests passed "
          f"(threshold {THRESHOLD} tiles).")
    sys.exit(0)


if __name__ == "__main__":
    main()
