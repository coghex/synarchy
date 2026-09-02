#!/usr/bin/env python3
"""world_check.py's missing-baseline exit policy (#1319), split out by #2070.

These drive the real world_check.main() over a temporary seeds file, so
the assertion is about the process's own exit status and printed output
rather than a re-implementation of the rule. No engine is booted: a
selected seed with no baseline returns before run_dump, and run_dump is
replaced by a recorder that fails loudly if it is reached at all.

The seven groups: the retained SKIP disposition; every selection path
fails by default; every missing entry is reported; the
`--allow-missing-baselines` escape hatch tolerates a clean skip run but
never masks an ordinary failure; a fully baselined run still exits 0; and
an invalid selection keeps its exit 2. The `run_world_check_main` harness
has no other consumer and lives here.

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
import world_baseline  # type: ignore  # noqa: E402
import world_check  # type: ignore  # noqa: E402
from world_check import check_seed, SKIP  # type: ignore  # noqa: E402
from test_audit_support import (  # noqa: E402
    HASH_ENTRY, capture_hash_baseline, expect, expect_exit,
    expect_output_contains, hash_dump_fixture,
)


MISSING_A = {"seed": 987654321, "world_size": 32, "region": [-4, -4, 4, 4],
             "description": "no baseline exists", "quick": True}


MISSING_B = {"seed": 987654322, "world_size": 64, "region": [-2, -2, 2, 2],
             "description": "also no baseline", "quick": True}


HASH_SEED_ENTRY = dict(HASH_ENTRY, description="drifts against its baseline",
                       quick=True)


def run_world_check_main(seeds: list[dict[str, Any]], argv: list[str],
                         baselines: dict[int, dict[str, Any]] | None = None,
                         dumps: list[list[dict[str, Any]]] | None = None
                         ) -> tuple[int, str, list[Any]]:
    """Run world_check.main() against a temporary seed selection.

    `baselines` maps a seed to a baseline document written to a temp file
    and served for that seed only; every other seed resolves through the
    real baseline_path, so a fake seed genuinely has no baseline on disk
    and its expected filename is the one the production naming rule
    generates. Returns (exit code, combined stdout+stderr, dump calls).
    """
    calls: list[Any] = []
    pending = list(dumps or [])
    original_run = world_check.run_dump
    original_path = world_check.baseline_path

    def fake_run_dump(*a: Any, **k: Any) -> list[dict[str, Any]]:
        calls.append(a)
        if not pending:
            raise RuntimeError("test: no world generation was expected here")
        return pending.pop(0)

    with tempfile.TemporaryDirectory() as tmp:
        seeds_file = Path(tmp) / "_seeds.json"
        seeds_file.write_text(json.dumps({"seeds": seeds}))
        overrides: dict[int, Path] = {}
        for seed, document in (baselines or {}).items():
            path = Path(tmp) / f"baseline_{seed}.json"
            path.write_text(json.dumps(document, indent=2) + "\n")
            overrides[seed] = path

        def fake_baseline_path(seed: int, world_size: int,
                               region: tuple[int, int, int, int]) -> Path:
            if seed in overrides:
                return overrides[seed]
            return original_path(seed, world_size, region)

        original_argv = sys.argv
        world_check.run_dump = fake_run_dump
        world_check.baseline_path = fake_baseline_path
        sys.argv = ["world_check.py", "--seeds-file", str(seeds_file)] + argv
        captured = io.StringIO()
        try:
            with contextlib.redirect_stdout(captured), \
                    contextlib.redirect_stderr(captured):
                code = world_check.main()
        finally:
            world_check.run_dump = original_run
            world_check.baseline_path = original_path
            sys.argv = original_argv
    return code, captured.getvalue(), calls


def expected_baseline_name(entry: dict[str, Any]) -> str:
    return world_baseline.baseline_path(
        entry["seed"], entry["world_size"], tuple(entry["region"])).name


def expect_missing_baseline_failure(entry: dict[str, Any], argv: list[str],
                                    label: str) -> None:
    code, output, calls = run_world_check_main([entry], argv)
    expect_exit(code, 1, output,
                f"{label}: a selected seed with no baseline fails")
    expect_output_contains(str(entry["seed"]), output, f"{label}: seed")
    expect_output_contains(expected_baseline_name(entry), output,
                           f"{label}: expected baseline file")
    expect(not calls,
           f"{label}: a seed with no baseline must not be generated, "
           f"run_dump was called {len(calls)} time(s)")


def test_missing_baseline_fails_every_selection_path() -> None:
    """Unfiltered, --quick and --seed N all fail on a missing baseline.

    The three paths are separate filters over the same seed list, so a
    fix applied to one of them would leave the other two green.
    """
    print("test_missing_baseline_fails_every_selection_path")
    expect_missing_baseline_failure(MISSING_A, [], "unfiltered")
    expect_missing_baseline_failure(MISSING_A, ["--quick"], "--quick")
    expect_missing_baseline_failure(
        MISSING_A, ["--seed", str(MISSING_A["seed"])], "--seed N")


def test_missing_baseline_reports_every_entry() -> None:
    """Two missing baselines are both named, not just the first."""
    print("test_missing_baseline_reports_every_entry")
    code, output, calls = run_world_check_main([MISSING_A, MISSING_B], [])
    expect_exit(code, 1, output, "two missing baselines fail")
    for entry in (MISSING_A, MISSING_B):
        expect_output_contains(expected_baseline_name(entry), output,
                               f"seed {entry['seed']} expected baseline")
    expect(not calls,
           f"no seed should have been generated, run_dump was called "
           f"{len(calls)} time(s)")


def test_allow_missing_baselines_tolerates_a_clean_skip_run() -> None:
    """The opt-in flag reports the skipped seeds and exits zero."""
    print("test_allow_missing_baselines_tolerates_a_clean_skip_run")
    code, output, calls = run_world_check_main(
        [MISSING_A, MISSING_B], ["--allow-missing-baselines"])
    expect_exit(code, 0, output,
                "--allow-missing-baselines tolerates an all-SKIP run")
    expect_output_contains("SKIP=2", output, "summary still counts the skips")
    for entry in (MISSING_A, MISSING_B):
        expect_output_contains(expected_baseline_name(entry), output,
                               f"tolerated seed {entry['seed']}")
    expect(not calls,
           f"the tolerant path still generates nothing, run_dump was called "
           f"{len(calls)} time(s)")


def test_allow_missing_baselines_does_not_mask_a_real_failure() -> None:
    """The flag narrows the missing-baseline cause and nothing else.

    A run holding both a missing baseline and an ordinary regression
    still exits 1 under the flag — otherwise the local-exploration
    escape hatch would be a way to pass a genuinely failing gate.
    """
    print("test_allow_missing_baselines_does_not_mask_a_real_failure")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    drifted = hash_dump_fixture((0, 70))

    code, output, _ = run_world_check_main(
        [HASH_SEED_ENTRY, MISSING_A], ["--allow-missing-baselines"],
        baselines={HASH_SEED_ENTRY["seed"]: baseline}, dumps=[drifted])
    expect_exit(code, 1, output,
                "--allow-missing-baselines does not mask an ordinary FAIL")
    expect_output_contains("FAIL=1 SKIP=1", output, "both dispositions")

    # Same seed, undrifted: the flag really does pass the rest of the run,
    # so the exit 1 above is the FAIL and not the flag failing to apply.
    code, output, _ = run_world_check_main(
        [HASH_SEED_ENTRY, MISSING_A], ["--allow-missing-baselines"],
        baselines={HASH_SEED_ENTRY["seed"]: baseline},
        dumps=[hash_dump_fixture()])
    expect_exit(code, 0, output,
                "a passing seed beside a tolerated missing baseline")


def test_clean_run_still_exits_zero() -> None:
    """A fully baselined, fully passing selection is unaffected."""
    print("test_clean_run_still_exits_zero")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    code, output, calls = run_world_check_main(
        [HASH_SEED_ENTRY], [], baselines={HASH_SEED_ENTRY["seed"]: baseline},
        dumps=[hash_dump_fixture()])
    expect_exit(code, 0, output, "a fully baselined passing run")
    expect_output_contains("PASS=1", output, "the seed passes")
    expect(len(calls) == 1,
           f"a baselined seed is generated exactly once at --runs 1, "
           f"run_dump was called {len(calls)} time(s)")


def test_bad_selections_keep_their_exit_two() -> None:
    """Empty selections are still invocation errors, not missing baselines.

    Both messages are pinned verbatim: the new exit policy sits after
    these returns, and folding an empty selection into it would turn a
    typo into a regression report.
    """
    print("test_bad_selections_keep_their_exit_two")
    code, output, _ = run_world_check_main(
        [dict(MISSING_A, quick=False)], ["--quick"])
    expect_exit(code, 2, output, "--quick with no quick-tagged seeds")
    expect_output_contains('error: no seeds tagged "quick": true in seeds file',
                           output, "unchanged empty-quick message")

    code, output, _ = run_world_check_main([MISSING_A], ["--seed", "1234567"])
    expect_exit(code, 2, output, "--seed for an absent seed")
    expect_output_contains("error: seed 1234567 not in seeds file", output,
                           "unchanged absent-seed message")


def test_missing_baseline_keeps_its_skip_disposition() -> None:
    """check_seed still reports SKIP and records the path it wanted.

    The exit policy names the recorded path rather than the SKIP string,
    so both halves have to hold for the run to fail for the right reason.
    """
    print("test_missing_baseline_keeps_its_skip_disposition")
    r = check_seed(MISSING_A, runs=1)
    expect(r.status == SKIP,
           f"a missing baseline stays externally visible as SKIP, got {r.status}")
    expect(r.missing_baseline is not None
           and r.missing_baseline.name == expected_baseline_name(MISSING_A),
           f"the expected baseline path must be recorded, got "
           f"{r.missing_baseline}")
    expect(world_check.exit_status([r], allow_missing_baselines=False) == 1,
           "a recorded missing baseline must fail by default")
    expect(world_check.exit_status([r], allow_missing_baselines=True) == 0,
           "a recorded missing baseline must be tolerated under the flag")


#: This owner's inventory, in the relative order these groups hold
#: within the aggregate's run sequence. `tools/test_audit.py` composes
#: that sequence from every owner's inventory; nothing here decides
#: when, or whether, it runs.
TESTS = (
    test_missing_baseline_keeps_its_skip_disposition,
    test_missing_baseline_fails_every_selection_path,
    test_missing_baseline_reports_every_entry,
    test_allow_missing_baselines_tolerates_a_clean_skip_run,
    test_allow_missing_baselines_does_not_mask_a_real_failure,
    test_clean_run_still_exits_zero,
    test_bad_selections_keep_their_exit_two,
)
