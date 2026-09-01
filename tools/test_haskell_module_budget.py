#!/usr/bin/env python3
"""Unit tests for haskell_module_budget.py (issue #967 acceptance: the
guard's directory patterns recurse into nested split modules, not just
direct children).

Mirrors tools/test_engine_env_capability_audit.py's own approach: import
the real guard's `check()` and drive it over synthetic temporary
fixtures, so these tests exercise the guard's actual discovery/reporting
code path rather than reimplementing glob matching -- the exact defect
being fixed is a guard that reports success while silently omitting a
file, so a test with its own private glob logic could pass while the
real guard stayed blind.

Usage:
  python3 tools/test_haskell_module_budget.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from haskell_module_budget import check  # type: ignore

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


def _write(root: Path, rel: str, lines: int) -> None:
    path = root / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(f"-- line {i}" for i in range(lines)) + "\n")


def _run(root: Path, patterns: list[str], budget: int, label: str = "fixture split"):
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        failures = check(repo_root=root, budgets=[(label, patterns, budget)])
    return failures, buf.getvalue()


def test_direct_child_module_discovered():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "src/Foo.hs", 3)
        _write(root, "src/Foo/Bar.hs", 5)
        failures, output = _run(
            root, ["src/Foo.hs", "src/Foo/**/*.hs"], budget=500)
        expect(failures == [],
               f"a direct child module within budget must produce no "
               f"failures, got: {failures}")
        expect("Foo/Bar.hs" in output,
               "a direct child module (src/Foo/Bar.hs) must be discovered "
               "and reported by the recursive directory pattern")


def test_nested_descendant_module_discovered():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "src/Foo.hs", 3)
        _write(root, "src/Foo/Bar.hs", 5)
        _write(root, "src/Foo/Bar/Baz.hs", 7)
        failures, output = _run(
            root, ["src/Foo.hs", "src/Foo/**/*.hs"], budget=500)
        expect(failures == [],
               f"a nested descendant module within budget must produce no "
               f"failures, got: {failures}")
        expect("Foo/Bar/Baz.hs" in output,
               "a nested descendant module two levels deep "
               "(src/Foo/Bar/Baz.hs) must be discovered by the recursive "
               "directory pattern -- this is the exact shape of the real "
               "src/Engine/Input/Thread/Mouse/Activation.hs regression")


def test_over_budget_nested_module_fails_with_attribution():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "src/Foo.hs", 3)
        _write(root, "src/Foo/Bar/Baz.hs", 10)
        failures, _output = _run(
            root, ["src/Foo.hs", "src/Foo/**/*.hs"], budget=5,
            label="fixture split (#0)")
        expect(any("Foo/Bar/Baz.hs" in f for f in failures),
               f"an over-budget nested module must be reported by path in "
               f"the failures list, got: {failures}")
        expect(any("10" in f for f in failures),
               f"the failure must name the offending line count (10), not "
               f"just that some failure occurred, got: {failures}")
        expect(any("5" in f for f in failures),
               f"the failure must name the budget it exceeded (5), got: "
               f"{failures}")
        expect(not any("Foo.hs" in f and "Foo/Bar/Baz.hs" not in f
                        for f in failures),
               f"the in-budget facade (Foo.hs) must not be misattributed "
               f"as the failing file, got: {failures}")


def test_within_budget_nested_module_passes():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "src/Foo.hs", 3)
        _write(root, "src/Foo/Bar/Baz.hs", 4)
        failures, _output = _run(
            root, ["src/Foo.hs", "src/Foo/**/*.hs"], budget=5)
        expect(failures == [],
               f"a nested module at or under its budget must not be "
               f"reported as a failure, got: {failures}")


def test_missing_pattern_still_reported_as_failure():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        # No src/Foo.hs or src/Foo/ directory created at all.
        failures, _output = _run(
            root, ["src/Foo.hs", "src/Foo/**/*.hs"], budget=500,
            label="empty split")
        expect(any("empty split" in f and "no files matched" in f
                    for f in failures),
               f"a pattern set matching zero files must still fail with "
               f"the existing 'no files matched' message, got: {failures}")


def test_duplicate_match_across_patterns_deduplicated():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "src/Foo/Bar.hs", 3)
        # Two patterns that both resolve to the same file.
        failures, output = _run(
            root, ["src/Foo/**/*.hs", "src/Foo/Bar.hs"], budget=500)
        expect(failures == [], f"expected no failures, got: {failures}")
        expect(output.count("Foo/Bar.hs") == 1,
               f"a file matched by two overlapping patterns must be "
               f"reported exactly once (deduplicated via the matched "
               f"set), got {output.count('Foo/Bar.hs')} occurrences")


def test_isolated_from_tracked_source():
    # Every fixture above is written under tempfile.TemporaryDirectory()
    # and passed as an explicit repo_root -- none of them touch this
    # repository's tracked src/ tree. This case pins that isolation
    # doesn't accidentally leak: a check() call against a fresh empty
    # temp root with no matching budgets must not discover anything
    # from the real repository.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        failures, _output = _run(
            root, ["src/Engine/Input/Thread/**/*.hs"], budget=500,
            label="isolation check")
        expect(any("no files matched" in f for f in failures),
               "an empty temporary root must not see any real repository "
               "files, even for a pattern that matches plenty in the real "
               "tree")


TESTS = [
    test_direct_child_module_discovered,
    test_nested_descendant_module_discovered,
    test_over_budget_nested_module_fails_with_attribution,
    test_within_budget_nested_module_passes,
    test_missing_pattern_still_reported_as_failure,
    test_duplicate_match_across_patterns_deduplicated,
    test_isolated_from_tracked_source,
]


def main() -> int:
    selftest.parse_verbose()
    for test in TESTS:
        print(f"{test.__name__}:")
        test()
    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s)")
        return selftest.concluded(1)
    return selftest.concluded(0, f"\nAll {len(TESTS)} tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
