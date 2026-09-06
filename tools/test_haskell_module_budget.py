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

Since #1924 the guard is silent about in-budget modules, so discovery is
proven by driving each fixture module over its budget and reading the
returned failures: a file the glob never matched cannot appear there.
That is a stronger witness than the removed per-module narration, which
a guard could print while still mis-attributing the count.

Usage:
  python3 tools/test_haskell_module_budget.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from haskell_module_budget import check  # type: ignore

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402


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
        # The child is over budget and the facade is not, so the returned
        # failures witness discovery of exactly the child.
        failures, output = _run(
            root, ["src/Foo.hs", "src/Foo/**/*.hs"], budget=4)
        expect(any("Foo/Bar.hs" in f for f in failures),
               f"a direct child module (src/Foo/Bar.hs) must be discovered "
               f"by the recursive directory pattern -- driving it over "
               f"budget must surface it in the failures, got: {failures}")
        expect(len(failures) == 1,
               f"only the over-budget child must be reported; the "
               f"in-budget facade must stay silent, got: {failures}")
        expect(output == "",
               f"check() must print nothing per module (#1924), got: "
               f"{output!r}")


def test_nested_descendant_module_discovered():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "src/Foo.hs", 3)
        _write(root, "src/Foo/Bar.hs", 5)
        _write(root, "src/Foo/Bar/Baz.hs", 7)
        # Only the two-levels-deep descendant exceeds the budget.
        failures, output = _run(
            root, ["src/Foo.hs", "src/Foo/**/*.hs"], budget=6)
        expect(any("Foo/Bar/Baz.hs" in f for f in failures),
               f"a nested descendant module two levels deep "
               f"(src/Foo/Bar/Baz.hs) must be discovered by the recursive "
               f"directory pattern -- this is the exact shape of the real "
               f"src/Engine/Input/Thread/Mouse/Activation.hs regression, "
               f"got: {failures}")
        expect(len(failures) == 1,
               f"only the over-budget descendant must be reported, got: "
               f"{failures}")
        expect(output == "",
               f"check() must print nothing per module (#1924), got: "
               f"{output!r}")


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
        expect(any("fixture split (#0)" in f for f in failures),
               f"the failure must name the label of the family it belongs "
               f"to, got: {failures}")
        expect(sum(1 for f in failures if "Foo/Bar/Baz.hs" in f) == 1,
               f"an over-budget module must be reported exactly once "
               f"(#1924 removed the duplicate per-module FAIL line), got: "
               f"{failures}")
        expect(_output == "",
               f"check() must print nothing per module even on the failing "
               f"path (#1924), got: {_output!r}")


def test_within_budget_nested_module_passes():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "src/Foo.hs", 3)
        _write(root, "src/Foo/Bar/Baz.hs", 4)
        failures, output = _run(
            root, ["src/Foo.hs", "src/Foo/**/*.hs"], budget=5)
        expect(failures == [],
               f"a nested module at or under its budget must not be "
               f"reported as a failure, got: {failures}")
        expect(output == "",
               f"an in-budget check must emit no per-module output at all "
               f"(#1924), got: {output!r}")


def test_in_budget_check_emits_no_output():
    # The #1924 contract, pinned on its own rather than as a rider on a
    # discovery case: however many modules a family governs, a passing
    # check() writes nothing to stdout, so the guard's output does not
    # grow as families are added.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        for rel in ("src/Foo.hs", "src/Foo/A.hs", "src/Foo/B.hs",
                    "src/Foo/Deep/C.hs", "src/Foo/Deep/Deeper/D.hs"):
            _write(root, rel, 3)
        failures, output = _run(
            root, ["src/Foo.hs", "src/Foo/**/*.hs"], budget=500)
        expect(failures == [], f"expected no failures, got: {failures}")
        expect(output == "",
               f"five in-budget modules must still produce zero lines of "
               f"output, got: {output!r}")


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
        expect(all(pattern in " ".join(failures)
                   for pattern in ("src/Foo.hs", "src/Foo/**/*.hs")),
               f"the no-match failure must still name every pattern in the "
               f"entry, so a glob that silently stops matching is "
               f"identifiable, got: {failures}")
        expect(_output == "",
               f"the no-match path must print nothing itself (#1924), got: "
               f"{_output!r}")


def test_duplicate_match_across_patterns_deduplicated():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "src/Foo/Bar.hs", 3)
        # Two patterns that both resolve to the same file, driven over
        # budget so the deduplication is observable in the failures now
        # that in-budget modules are never narrated (#1924).
        failures, output = _run(
            root, ["src/Foo/**/*.hs", "src/Foo/Bar.hs"], budget=2)
        matches = sum(1 for f in failures if "Foo/Bar.hs" in f)
        expect(matches == 1,
               f"a file matched by two overlapping patterns must be "
               f"reported exactly once (deduplicated via the matched "
               f"set), got {matches} occurrences in {failures}")
        expect(output == "",
               f"check() must print nothing per module (#1924), got: "
               f"{output!r}")


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


def test_cli_success_prints_only_its_summary():
    # #1924's user-visible contract, read the way CI reads it: run the
    # real script as a subprocess so the __main__ entry point and the
    # summary formatting are both covered. A green run must be exactly
    # one physical line -- no per-module lines and no leading blank one.
    script = Path(__file__).resolve().parent / "haskell_module_budget.py"
    completed = subprocess.run(
        [sys.executable, str(script)],
        capture_output=True, text=True, check=False)
    lines = completed.stdout.splitlines()
    if completed.returncode == 0:
        expect(len(lines) == 1,
               f"a passing run must print exactly one physical line, got "
               f"{len(lines)}: {lines!r}")
        expect(lines[:1] == ["All budgeted Haskell modules are within "
                             "their line budget"],
               f"the one line must be the summary, got: {lines!r}")
    else:
        # A real over-budget module in the tracked tree: the guard is
        # correctly red, and this case still pins that it reports each
        # violation once with no per-module narration around it.
        expect(len(lines) >= 2,
               f"a failing run must print a header and name its "
               f"violations, got: {lines!r}")
        expect(bool(lines) and lines[0].endswith("budget violation(s):"),
               f"a failing run must open with the violation header and no "
               f"leading blank line, got: {lines!r}")
    expect(not any(line.lstrip().startswith(("OK:", "FAIL:"))
                   for line in lines),
           f"no run may print a per-module OK:/FAIL: line (#1924), got: "
           f"{lines!r}")


TESTS = [
    test_direct_child_module_discovered,
    test_nested_descendant_module_discovered,
    test_over_budget_nested_module_fails_with_attribution,
    test_within_budget_nested_module_passes,
    test_in_budget_check_emits_no_output,
    test_missing_pattern_still_reported_as_failure,
    test_duplicate_match_across_patterns_deduplicated,
    test_isolated_from_tracked_source,
    test_cli_success_prints_only_its_summary,
]


def main() -> int:
    selftestlib.parse_verbose()
    for test in TESTS:
        print(f"{test.__name__}:")
        test()
    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s)")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, f"\nAll {len(TESTS)} tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
