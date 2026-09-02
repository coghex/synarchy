#!/usr/bin/env python3
"""world_check.py's summary comparison and determinism status (#2070).

Pure-logic groups over `CheckResult`: strict match, regression,
improvement and drop-to-zero; bug and quality threshold precedence over a
match; unclassified-category and missing-threshold failures; the racy
seed that needs no match; and determinism regression, improvement,
single-run safety and one-run inactivity. Nothing here reads a baseline
file or generates a dump.

`test_check_determinism_inactive_at_one_run` belongs to this owner even
though the monolith defined it inside the content-hash section: it pins
`check_determinism_status`, not the hash gate.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from world_audit import (  # type: ignore  # noqa: E402
    classify_category, QUALITY_CATEGORIES, QUALITY_THRESHOLDS,
)
from world_check import (  # type: ignore  # noqa: E402
    CheckResult, check_issue_summary, check_determinism_status,
    PASS, FAIL, IMPROVED,
)
from test_audit_support import expect  # noqa: E402


def _result() -> CheckResult:
    return CheckResult(seed=0, world_size=32, region=(0, 0, 0, 0), status=PASS)


def test_check_summary_strict_match() -> None:
    """Deterministic seed whose summary equals the baseline passes."""
    print("test_check_summary_strict_match")
    r = _result()
    base = {"LAKE_HOLE": 4, "FLOATING_LAKE": 300}
    check_issue_summary([dict(base)], base, {}, strict=True, result=r)
    expect(r.status == PASS, f"exact match should PASS, got {r.status}: {r.failures}")
    expect(not r.failures, f"no failures expected, got {r.failures}")


def test_check_summary_strict_regression() -> None:
    """Deterministic count above baseline (under threshold) is a regression."""
    print("test_check_summary_strict_regression")
    r = _result()
    base = {"LAKE_HOLE": 4}
    check_issue_summary([{"LAKE_HOLE": 5}], base, {}, strict=True, result=r)
    expect(r.status == FAIL, f"regression should FAIL, got {r.status}")
    expect(any("regressed above baseline" in f for f in r.failures),
           f"expected regression message, got {r.failures}")


def test_check_summary_strict_improvement() -> None:
    """Deterministic count below baseline is an improvement, not a failure."""
    print("test_check_summary_strict_improvement")
    r = _result()
    base = {"LAKE_HOLE": 4}
    check_issue_summary([{"LAKE_HOLE": 2}], base, {}, strict=True, result=r)
    expect(r.status == IMPROVED, f"improvement should be IMPROVED, got {r.status}")
    expect(not r.failures, f"no failures expected, got {r.failures}")
    expect(any("below baseline" in i for i in r.improvements),
           f"expected improvement message, got {r.improvements}")


def test_check_summary_strict_drop_to_zero() -> None:
    """A baseline category absent from the current summary counts as 0 (improvement)."""
    print("test_check_summary_strict_drop_to_zero")
    r = _result()
    base = {"LAKE_HOLE": 4}
    check_issue_summary([{}], base, {}, strict=True, result=r)
    expect(r.status == IMPROVED, f"drop-to-zero should be IMPROVED, got {r.status}")
    expect(any("LAKE_HOLE" in i for i in r.improvements),
           f"expected LAKE_HOLE improvement, got {r.improvements}")


def test_check_summary_bug_overrides_match() -> None:
    """A BUG category fails even when the deterministic count matches baseline."""
    print("test_check_summary_bug_overrides_match")
    r = _result()
    base = {"TERRAIN_SPIKE": 2}
    check_issue_summary([{"TERRAIN_SPIKE": 2}], base, {}, strict=True, result=r)
    expect(r.status == FAIL, f"nonzero BUG should FAIL despite match, got {r.status}")
    expect(any("must be 0" in f for f in r.failures),
           f"expected must-be-0 message, got {r.failures}")


def test_check_summary_threshold_overrides() -> None:
    """Exceeding the QUALITY threshold fails regardless of strict/baseline."""
    print("test_check_summary_threshold_overrides")
    over = QUALITY_THRESHOLDS["LAKE_HOLE"] + 1
    r = _result()
    # Baseline "matches" the over-threshold value, but the cap still fails.
    check_issue_summary([{"LAKE_HOLE": over}], {"LAKE_HOLE": over}, {},
                        strict=True, result=r)
    expect(r.status == FAIL, f"over-threshold should FAIL, got {r.status}")
    expect(any("exceeds threshold" in f for f in r.failures),
           f"expected threshold message, got {r.failures}")


def test_check_summary_unclassified_category_fails() -> None:
    """A category in neither BUG_CATEGORIES nor QUALITY_CATEGORIES fails the
    seed by name, on both paths and at any count — never tolerated under an
    implicit threshold."""
    print("test_check_summary_unclassified_category_fails")
    for strict, baseline in ((False, {}), (True, {"NEW_CORRUPTION": 1})):
        r = _result()
        check_issue_summary([{"NEW_CORRUPTION": 1}], baseline, {},
                            strict=strict, result=r)
        expect(r.status == FAIL,
               f"unclassified category should FAIL (strict={strict}), "
               f"got {r.status}")
        expect(any("NEW_CORRUPTION" in f and "UNCLASSIFIED" in f
                   for f in r.failures),
               f"failure should name the category as unclassified "
               f"(strict={strict}), got {r.failures}")

    # The old implicit 1000 tolerated a brand-new corruption class up to
    # that count in both modes; a single occurrence must now fail.
    r = _result()
    check_issue_summary([{"NEW_CORRUPTION": 999}], {}, {},
                        strict=False, result=r)
    expect(r.status == FAIL,
           f"a sub-1000 unclassified count should FAIL, got {r.status}")
    expect(not any("threshold 1000" in f for f in r.failures),
           f"no implicit 1000 threshold should survive, got {r.failures}")


def test_check_summary_unclassified_from_baseline_or_envelope() -> None:
    """An unclassified category reaching the check only through the baseline
    or the audit envelope fails too, even at a current count of zero."""
    print("test_check_summary_unclassified_from_baseline_or_envelope")
    r = _result()
    check_issue_summary([{}], {"BASELINE_ONLY": 3}, {}, strict=True, result=r)
    expect(r.status == FAIL,
           f"baseline-only unclassified category should FAIL, got {r.status}")
    expect(any("BASELINE_ONLY" in f for f in r.failures),
           f"failure should name BASELINE_ONLY, got {r.failures}")

    r = _result()
    check_issue_summary([{}], {}, {"ENVELOPE_ONLY": {"max": 7}},
                        strict=False, result=r)
    expect(r.status == FAIL,
           f"envelope-only unclassified category should FAIL, got {r.status}")
    expect(any("ENVELOPE_ONLY" in f for f in r.failures),
           f"failure should name ENVELOPE_ONLY, got {r.failures}")


def test_check_summary_quality_without_threshold_fails() -> None:
    """A QUALITY category with no QUALITY_THRESHOLDS entry fails by name
    rather than silently receiving an implicit default."""
    print("test_check_summary_quality_without_threshold_fails")
    cat = "QUALITY_NO_THRESHOLD"
    QUALITY_CATEGORIES.add(cat)
    try:
        for strict, baseline in ((False, {}), (True, {cat: 1})):
            r = _result()
            check_issue_summary([{cat: 1}], baseline, {},
                                strict=strict, result=r)
            expect(r.status == FAIL,
                   f"threshold-less QUALITY category should FAIL "
                   f"(strict={strict}), got {r.status}")
            expect(any(cat in f and "no explicit threshold" in f
                       for f in r.failures),
                   f"failure should name the category and the missing "
                   f"threshold (strict={strict}), got {r.failures}")

        # Baseline-only, current count zero: still a failure.
        r = _result()
        check_issue_summary([{}], {cat: 0}, {}, strict=True, result=r)
        expect(r.status == FAIL,
               f"threshold-less QUALITY category should FAIL at count 0, "
               f"got {r.status}")
    finally:
        QUALITY_CATEGORIES.discard(cat)

    # The mutation is undone, so the category is unclassified again.
    expect(classify_category(cat) is None,
           f"{cat} should be unclassified after the test restores the set")


def test_check_summary_racy_no_match_required() -> None:
    """Racy seeds don't require an exact match; under-threshold drift is a note."""
    print("test_check_summary_racy_no_match_required")
    r = _result()
    base = {"LAKE_HOLE": 2}
    env = {"LAKE_HOLE": {"min": 2, "max": 2}}
    # 5 != baseline 2, but it's under the threshold (25); racy mode must
    # not fail on the mismatch (the strict match rule does not apply).
    check_issue_summary([{"LAKE_HOLE": 5}], base, env, strict=False, result=r)
    expect(r.status == PASS, f"racy under-threshold mismatch should PASS, got {r.status}")
    expect(not r.failures, f"racy mode should not fail on mismatch, got {r.failures}")


def test_check_determinism_regression() -> None:
    """A seed that was deterministic and is now racy fails."""
    print("test_check_determinism_regression")
    r = _result()
    check_determinism_status(deterministic_baseline=True, deterministic_now=False,
                             n_distinct=3, runs=3, result=r)
    expect(r.status == FAIL, f"determinism regression should FAIL, got {r.status}")
    expect(any("determinism regression" in f for f in r.failures),
           f"expected determinism-regression message, got {r.failures}")


def test_check_determinism_improvement() -> None:
    """A seed that was racy and is now deterministic across runs>1 improves."""
    print("test_check_determinism_improvement")
    r = _result()
    check_determinism_status(deterministic_baseline=False, deterministic_now=True,
                             n_distinct=1, runs=3, result=r)
    expect(r.status == IMPROVED, f"racy->det should be IMPROVED, got {r.status}")


def test_check_determinism_single_run_safe() -> None:
    """With runs==1 a deterministic baseline can't trip a false regression."""
    print("test_check_determinism_single_run_safe")
    r = _result()
    check_determinism_status(deterministic_baseline=True, deterministic_now=True,
                             n_distinct=1, runs=1, result=r)
    expect(r.status == PASS, f"single-run det should stay PASS, got {r.status}")
    expect(not r.failures, f"no failures expected, got {r.failures}")


def test_check_determinism_inactive_at_one_run() -> None:
    """At runs==1 the determinism rule records nothing, in either direction.

    The guard is explicit rather than incidental: one dump cannot be
    compared with itself, so this branch must not read as though it gates
    something. Content coverage at this setting comes from the baseline
    hash instead.
    """
    print("test_check_determinism_inactive_at_one_run")
    r = _result()
    check_determinism_status(deterministic_baseline=True, deterministic_now=False,
                             n_distinct=2, runs=1, result=r)
    expect(r.status == PASS,
           f"determinism status must be inactive at runs==1, got {r.status}")
    expect(not r.failures and not r.improvements,
           f"nothing should be recorded at runs==1, got "
           f"{r.failures} / {r.improvements}")


#: This owner's inventory, in the relative order these groups hold
#: within the aggregate's run sequence. `tools/test_audit.py` composes
#: that sequence from every owner's inventory; nothing here decides
#: when, or whether, it runs.
TESTS = (
    test_check_summary_strict_match,
    test_check_summary_strict_regression,
    test_check_summary_strict_improvement,
    test_check_summary_strict_drop_to_zero,
    test_check_summary_bug_overrides_match,
    test_check_summary_threshold_overrides,
    test_check_summary_racy_no_match_required,
    test_check_summary_unclassified_category_fails,
    test_check_summary_unclassified_from_baseline_or_envelope,
    test_check_summary_quality_without_threshold_fails,
    test_check_determinism_regression,
    test_check_determinism_improvement,
    test_check_determinism_single_run_safe,
    test_check_determinism_inactive_at_one_run,
)
