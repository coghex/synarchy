#!/usr/bin/env python3
"""Focused self-test for the read-only Codex `$test` evidence reader (#1432).

Deterministic, engine-free, GPU-free and offline. Every case runs
against a synthetic `codex-test` tree in a throwaway temporary
directory: a synthetic `registry.json` shaped like the real
`codex-test-coordinator/v1` document plus synthetic
`*.test-result.md` reports. Nothing here boots an engine, runs a
registered probe, or touches the developer's real machine-local `$test`
state. The real `tools/probe_external_evidence.py` is imported and
driven, so this exercises the shipped code paths rather than a copy.

The central contract under test is NON-INTERACTION, and it is proved
mechanically rather than inferred from the reader's output: every file
under the synthetic tree is digested before and after each read, the
confinement cases record every file the reader actually opens, and
`subprocess.*` and `fcntl.flock` / `lockf` are replaced with tripwires
so a coordinator invocation or a lock fails the test rather than
passing quietly. The instrumentation lives in
`probe_external_evidence_test_support`, the single owner of the
assertion state and the synthetic state tree.

The one case that legitimately shells out (`git rev-parse
--git-common-dir` for state resolution) builds its own scratch
repository with a real linked worktree and runs outside the tripwires.

Composition (#2187)
-------------------
This module is composition and selection only -- it holds no case body.
The 27 cases live with four independently changing owners, each of
which keeps its own ordered inventory in `CASES`:

  `probe_external_evidence_test_identity`     probe/test identity and
                                              coordinator-state
                                              interpretation (10 cases);
  `probe_external_evidence_test_reports`      report parsing and the
                                              mechanical-versus-
                                              interpretive split
                                              (5 cases);
  `probe_external_evidence_test_confinement`  filesystem confinement
                                              (5 cases);
  `probe_external_evidence_test_resilience`   damaged-input resilience,
                                              state-root resolution and
                                              presentation (7 cases).

`AGGREGATE_ORDER` below is the run sequence this gate has always used:
the four owners as contiguous blocks, in that order. It is spelled out
rather than derived by concatenating the inventories so that the
composition checks have something independent to check against, and
because the identity block's run order is not its source order.

Two checks run before any case does, on every invocation including a
focused one, so no arrangement of owners and sequence can report a pass
having run less than it claims. `inventories()` refuses an owner that
has stopped declaring `CASES` and one that declares an empty inventory;
`_resolve` then binds every entry to its owner's case object and
refuses drift in either direction -- a sequence entry its owner does
not declare, a declared case the sequence never runs, an entry the
sequence would run twice, a case two owners both claim, and a resolved
sequence whose per-owner allocation is not `EXPECTED_CASES` or whose
total is not `EXPECTED_TOTAL`.
After the selected cases run, an invocation that executed zero
assertions exits nonzero rather than reporting a vacuous pass, and one
that left any tripwired seam patched does the same.

Usage:
  python3 tools/test_probe_external_evidence.py                     every case, in order
  python3 tools/test_probe_external_evidence.py --only identity     one owner's cases
  python3 tools/test_probe_external_evidence.py --only reports
  python3 tools/test_probe_external_evidence.py --only confinement
  python3 tools/test_probe_external_evidence.py --only resilience
The bare form is the gate. The focused forms are for iteration and each
runs its own owner's cases only, in the relative order they hold in the
full run. An unknown selector exits 2 with argparse's usage.
Exit codes: 0 = all selected tests passed, 1 = one or more failed, or a
composition / vacuity / seam-restoration refusal.
"""
from __future__ import annotations

import argparse
import fcntl
import os
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_external_evidence_test_confinement as confinement_owner  # noqa: E402
import probe_external_evidence_test_identity as identity_owner  # noqa: E402
import probe_external_evidence_test_reports as reports_owner  # noqa: E402
import probe_external_evidence_test_resilience as resilience_owner  # noqa: E402
import probe_external_evidence_test_support as support  # noqa: E402

#: The four owners, by selector name.
OWNERS = {
    "identity": identity_owner,
    "reports": reports_owner,
    "confinement": confinement_owner,
    "resilience": resilience_owner,
}

#: The starting allocation (#2187): how many cases each owner holds,
#: and the total the aggregate must run exactly once. The total is its
#: own literal rather than the sum, so editing one owner's share alone
#: is caught rather than silently re-balanced.
EXPECTED_CASES = {
    "identity": 10,
    "reports": 5,
    "confinement": 5,
    "resilience": 7,
}
EXPECTED_TOTAL = 27

#: The full run sequence, owner-tagged. This is the order the gate has
#: always run in.
AGGREGATE_ORDER = (
    ("identity", "test_identity_mapping"),
    ("identity", "test_both_identities_map_to_one_probe"),
    ("identity", "test_a_measurement_run_is_the_same_probes_work"),
    ("identity", "test_the_heartbeat_is_reported_raw"),
    ("identity", "test_diagnostics_carry_the_state_they_concern"),
    ("identity", "test_a_damaged_state_root_is_not_an_absent_one"),
    ("identity", "test_an_unreadable_run_identity_is_record_damage"),
    ("identity", "test_entry_state_separates_absent_from_unexaminable"),
    ("identity", "test_unknown_key_is_rejected"),
    ("identity", "test_exact_matching"),
    ("reports", "test_clean_and_observed_reports"),
    ("reports", "test_incomplete_run_reports_unavailable_not_false"),
    ("reports", "test_mechanical_outcome_is_not_inferred_from_interpretation"),
    ("reports", "test_missing_and_malformed_reports_are_non_fatal"),
    ("reports", "test_an_existing_non_regular_report_is_damage_not_absence"),
    ("confinement", "test_report_reads_are_confined_to_the_reports_directory"),
    ("confinement", "test_a_symlinked_reports_directory_refuses_every_read"),
    ("confinement", "test_a_misplaced_reports_directory_refuses_every_read"),
    ("confinement", "test_the_registry_is_confined_to_the_state_root"),
    ("confinement", "test_a_malformed_report_path_never_aborts_the_read"),
    ("resilience", "test_absent_state_is_success_not_error"),
    ("resilience", "test_damaged_registry_is_non_fatal"),
    ("resilience", "test_non_finite_numbers_never_reach_the_output"),
    ("resilience", "test_full_history_is_never_truncated"),
    ("resilience", "test_presentation_only"),
    ("resilience", "test_state_root_resolves_through_the_common_git_dir"),
    ("resilience", "test_render_is_total"),
)

#: Every function the support module's guards patch and must restore.
#: Snapshotted before the first case and compared after the last, so a
#: guard that stopped restoring fails every entry point.
SEAMS = (
    (subprocess, "run"), (subprocess, "Popen"), (subprocess, "call"),
    (subprocess, "check_output"), (fcntl, "flock"), (fcntl, "lockf"),
    (Path, "read_text"), (Path, "read_bytes"), (Path, "open"),
)


class CompositionError(Exception):
    """The owners and the run sequence no longer agree."""


def inventories() -> dict:
    """Every owner's declared cases.

    Both degenerate shapes are composition failures rather than owners
    with nothing to run, and they are checked here -- before the run
    sequence is consulted -- so that an owner losing its inventory is
    reported as itself rather than as whatever the sequence notices
    second. An ABSENT owner has stopped declaring `CASES` at all; an
    EMPTY one still declares it, which is the shape that would
    otherwise let a focused command collect nothing and exit 0.
    """
    found = {}
    for owner_key in sorted(OWNERS):
        module = OWNERS[owner_key]
        cases = getattr(module, "CASES", None)
        if cases is None:
            raise CompositionError(
                f"owner {owner_key!r} ({module.__name__}) declares no CASES "
                f"inventory")
        if not cases:
            raise CompositionError(
                f"owner {owner_key!r} ({module.__name__}) declares an EMPTY "
                f"inventory -- refusing to report a vacuous pass")
        found[owner_key] = list(cases)
    return found


def _resolve(order=None, by_owner=None) -> list:
    """Bind every entry in `order` to its owner's own case object.

    Checks both directions, because either drift is a silent loss of
    coverage: an entry naming a case its owner does not declare, and a
    declared case no entry runs. A case declared by two owners fails
    here too -- every case belongs to exactly one owner -- and so does
    a resolved sequence whose per-owner allocation differs from
    `EXPECTED_CASES` or whose total differs from `EXPECTED_TOTAL`.

    Both parameters default at CALL time, not at import: a test that
    substitutes `AGGREGATE_ORDER` to prove one of these checks bites
    must actually reach the substituted value.
    """
    order = AGGREGATE_ORDER if order is None else order
    by_owner = inventories() if by_owner is None else by_owner

    seen: dict[str, str] = {}
    for owner_key, cases in by_owner.items():
        for case in cases:
            if case.__name__ in seen:
                raise CompositionError(
                    f"case {case.__name__!r} is declared by both "
                    f"{seen[case.__name__]!r} and {owner_key!r}")
            seen[case.__name__] = owner_key

    resolved, ran = [], set()
    for owner_key, case_name in order:
        matches = [case for case in by_owner[owner_key]
                   if case.__name__ == case_name]
        if not matches:
            raise CompositionError(
                f"the run sequence names {case_name!r} as {owner_key!r}'s, "
                f"but that owner does not declare it")
        if case_name in ran:
            raise CompositionError(
                f"the run sequence runs {case_name!r} more than once")
        ran.add(case_name)
        resolved.append((owner_key, matches[0]))

    missing = [f"{owner_key}:{case.__name__}"
               for owner_key, cases in by_owner.items()
               for case in cases if case.__name__ not in ran]
    if missing:
        raise CompositionError(
            f"declared cases the run sequence never runs: {sorted(missing)}")

    counted = {owner_key: sum(1 for key, _case in resolved if key == owner_key)
               for owner_key in by_owner}
    if counted != EXPECTED_CASES:
        raise CompositionError(
            f"the resolved sequence allocates {counted}, not the expected "
            f"{EXPECTED_CASES}")
    if sum(EXPECTED_CASES.values()) != EXPECTED_TOTAL:
        raise CompositionError(
            f"the per-owner allocation {EXPECTED_CASES} sums to "
            f"{sum(EXPECTED_CASES.values())}, not {EXPECTED_TOTAL}")
    if len(resolved) != EXPECTED_TOTAL:
        raise CompositionError(
            f"the resolved sequence holds {len(resolved)} cases, not "
            f"{EXPECTED_TOTAL}")
    return resolved


def selected_cases(only: str | None) -> list:
    """The cases one invocation runs.

    With no selector that is the whole sequence; with one it is that
    owner's cases alone, keeping the relative order they hold in the
    full run so a focused run reproduces the sequence it is a slice of.
    Either way the composition check above has already run, so a
    focused command cannot pass while a sibling owner has gone missing.
    """
    resolved = _resolve()
    if only is None:
        return [case for _owner, case in resolved]
    return [case for owner_key, case in resolved if owner_key == only]


def _seams() -> dict:
    return {f"{owner.__name__}.{name}": getattr(owner, name)
            for owner, name in SEAMS}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Self-test for tools/probe_external_evidence.py (#1432).",
        epilog="With no selector, every case runs in the full sequence.")
    parser.add_argument(
        "--only", choices=sorted(OWNERS),
        help="run one owner's cases only.")
    args = parser.parse_args(argv)

    try:
        cases = selected_cases(args.only)
    except CompositionError as error:
        print(f"probe_external_evidence self-test composition error: {error}")
        return 1

    if not cases:
        # Unreachable through the checks above -- `inventories()` has
        # already refused an absent or empty owner and `_resolve` has
        # already refused a sequence that skips one. Kept as the last
        # word on the invariant the whole selection exists to hold:
        # this gate never reports a pass having run nothing.
        target = "the aggregate" if args.only is None else f"--only {args.only}"
        print(f"no cases collected for {target} -- refusing to report a "
              f"vacuous pass")
        return 1

    if args.only is not None:
        print(f"probe_external_evidence self-test --only {args.only}: "
              f"{len(cases)} cases")

    pristine = _seams()
    for case in cases:
        try:
            case()
        except Exception as exc:                          # noqa: BLE001
            support.FAILURES.append(
                f"{case.__name__} raised {type(exc).__name__}: {exc}")
    print(f"probe_external_evidence self-test: {support.PASSED} checks passed, "
          f"{len(support.FAILURES)} failed")
    for failure in support.FAILURES:
        print(f"  FAIL {failure}")
    if support.FAILURES:
        return 1

    if support.PASSED == 0:
        print("no assertions executed -- refusing to report a vacuous pass")
        return 1
    leaked = sorted(name for name, original in pristine.items()
                    if _seams()[name] is not original)
    if leaked:
        print(f"patched functions left unrestored after the run: {leaked}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
