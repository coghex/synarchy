#!/usr/bin/env python3
"""Focused self-test for the atomic per-probe claim (#1434).

Deterministic, engine-free, GPU-free and offline. Nothing here boots an
engine, runs a registered probe, touches the developer's real `docs-wip`
worktree or the repository's real claim namespace: every case runs in a
throwaway temporary tree, and the only subprocesses are `git` (to build
scratch repositories with real linked worktrees) and this same
interpreter (for the cases that need genuinely separate processes).

The real `tools/probe_claim.py` and `tools/probe_census.py` are imported
and driven, with `probe_runner_registry.PROBES` and the other live registries
pointed at a synthetic set, so this exercises the shipped code paths
rather than a copy.

What the concurrency cases actually prove, and why they are subprocesses
rather than threads: the claim has to hold between OS processes, and an
in-process test could pass on nothing but the GIL. Each of them starts N
real interpreters that block on a shared barrier file before racing, so
the contention is real and the count of winners is the assertion.

Composition (#2100)
-------------------
This module is composition and selection only -- it holds no case body.
The 29 cases live with three independently changing contract owners,
each of which keeps its own inventory in `CASES`:

  `probe_claim_selftest_claim`          the atomic claim and lease
                                        lifecycle (12 cases);
  `probe_claim_selftest_census`         the census collection, its
                                        schema, and the collaborator
                                        boundaries (4 cases);
  `probe_claim_selftest_orchestration`  the claimed measurement, its
                                        retained result and the CLI
                                        (13 cases).

`probe_claim_selftest_support` is the single source of everything they
share: the assertion helpers and the ONE failure accumulator behind
them, the synthetic registries, the scratch trees and scratch
repository, the real `probe_flake.Measurement` builder, and the
subprocess programs the concurrency cases race.

`AGGREGATE_ORDER` below is the run sequence this gate has always used.
It is deliberately spelled out rather than derived by concatenating the
three inventories, because it interleaves them: three census cases run
early and the fourth runs second-to-last, and two orchestration cases
run out of their own source order.

Two checks run before any case does, on every invocation including a
focused one, so no arrangement of owners and sequence can report a pass
having run less than it claims. `inventories()` refuses an owner that
has stopped declaring `CASES` and one that declares an empty inventory;
`_resolve` then binds every entry to its owner's case object and
refuses drift in either direction -- a sequence entry its owner does
not declare, a declared case the sequence never runs, an entry the
sequence would run twice, and a case two owners both claim.

Usage:
  python3 tools/test_probe_claim.py                     every case, in order
  python3 tools/test_probe_claim.py --only claim        one owner's cases
  python3 tools/test_probe_claim.py --only census
  python3 tools/test_probe_claim.py --only orchestration
The bare form is the gate: CI and `tools/ci-local.sh` invoke that and
nothing else. The focused forms are for iteration and each runs its own
owner's cases only, in the relative order they hold in the full run.
Exit codes: 0 = all selected tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import probe_claim_selftest_census as census_owner  # type: ignore  # noqa: E402
import probe_claim_selftest_claim as claim_owner  # type: ignore  # noqa: E402
import probe_claim_selftest_orchestration as orchestration_owner  # type: ignore  # noqa: E402
import probe_claim_selftest_support as support  # type: ignore  # noqa: E402
import selftestlib  # noqa: E402

#: The three contract owners, by selector name.
OWNERS = {
    "claim": claim_owner,
    "census": census_owner,
    "orchestration": orchestration_owner,
}

#: The full run sequence, owner-tagged. This is the order the gate has
#: always run in -- neither source order nor a concatenation of the
#: three inventories.
AGGREGATE_ORDER = (
    ("claim", "test_namespace"),
    ("claim", "test_exclusive_acquisition"),
    ("claim", "test_independent_process_contention"),
    ("claim", "test_expiry_and_reclaim"),
    ("claim", "test_concurrent_stale_reclaimers"),
    ("claim", "test_owner_safe_late_release"),
    ("claim", "test_a_contended_acquisition_gets_a_live_lease"),
    ("claim", "test_expiry_is_one_way"),
    ("claim", "test_malformed_claim"),
    ("claim", "test_release_on_every_managed_exit"),
    ("claim", "test_crash_recovery_through_ttl"),
    ("claim", "test_renewer_keeps_a_long_measurement_alive"),
    ("census", "test_census_claim_collection"),
    ("census", "test_claims_are_not_measurements"),
    ("census", "test_schema_migration_is_lossless"),
    ("orchestration", "test_orchestration_happy_path"),
    ("orchestration", "test_orchestration_denied_creates_nothing"),
    ("orchestration", "test_orchestration_claim_audit_failure"),
    ("orchestration", "test_orchestration_harness_error_is_still_ingested"),
    ("orchestration", "test_orchestration_rejects_before_claiming"),
    ("orchestration",
     "test_orchestration_refuses_a_lease_that_cannot_survive_a_run"),
    ("orchestration", "test_a_short_lease_means_what_it_says"),
    ("orchestration", "test_orchestration_aborts_when_the_claim_is_lost"),
    ("orchestration", "test_ingestion_cannot_be_overtaken"),
    ("orchestration", "test_a_delayed_audit_cannot_be_overtaken"),
    ("orchestration", "test_commit_while_held_renews_and_refuses"),
    ("orchestration", "test_a_completed_measurement_is_never_lost"),
    ("census", "test_probe_flake_needs_no_docs_worktree"),
    ("orchestration", "test_cli"),
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
    here too -- every case belongs to exactly one owner.

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


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Self-test for tools/probe_claim.py (#1434).",
        epilog="With no selector, every case runs in the full sequence.")
    parser.add_argument(
        "--only", choices=sorted(OWNERS),
        help="run one contract owner's cases only.")
    # This module owns its own command line, so the shared verbosity
    # flag joins that parser rather than being consumed behind its
    # back; `begin` then starts this invocation's own count (#1922).
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(argv)
    selftestlib.begin(args.verbose)

    try:
        cases = selected_cases(args.only)
    except CompositionError as error:
        print(f"probe_claim self-test composition error: {error}")
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

    for case in cases:
        case()
    print()
    if support.FAILURES:
        print(f"{len(support.FAILURES)} FAILED:")
        for message in support.FAILURES:
            print(f"  - {message}")
        return selftestlib.concluded(1)
    if args.only is None:
        return selftestlib.concluded(0, "probe_claim self-test: all cases pass")
    return selftestlib.concluded(
        0, f"probe_claim self-test [{args.only}]: all {len(cases)} cases pass")


if __name__ == "__main__":
    sys.exit(main())
