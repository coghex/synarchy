#!/usr/bin/env python3
"""Unit tests for the `/deflake` diagnosis mechanics (#1437, #1439, #1438).

Deterministic, engine-free, GPU-free and network-free. No probe is run,
no port opened, no worktree created, no census touched: every fixture is
a document in memory or a file in a temporary directory. #1438's section
is the one exception to "no file outside a temporary directory": it
stages the retained artifact tree a failing batch would have left, under
a fixture-owned path in `/tmp`, because that workflow READS those
artifacts and a filed issue whose only evidence is a pathname is the
thing it exists to prevent. The tracker itself stays a fake at the
publication boundary — no `gh`, no network, no issue.

Two things are deliberately REAL rather than faked, for the same reason
`tools/test_deflake.py` keeps them real — faking either would move the
assertion off the thing under test:

* `probe_flake.Measurement`, so every result document a case feeds the
  evaluator is one the harness would actually have written, built out of
  real `RunRecord`s against a real `probe_protocol` descriptor; and
* `probe_census`'s X arithmetic, so "at or below X out of ten" is the
  shipped policy's answer rather than a second implementation of it that
  could drift from the census the same numbers are recorded in.

What is NOT covered here, deliberately: whether a diagnosis is
convincing, whether a repair is minimal, and whether a surviving
assertion was quietly broadened. Those are reviewer judgements. The
module refuses a route whose machine-checkable evidence is missing, and
these tests hold it to exactly that.

Composition (#2031)
-------------------
This module is composition and selection only — it holds no test body.
The cases live with the three independently changing workflow owners
they belong to, each of which derives its own registry into `TESTS`:

  `deflake_diagnosis_selftest_diagnosis`
      the entry gate, the routes and the diagnosis CLI, for
      `tools/deflake_diagnosis.py` — and #2041's contract-vs-facade
      cases for `tools/deflake_contract.py`, which the pre-split file
      appended below #1438's section but which are this evaluator's;
  `deflake_diagnosis_selftest_outcome`
      the stable non-success outcome record, for
      `tools/deflake_outcome.py`;
  `deflake_diagnosis_selftest_issue`
      production-defect issue publication, for `tools/deflake_issue.py`
      and its owners.

The prefix is the FACADE's name, not `deflake_selftest_`: that one is
taken by `tools/test_deflake.py`'s own three owners (#2093), and two
unrelated gates sharing a module prefix is how a sibling's support
module gets imported by mistake.

`deflake_diagnosis_selftest_support` is the single source of what they
share: the probe and worktree constants, the real `probe_flake`-backed
result, handoff and diagnosis documents, and #1922's shared assertion
helper with the ONE `FAILURES` ledger behind it, re-exported from there
so the owners import everything shared from one place.

The aggregate run is DERIVED from those same three registries rather
than restated as a fourth list, so a case an owner declares cannot go
missing from the full run while it still exits zero. Cases are keyed on
the (owner, name) pair, never on the bare name: three namespaces may
now each define a `test_the_route_is_refused`, and a merge keyed on the
name alone would discard one of them and still report a plausible
total. The run order is the alphabetical-by-name order this gate has
always used, which is why the aggregate interleaves the three owners.

Two guards run before any case does, on every invocation including a
focused one, so no arrangement of owners can report a pass having run
less than it claims. `inventories()` refuses an owner that has stopped
declaring `TESTS` and one that declares an EMPTY registry — in the
aggregate as well as under `--only`, since a wiped registry would
otherwise leave the compatibility command printing `ok - N` for an N
below its real coverage. `collected()` then holds every owner to the
count it is expected to carry, in both directions.

Usage:
  python3 tools/test_deflake_diagnosis.py                  every case
  python3 tools/test_deflake_diagnosis.py --only diagnosis one owner's
  python3 tools/test_deflake_diagnosis.py --only outcome
  python3 tools/test_deflake_diagnosis.py --only issue
The bare form is the gate the production modules and `tools/README.md`
name. The focused forms are for iteration; each runs its own owner's
cases only, in the relative order they hold in the full run, and each
is self-contained in a fresh process.
Exit codes: 0 = all selected tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake_diagnosis_selftest_diagnosis as diagnosis_owner  # noqa: E402
import deflake_diagnosis_selftest_issue as issue_owner  # noqa: E402
import deflake_diagnosis_selftest_outcome as outcome_owner  # noqa: E402
import deflake_diagnosis_selftest_support as support  # noqa: E402
import selftestlib  # noqa: E402

#: The three workflow owners, by selector name.
OWNERS = {
    "diagnosis": diagnosis_owner,
    "outcome": outcome_owner,
    "issue": issue_owner,
}

#: What each owner is expected to carry. This is coverage, not a
#: roster: the run set is still derived from the owners themselves, and
#: these numbers only refuse a run whose collection has drifted from
#: what the gate is known to hold. Adding or removing a case is a
#: deliberate edit here.
EXPECTED_TESTS = {"diagnosis": 195, "outcome": 36, "issue": 34}

#: Spelled independently of the allocation above rather than derived
#: from it, so an edit that MOVES a case between owners — or that
#: adjusts one owner's number to absorb a case another owner lost —
#: still has to state the new total out loud.
EXPECTED_TOTAL = 265


class CompositionError(Exception):
    """The owners no longer declare the coverage this gate composes."""


def inventories(owners=None) -> dict:
    """Every owner's declared registry.

    Both degenerate shapes are composition failures rather than owners
    with nothing to run, and they are checked here — before any case
    executes and regardless of which invocation is running — so that an
    owner losing its registry is reported as itself. An ABSENT owner has
    stopped declaring `TESTS` at all; an EMPTY one still declares it,
    which is the shape that would otherwise let a command collect
    nothing and exit 0.

    `owners` defaults at CALL time, not at import, so a check that
    substitutes `OWNERS` reaches the substituted value.
    """
    owners = OWNERS if owners is None else owners
    found = {}
    for owner_key in owners:
        module = owners[owner_key]
        tests = getattr(module, "TESTS", None)
        if tests is None:
            raise CompositionError(
                f"owner {owner_key!r} ({module.__name__}) declares no TESTS "
                f"registry")
        if not tests:
            raise CompositionError(
                f"owner {owner_key!r} ({module.__name__}) declares an EMPTY "
                f"registry — refusing to report a vacuous pass")
        found[owner_key] = list(tests)
    return found


def collected(only=None, by_owner=None) -> list:
    """The `(owner, test)` pairs one invocation runs.

    Keyed on the pair throughout. Two owners may define the same test
    name — they are separate namespaces now — and both must run, so
    nothing here collapses the collection by name.

    The expected coverage is checked in both directions before any case
    runs: an owner short of its number has lost cases, and one above it
    has gained cases nobody recorded. The total is checked against its
    own literal so that moving a case between owners cannot net out.
    """
    by_owner = inventories() if by_owner is None else by_owner

    for owner_key in sorted(by_owner):
        expected = EXPECTED_TESTS.get(owner_key)
        if expected is None:
            raise CompositionError(
                f"owner {owner_key!r} declares tests but no expected count")
        actual = len(by_owner[owner_key])
        if actual != expected:
            raise CompositionError(
                f"owner {owner_key!r} collects {actual} tests, expected "
                f"{expected} — update EXPECTED_TESTS in the same change "
                f"that adds or removes one")
    total = sum(len(cases) for cases in by_owner.values())
    if total != EXPECTED_TOTAL:
        raise CompositionError(
            f"the three owners collect {total} tests in total, expected "
            f"{EXPECTED_TOTAL}")

    pairs = [(owner_key, test)
             for owner_key, cases in by_owner.items() for test in cases]
    # The order this gate has always run in: alphabetical by test name
    # across the whole collection, which is what the single-namespace
    # `sorted(globals().items())` runner produced. The owner is the tie
    # break, so a name two owners share has a stable order rather than
    # depending on which module imported first.
    pairs.sort(key=lambda pair: (pair[1].__name__, pair[0]))
    if only is None:
        return pairs
    return [pair for pair in pairs if pair[0] == only]


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(
        description="Deterministic gate for the /deflake workflows "
                    "(#1437, #1439, #1438).",
        epilog="With no selector, every owner's tests run exactly once.")
    parser.add_argument(
        "--only", choices=sorted(OWNERS), metavar="OWNER",
        help="run one workflow owner's tests only: "
             + ", ".join(sorted(OWNERS)) + ".")
    # This module owns its own command line, so the shared verbosity
    # flag joins that parser rather than being consumed behind its
    # back; `begin` then starts this invocation's own count (#1922).
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(argv)
    selftestlib.begin(args.verbose)

    try:
        selected = collected(args.only)
    except CompositionError as error:
        print(f"deflake-diagnosis gate composition error: {error}",
              file=sys.stderr)
        return 1

    if not selected:
        # Unreachable through the guards above — `inventories()` has
        # already refused an absent or empty owner and `collected()`
        # has already held each one to its count. Kept as the last word
        # on the invariant the whole selection exists to hold: this
        # gate never reports a pass having run nothing.
        target = ("the aggregate run" if args.only is None
                  else f"--only {args.only}")
        print(f"no tests collected for {target} — refusing to report a "
              f"vacuous pass", file=sys.stderr)
        return 1

    for _owner_key, test in selected:
        try:
            test()
        except Exception as error:  # noqa: BLE001 - a crash is a failure
            selftestlib.record_fail(f"{test.__name__} raised "
                                    f"{type(error).__name__}: {error}")
    if support.FAILURES:
        print(f"FAILED ({len(support.FAILURES)}):")
        for failure in support.FAILURES:
            print(f"  - {failure}")
        return selftestlib.concluded(1)
    if args.only is None:
        return selftestlib.concluded(
            0, f"ok - {len(selected)} deflake-diagnosis tests passed")
    return selftestlib.concluded(
        0, f"ok - {len(selected)} deflake-diagnosis [{args.only}] tests "
           f"passed")


if __name__ == "__main__":
    sys.exit(main())
