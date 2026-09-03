#!/usr/bin/env python3
"""The probe-runner process gate: selection, ordering and reporting (#2130).

`python3 tools/test_run_probes.py` is the unconditional gate CI and
`tools/ci-local.sh` invoke, and its no-argument behaviour is unchanged:
every group, in the order this suite has always run them, the same
assertions, the same failure accounting and the same final summary.

What this module holds is only that -- selection, ordered aggregation and
reporting. It carries no test body and names no test group. The 74 groups
live with six owners, five of them mirroring the production owners #2074
split `run_probes.py` into, so the suite divides exactly where the code
under test does:

  `readme`        2  #2035's `tools/README.md` registry-count audit, and
                     the proof that a failing audit fails this gate;
  `registry`     13  timeout-override and exclusive-resource
                     declarations, exact and substring selection, and
                     reserved port spans;
  `resources`    20  the one-time engine preflight, resolved-executable
                     propagation, inherited and foreign holds, and the
                     reader/writer resource ledger;
  `lifecycle`    14  one probe's launch, teardown, liveness and reap,
                     including the shutdown launch window;
  `scheduler`    14  aggregate exits, conflict scheduling, retries,
                     Ctrl-C, and the synthetic fixtures' own validation;
  `diagnostics`  11  the durable progress and failure record protocols.

Each owner declares its own ordered `TESTS` under
`tools/probe_runner_tests/`, and `support` there holds what two or more of
them share. Dependencies run one way: support imports no case owner, case
owners import support and the production module they exercise, and only
this facade imports the case owners. Importing any of them runs no test.

The aggregate's order interleaves the six families rather than finishing
one before starting the next -- it opens with the scheduler's fixture
validation, then nine lifecycle groups, then the README pair -- so each
family declares its blocks as FRAGMENTS and `SEQUENCE_FRAGMENTS` below is
that order written as which fragment runs when. Naming a fragment is not
naming a test: every group name still lives with exactly one owner. That
sequence was written by diffing this gate's stdout against the pre-split
run until the two were identical.

Before any group runs, `compose()` checks that arrangement, because
`selftestlib.concluded`'s vacuity guard cannot: it overrides to failure
only when the whole interpreter run executed NO assertion, so an owner
dropped from `FAMILIES` would still leave five families' assertions
counted and the aggregate would report success having silently skipped
twenty groups. So the family roster is cross-checked against the modules
actually on disk, in both directions, the way `tools/playtest/selftest.py`
(#2040) derives its component roster independently of its registry; every
family must declare at least the number of groups it carried at the
split, floored PER FAMILY so one family cannot be emptied while another
grows by the same amount; each family's fragments must concatenate to
exactly its own `TESTS`; and the sequence must then run every declared
group exactly once. A focused `--family` run is served from the same
checked inventories, so it is the aggregate's groups for that family, in
the aggregate's order, never a separately maintained list.

The synthetic-only boundary is unchanged and belongs to every family: no
registered behavior probe is executed, no real engine or worldgen process
is started, every spawned synthetic process group is reaped, and the
throwaway tree and per-case resource namespace are cleaned on every
outcome -- including on a focused run, which never depends on a sibling
family having run.

Usage:
  python3 tools/test_run_probes.py
  python3 tools/test_run_probes.py -v
  python3 tools/test_run_probes.py --family lifecycle
Exit codes:
  0 = all tests passed
  1 = one or more failed, or the composition refused to run
  2 = an unrecognized argument, including an unknown --family
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import selftestlib  # noqa: E402
from selftestlib import FAILURES  # noqa: E402
from probe_runner_tests import (  # noqa: E402
    diagnostics, lifecycle, readme, registry, resources, scheduler,
)

PACKAGE_DIR = Path(__file__).resolve().parent / "probe_runner_tests"

#: The six families by their command-line name, each mapped to the module
#: that owns its groups. The keys are what `--family` accepts.
FAMILIES = {
    "readme": readme,
    "registry": registry,
    "resources": resources,
    "lifecycle": lifecycle,
    "scheduler": scheduler,
    "diagnostics": diagnostics,
}

#: Everything in the package that is NOT a case owner. Deriving the roster
#: from the directory is what makes the coverage check work in both
#: directions: a seventh owner module cannot be added without either
#: joining `FAMILIES` or failing here, and a family dropped from
#: `FAMILIES` while its module remains is caught too. `tools/playtest/`
#: (#2040) and `tools/test_persistence_inventory_audit.py` (#2138) use the
#: same shape, for the same reason -- a check that enumerated the registry
#: would still pass after a registry entry was deleted.
NON_OWNER_MODULES = {"__init__", "support"}

#: The group count each family carried at this split: 2 + 13 + 20 + 14 +
#: 14 + 11 = 74. The issue pinned 1 + 13 + 20 + 14 + 14 + 11 = 73 at
#: 837792c; #2035 has since landed `test_a_failing_readme_audit_fails_this
#: _gate` beside the audit it extracted, which is the whole of the delta
#: and is why `readme` is 2. A FLOOR, not an exact count, so a
#: legitimately added group joins without an edit here; a family declaring
#: FEWER is a truncation and is refused.
MINIMUM_GROUPS = {
    "readme": 2,
    "registry": 13,
    "resources": 20,
    "lifecycle": 14,
    "scheduler": 14,
    "diagnostics": 11,
}

#: The aggregate's order, as which family fragment runs when. Twenty-one
#: entries for six families, because the run interleaves them: the
#: scheduler's fixture validation opens it, the lifecycle block follows,
#: and the port-span and diagnostics blocks close it. Each entry names a
#: family and one attribute on its module; no test name appears.
SEQUENCE_FRAGMENTS = (
    ("scheduler", "TESTS_FIXTURE_VALIDATION"),
    ("lifecycle", "TESTS_LIVENESS_AND_TEARDOWN"),
    ("scheduler", "TESTS_AGGREGATE_EXIT"),
    ("readme", "TESTS"),
    ("registry", "TESTS_TIMEOUT_DECLARATIONS"),
    ("scheduler", "TESTS_KEY_TIMEOUTS"),
    ("registry", "TESTS_EXCLUSIVE_DECLARATIONS"),
    ("resources", "TESTS_PREFLIGHT"),
    ("lifecycle", "TESTS_DIRECT_RUN_ONE"),
    ("resources", "TESTS_PROPAGATION_AND_HOLDS"),
    ("scheduler", "TESTS_RESOURCE_SCHEDULING"),
    ("resources", "TESTS_FOREIGN_HOLDERS"),
    ("registry", "TESTS_EXACT_SELECTION"),
    ("scheduler", "TESTS_RETRY_TEARDOWN"),
    ("lifecycle", "TESTS_ENGINE_LEDGER"),
    ("scheduler", "TESTS_INTERRUPTION"),
    ("lifecycle", "TESTS_LAUNCH_WINDOW"),
    ("scheduler", "TESTS_PORT_REBINDING"),
    ("registry", "TESTS_PORT_SPANS"),
    ("scheduler", "TESTS_NEIGHBOUR_ALLOCATION"),
    ("diagnostics", "TESTS"),
)


class CompositionError(Exception):
    """The family inventories no longer compose into the complete run."""


def _owner_modules_on_disk() -> set[str]:
    """The case-owner module names the package directory actually holds."""
    return {path.stem for path in PACKAGE_DIR.glob("*.py")
            if path.stem not in NON_OWNER_MODULES}


def inventories() -> dict[str, list]:
    """Every family's declared inventory, each at least its historical size.

    Checked before the run sequence is consulted, so a family that lost
    its `TESTS`, was emptied, or shrank is reported as itself, by name,
    rather than as whatever the sequence notices second. The roster is
    checked against the modules on disk first, because a family omitted
    from `FAMILIES` declares nothing to be short -- and that omission is
    exactly the silent shortening `selftestlib.concluded` cannot see.
    """
    on_disk = _owner_modules_on_disk()
    declared = {module.__name__.rsplit(".", 1)[-1]
                for module in FAMILIES.values()}
    if on_disk != declared:
        raise CompositionError(
            f"the family roster disagrees with the package directory: "
            f"modules present but not registered as a family "
            f"{sorted(on_disk - declared)}, families registered whose module "
            f"is missing {sorted(declared - on_disk)}")

    found: dict[str, list] = {}
    for name, module in FAMILIES.items():
        tests = getattr(module, "TESTS", None)
        if tests is None:
            raise CompositionError(
                f"family {name!r} ({module.__name__}) declares no TESTS "
                f"inventory")
        floor = MINIMUM_GROUPS[name]
        if len(tests) < floor:
            raise CompositionError(
                f"family {name!r} ({module.__name__}) declares {len(tests)} "
                f"test group(s), fewer than the {floor} it has always carried "
                f"-- refusing to report a shortened run")
        found[name] = list(tests)
    return found


def compose(family: str | None = None) -> list:
    """The run sequence, checked against every family's own inventory.

    Both directions are checked, because either drift is a silent loss of
    coverage: a sequence entry no family declares, a declared group the
    sequence never runs, a group run twice, and a group two families both
    declare all fail here. A family's fragments must also concatenate to
    exactly its `TESTS`, which is what stops a fragment from being dropped
    out of the order while its groups still look accounted for.

    `family` selects one family's inventory instead, after the same
    checks -- a focused run is the aggregate's groups for that family, in
    the aggregate's order, never a separately maintained list.
    """
    by_family = inventories()

    sequence: list = []
    fragments_seen: dict[str, list] = {name: [] for name in FAMILIES}
    for name, attribute in SEQUENCE_FRAGMENTS:
        fragment = getattr(FAMILIES[name], attribute, None)
        if fragment is None:
            raise CompositionError(
                f"family {name!r} declares no {attribute!r} fragment, which "
                f"the run order names")
        if not fragment:
            raise CompositionError(
                f"family {name!r}'s {attribute!r} fragment is empty")
        sequence.extend(fragment)
        fragments_seen[name].extend(fragment)

    for name, seen in fragments_seen.items():
        if seen == by_family[name]:
            continue
        ordered = [test.__name__ for test in seen]
        owned = [test.__name__ for test in by_family[name]]
        detail = []
        if [n for n in owned if n not in ordered]:
            detail.append(f"groups the order never runs: "
                          f"{[n for n in owned if n not in ordered]}")
        if [n for n in ordered if n not in owned]:
            detail.append(f"groups the order runs that the family does not "
                          f"declare: {[n for n in ordered if n not in owned]}")
        if not detail:
            detail.append(f"the same groups in a different order -- the order "
                          f"runs {ordered}, the family declares {owned}")
        raise CompositionError(
            f"family {name!r}'s fragments in the run order do not reconstruct "
            f"its TESTS -- " + "; ".join(detail))

    declared: dict[str, str] = {}
    for name, tests in by_family.items():
        within: set[str] = set()
        for test in tests:
            if test.__name__ in within:
                raise CompositionError(
                    f"family {name!r} declares test group {test.__name__!r} "
                    f"more than once")
            within.add(test.__name__)
            if test.__name__ in declared:
                raise CompositionError(
                    f"test group {test.__name__!r} is declared by both "
                    f"{declared[test.__name__]!r} and {name!r}")
            declared[test.__name__] = name

    ran: list[str] = []
    for test in sequence:
        if test.__name__ not in declared:
            raise CompositionError(
                f"the run sequence includes {test.__name__!r}, which no "
                f"family declares in its TESTS")
        if test.__name__ in ran:
            raise CompositionError(
                f"the run sequence runs {test.__name__!r} more than once")
        ran.append(test.__name__)

    missing = sorted(f"{owner}:{group}" for group, owner in declared.items()
                     if group not in ran)
    if missing:
        raise CompositionError(
            f"declared test groups the run sequence never runs: {missing}")

    return by_family[family] if family else sequence


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Self-test for run_probes.py and the owner modules "
                    "behind it.")
    parser.add_argument(
        "--family", choices=sorted(FAMILIES),
        help="run one family's test groups instead of the whole aggregate")
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(argv)
    selftestlib.begin(args.verbose)

    try:
        tests = compose(args.family)
    except CompositionError as error:
        print(f"test_run_probes composition error: {error}", file=sys.stderr)
        return 1

    for test in tests:
        test()

    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    if args.family:
        return selftestlib.concluded(
            0, f"\nAll {len(tests)} run_probes process-suite tests passed "
               f"in family {args.family}")
    return selftestlib.concluded(
        0, "\nAll run_probes process-suite tests passed, "
           "tools/README.md registry-count audit included")


if __name__ == "__main__":
    raise SystemExit(main())
