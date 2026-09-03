#!/usr/bin/env python3
"""Shared self-test for world_audit.py, world_check.py and world_baseline.py.

Synthetic tile grids and dumps exercise each production check and every
gate decision the three tools make; nothing here generates a world, boots
an engine, invokes `--dump`, or writes under `tools/baselines/`. Sub-second
on a warm checkout.

Composition (#2070)
-------------------
This module is composition, dispatch and reporting only -- it holds no
test body. The 65 test groups live with six owners, each of which keeps
its own ordered inventory in `TESTS`:

  `test_audit_categories`        the emitted-category inventory derived
                                 by AST from the source of every module
                                 backing the live ALL_CHECKS registry,
                                 its two mutation groups, and the #2224
                                 closure of that derivation (3);
  `test_audit_world_audit`       world_audit's checks over synthetic
                                 grids, the tracked real lava dump, and
                                 the #2224 registry composition and
                                 façade surface (28);
  `test_audit_world_check`       world_check.py's summary comparison and
                                 determinism status (14);
  `test_audit_content_hash`      the baseline content-hash gate, #1361 (4);
  `test_audit_strict_capture`    world_baseline.py's strict capture
                                 invariants, #1598 (9);
  `test_audit_missing_baseline`  world_check.py's missing-baseline exit
                                 policy, #1319 (7).

`test_audit_support` is the single source of what two or more of them
share: the #1922 assertion facility, the audit-clean hash fixtures, and
the exit/output assertions over a captured tool run. Dependencies run one
way -- support imports no owner, owners import support and the production
module they exercise, and only this façade imports the owners.

`compose()` builds the run sequence this gate has always used: the owner
inventories concatenated, with one seam. The world_audit owner exposes
its inventory in two fragments (`TESTS_LEADING`, `TESTS_TRAILING`) so
the two category-inventory groups keep their historical position inside
the audit block, between the below-sea groups and the slope checks.

Before any group runs, the composition is checked so no arrangement of
owners can report a shortened green run. Every owner must declare at
least the number of groups it has always carried -- `MINIMUM_GROUPS`,
floored PER OWNER because a single aggregate floor would let one owner
be emptied while another grows by the same amount -- and the diagnostic
names the owner that came up short. The sequence must then run every
declared group exactly once: no duplicate, no group two owners both
declare, no declared group left out, no sequence entry no owner declares.
`selftestlib.concluded` is the last guard behind those: a run that
executed no assertion at all is a failure whatever this module believed.

Usage:
  python3 tools/test_audit.py          every group, in order
  python3 tools/test_audit.py -v       narrate passing assertions too
The bare form is the gate CI and `tools/ci-local.sh` invoke. The owner
modules expose no command line of their own.

Exit codes:
  0 = all tests passed
  1 = one or more tests failed, or the composition refused to run
"""

from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import selftestlib  # noqa: E402
from selftestlib import FAILURES  # noqa: E402
import test_audit_categories as categories  # noqa: E402
import test_audit_content_hash as content_hash  # noqa: E402
import test_audit_missing_baseline as missing_baseline  # noqa: E402
import test_audit_strict_capture as strict_capture  # noqa: E402
import test_audit_world_audit as world_audit_cases  # noqa: E402
import test_audit_world_check as world_check_cases  # noqa: E402

#: The six owners by name, in the order their inventories run.
OWNERS = {
    "world_audit": world_audit_cases,
    "categories": categories,
    "world_check": world_check_cases,
    "content_hash": content_hash,
    "strict_capture": strict_capture,
    "missing_baseline": missing_baseline,
}

#: The group count each owner carried at the split (#2070, requirement
#: 16): 28 audit/category groups as 26 + 2, then 14, 4, 9 and 7 -- 62 in
#: all. A floor, not an exact count, so a legitimately added group joins
#: without an edit here; an owner declaring FEWER is a truncation. The
#: world_audit floor rose to 28 and the categories floor to 3 with
#: #2224, which added the registry composition and façade surface groups
#: to the first and the inventory-closure group to the second.
MINIMUM_GROUPS = {
    "world_audit": 28,
    "categories": 3,
    "world_check": 14,
    "content_hash": 4,
    "strict_capture": 9,
    "missing_baseline": 7,
}


class CompositionError(Exception):
    """The owner inventories no longer compose into the complete run."""


def inventories() -> dict[str, list]:
    """Every owner's declared inventory, each at least its historical size.

    An owner that has stopped declaring `TESTS`, and one that declares
    fewer groups than `MINIMUM_GROUPS` records for it, are both refused
    here -- before the run sequence is consulted -- so an owner losing
    its inventory is reported as itself, by name, rather than as whatever
    the sequence notices second.
    """
    found: dict[str, list] = {}
    for name, module in OWNERS.items():
        tests = getattr(module, "TESTS", None)
        if tests is None:
            raise CompositionError(
                f"owner {name!r} ({module.__name__}) declares no TESTS "
                f"inventory")
        floor = MINIMUM_GROUPS[name]
        if len(tests) < floor:
            raise CompositionError(
                f"owner {name!r} ({module.__name__}) declares "
                f"{len(tests)} test group(s), fewer than the {floor} it has "
                f"always carried -- refusing to report a shortened run")
        found[name] = list(tests)
    return found


def compose() -> list:
    """The full run sequence, checked against every owner's inventory.

    Checks both directions, because either drift is a silent loss of
    coverage: a sequence entry no owner declares, a declared group the
    sequence never runs, a group run twice, and a group two owners both
    declare all fail here. Every group belongs to exactly one owner and
    runs exactly once.
    """
    by_owner = inventories()
    sequence = [
        *world_audit_cases.TESTS_LEADING,
        *categories.TESTS,
        *world_audit_cases.TESTS_TRAILING,
        *world_check_cases.TESTS,
        *content_hash.TESTS,
        *strict_capture.TESTS,
        *missing_baseline.TESTS,
    ]

    declared: dict[str, str] = {}
    for name, tests in by_owner.items():
        for test in tests:
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
                f"owner declares in its TESTS")
        if test.__name__ in ran:
            raise CompositionError(
                f"the run sequence runs {test.__name__!r} more than once")
        ran.append(test.__name__)

    missing = sorted(f"{owner}:{group}" for group, owner in declared.items()
                     if group not in ran)
    if missing:
        raise CompositionError(
            f"declared test groups the run sequence never runs: {missing}")
    return sequence


def main() -> int:
    selftestlib.parse_verbose()
    try:
        tests = compose()
    except CompositionError as error:
        print(f"test_audit composition error: {error}")
        return 1

    for t in tests:
        t()
        print()

    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s):")
        for f in FAILURES:
            print(f"  {f}")
        return selftestlib.concluded(1)

    return selftestlib.concluded(0, f"\nAll {len(tests)} test groups passed")


if __name__ == "__main__":
    raise SystemExit(main())
