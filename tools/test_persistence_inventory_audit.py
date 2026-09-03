#!/usr/bin/env python3
"""Self-test for persistence_inventory_audit.py and the owner modules
behind it -- persistence_inventory_audit_common / _haskell / _lua /
_policy, issue #2124 -- (issue #756 acceptance: "the audit detects an
intentionally introduced unclassified root state owner or Lua
persistence module in its own automated test").

Mostly feeds the audit's pure functions synthetic Haskell record text,
synthetic Lua source, and a synthetic inventory doc, so those tests stay
stable regardless of how EngineEnv or the inventory grow. A handful of
groups deliberately read the REAL checked-out sources instead, because
what they assert is a property of the production configuration rather
than of the parser: `test_audit_against_the_real_repo` (the end-to-end
smoke test CI runs via main()), the three #1703 groups binding the
pointer-reached gameplay managers to the production ROOT_RECORDS, and
the #2124 groups over the ownership split's own structure. All of them
read the tree and mutate only in-memory copies; none writes a repo file.

Composition (#2138)
-------------------
This module is selection, ordered aggregation and reporting only -- it
holds no test body and names no test group. The 173 groups live with six
family owners under `persistence_inventory_audit_tests/`, each declaring
its own ordered inventory in `TESTS`:

  `haskell`     24  record-field extraction, the end-to-end mutations
                    that hide or expose a Haskell field, and #1703's
                    pointer-reached managers;
  `lua_parser`  70  the four Lua scanners, asserted on extracted names:
                    every registration, alias, dynamic-name and
                    registry-escape spelling, each with its
                    false-positive control;
  `lua_audit`   44  the same Lua fixtures through `audit()`, asserted on
                    the classification violation each must or must not
                    raise;
  `inventory`   13  owner-heading scope, the classification taxonomy,
                    #756's intentionally-unclassified pair, and the real
                    repository;
  `references`   6  #764's typed Haskell references and Lua reference
                    kinds, discovered and enforced;
  `topology`    16  #760 component registration, #767's coverage map,
                    registry derivation, and #2124's split structure.

`support` is what two or more of them share -- the one `sys.path` entry
that lets a child reach the production modules, and #1922's `expect`
re-exported. Three fixture modules hold the synthetic sources read by
more than one family; a fixture only one family reads stays with that
family. Dependencies run one way: support and fixtures import no case
owner, case owners import support, their fixtures and the production
module that owns what they exercise, and only this façade imports the
case owners. Importing any of them runs no test.

`compose()` builds the run sequence this gate has always used. Five of
the six families interleave in it, so each declares its blocks as
FRAGMENTS -- `haskell.TESTS_FIELD_MUTATIONS` runs after the inventory's
classification-parsing block, not beside its own parser cases -- and
`SEQUENCE_FRAGMENTS` below is the aggregate's order written as which
fragment runs when. Naming a fragment is not naming a test: every group
name still lives with exactly one owner.

Before any group runs, the composition is checked so no arrangement of
owners can report a shortened green run (#2138 requirement 19). The
family roster is cross-checked against the modules actually on disk, so
a family dropped from the roster fails rather than vanishing quietly;
every family must declare at least the number of groups it carried at
the split, floored PER FAMILY because one aggregate floor would let one
family be emptied while another grew by the same amount; each family's
fragments must concatenate to exactly its own `TESTS`; and the sequence
must then run every declared group exactly once -- no duplicate, no
group two families both declare, no declared group left out, no sequence
entry no family declares. `selftestlib.concluded` is the last guard
behind those: a run that executed no assertion at all is a failure
whatever this module believed.

What that guard does NOT do is second-guess `SEQUENCE_FRAGMENTS` itself,
because there is nothing independent left to check it against: it IS the
declaration of the aggregate's order, and it was written by diffing this
gate's stdout against the pre-split run until the two were byte-identical
(#2138). Reordering it is a deliberate edit to a declared contract, not
the silent truncation the checks above exist to refuse -- and it is
harmless to every verdict besides, since no family may depend on another
having run first (which is what each family passing alone proves).

Usage:
  python3 tools/test_persistence_inventory_audit.py
  python3 tools/test_persistence_inventory_audit.py -v
  python3 tools/test_persistence_inventory_audit.py --family lua-parser
The bare form is the gate CI and `tools/ci-local.sh` invoke, and it is
unchanged: every group, in the aggregate's order. `--family` runs one
family's groups and nothing else, which is most of the iteration cost
gone for the five families that do not generate a world -- the Haskell
family takes about 15 seconds and the inventory family about 3, because
they read real sources; the other four are under a tenth of a second.
A family runs the same groups, in the same order, whether selected on
its own or as part of the aggregate.

Exit codes:
  0 = all tests passed
  1 = one or more tests failed, or the composition refused to run
  2 = an unrecognized argument, including an unknown --family
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import selftestlib  # noqa: E402
from selftestlib import FAILURES  # noqa: E402
from persistence_inventory_audit_tests import (  # noqa: E402
    haskell, inventory, lua_audit, lua_parser, references, topology,
)

PACKAGE_DIR = Path(__file__).resolve().parent / "persistence_inventory_audit_tests"

#: The six families by their command-line name, each mapped to the module
#: that owns its groups. The keys are what `--family` accepts.
FAMILIES = {
    "haskell": haskell,
    "lua-parser": lua_parser,
    "lua-audit": lua_audit,
    "inventory": inventory,
    "references": references,
    "topology": topology,
}

#: Everything in the package that is NOT a case owner. Deriving the family
#: roster from the directory means a seventh family module cannot be added
#: without either joining `FAMILIES` or failing this check, and a family
#: dropped from `FAMILIES` while its module remains is caught too -- the
#: registry cannot disagree with what is on disk (the shape
#: `tools/test_location_content_probe.py` uses for #2095's split).
NON_OWNER_MODULES = {"__init__", "support",
                     "fixtures_haskell", "fixtures_lua", "fixtures_inventory"}

#: The group count each family carried at the split (#2138): 24 + 70 + 44
#: + 13 + 6 + 16 = 173, the aggregate's count at this PR's base. A FLOOR,
#: not an exact count, so a legitimately added group joins without an edit
#: here; a family declaring FEWER is a truncation and is refused.
MINIMUM_GROUPS = {
    "haskell": 24,
    "lua-parser": 70,
    "lua-audit": 44,
    "inventory": 13,
    "references": 6,
    "topology": 16,
}

#: The aggregate's order, as which family fragment runs when. Twelve
#: entries for six families: the run alternates between direct-parser and
#: end-to-end blocks rather than finishing one family before starting the
#: next, and that order is the one this gate has always had. Each entry
#: names a family and one attribute on its module; no test name appears.
SEQUENCE_FRAGMENTS = (
    ("haskell", "TESTS_RECORD_PARSING"),
    ("lua-parser", "TESTS"),
    ("inventory", "TESTS_CLASSIFICATION_PARSING"),
    ("haskell", "TESTS_FIELD_MUTATIONS"),
    ("lua-audit", "TESTS_LITERAL_FORMS"),
    ("inventory", "TESTS_OWNER_SCOPING_AND_TAXONOMY"),
    ("lua-audit", "TESTS_ALIAS_AND_ESCAPE_FORMS"),
    ("inventory", "TESTS_INTENTIONALLY_UNCLASSIFIED"),
    ("references", "TESTS"),
    ("inventory", "TESTS_REAL_REPO"),
    ("haskell", "TESTS_POINTER_REACHED"),
    ("topology", "TESTS"),
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
    from `FAMILIES` declares nothing to be short.
    """
    on_disk = _owner_modules_on_disk()
    declared = {module.__name__.rsplit(".", 1)[-1] for module in FAMILIES.values()}
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
    exactly its `TESTS`, which is what stops a fragment from being
    dropped out of the order while its groups still look accounted for.

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
        description="Self-test for the persistence-inventory audit.")
    parser.add_argument(
        "--family", choices=sorted(FAMILIES),
        help="run one family's test groups instead of the whole aggregate")
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(argv)
    selftestlib.begin(args.verbose)

    try:
        tests = compose(args.family)
    except CompositionError as error:
        print(f"test_persistence_inventory_audit composition error: {error}",
              file=sys.stderr)
        return 1

    for t in tests:
        print(f"{t.__name__}:")
        t()

    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s):")
        for f in FAILURES:
            print(f"  {f}")
        return selftestlib.concluded(1)

    scope = f" in family {args.family}" if args.family else ""
    return selftestlib.concluded(
        0, f"\nAll {len(tests)} test groups passed{scope}")


if __name__ == "__main__":
    raise SystemExit(main())
