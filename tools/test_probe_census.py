#!/usr/bin/env python3
"""The complete self-test for the probe census (#1428, split #2129).

Deterministic, engine-free, GPU-free and offline: every case runs
against synthetic documents in a throwaway temporary tree. Nothing here
boots an engine, runs a registered probe, touches the developer's real
`docs-wip` worktree, or shells out to anything but `git` (to build a
two-worktree scratch repository the CLI cases can resolve) and this same
interpreter (for the independent-process contention case).

The real `tools/probe_census.py` is imported and driven -- with
`probe_runner_registry.PROBES`, `probe_engine.REPO_ROOT`,
`ci_probes.CI_ELIGIBLE` and `probe_flake.PROTOCOL_PROBES` pointed at a
synthetic registry by `probe_census_selftest_support.registry` -- so this
exercises the shipped code paths rather than a copy.

Composition (#2034, #2129)
--------------------------
This stays the COMPLETE census gate, and the only one CI and
`tools/ci-local.sh` invoke. This module is selection, ordered
aggregation and reporting only: it holds no test body and names no
individual test group. The cases live with six owners, each declaring
its own ordered inventory:

  `probe_census_tests.storage`     12  the record, its migrations, and
                                       how it reaches disk;
  `probe_census_tests.policy`      11  #1430's acceptable-failure
                                       policy, the refusal contract, and
                                       the mutation CLI;
  `probe_census_tests.validation`   5  #1492's declared schema and
                                       #1493's cross-field invariants;
  `probe_census_tests.cohort`       9  #1429's cohorts, staleness and
                                       the summary reader;
  `probe_census_tests.outcomes`     2  #1439's outcome log and the
                                       deferral gate;
  `test_probe_census_promotion`     5  #1441's CI-promotion report,
                                       extracted by #2034 and still
                                       living beside this file.

`probe_census_selftest_support` is the ONE synthetic world all six
drive -- the registries and the fixture that installs them, the scratch
tree and scratch repository, the CLI driver, the realistic result
document, the fixed evaluation moment, and `expect_refusal`. It is
shared with the promotion owner, which is outside the package, so it
stays at the top level; `probe_census_tests.support` re-exports it and
adds the fixtures more than one family INSIDE the package reads.

Running the promotion cases HERE is the point rather than an
implementation detail: they append to the same `selftestlib.FAILURES`,
so a promotion regression still fails `python3
tools/test_probe_census.py`, and a case added to that owner joins this
gate without anyone remembering to list it. That owner runs its own
block through its `run_cases()`, which adds the guard that no fixture
leaked a patched registry -- iterating `CASES` here would silently drop
it. The promotion owner is separately runnable for iteration and is a CI
step in NEITHER file -- the `tools/test_probe_census_page.py` precedent.

The aggregate's order is INTERLEAVED, and always has been: the two
outcomes groups run third, the storage family's five persistence groups
run after the whole validation family, and the two policy CLI groups run
after those. `SEQUENCE_FRAGMENTS` below is that order written as which
fragment runs when, so a family's groups can appear in more than one
place without any group name being written down twice. A focused
`--family` run is the same groups in the same relative order, never a
separately maintained list.

Before any group runs, `compose()` refuses an arrangement that could
report a shortened green run (#2129 requirements 5 and 12).
`selftestlib.concluded`'s vacuity guard cannot see this: it refuses only
a run that executed NO assertion, and a family dropped from the registry
still leaves thousands of assertions from the other five. So the family
roster is cross-checked against the modules on disk, every family must
declare at least the groups it carried at the split, each family's
fragments must concatenate to exactly its own inventory, and the
sequence must then run every declared group exactly once. The package's
modules are structurally checked too, because
`tools/test_selftestlib.py` globs `tools/*.py` NON-recursively and
therefore cannot see them: no owner may define an assertion helper, keep
its own failure accumulator, or narrate a passing assertion.

`compose()` then checks the PRODUCTION owners #2131 split the census
into, for the same reason and in the same place: no behavioural case can
see any of it. Every owner could import the facade, or two owners could
each define `parse_timestamp`, or an implementation body could stay
behind in `probe_census.py`, and all 44 groups would still pass over a
module family that was one circular tangle again. So before anything
runs: no owner imports the facade or an owner below it in the dependency
order, no two owners define the same top-level name, and the facade
defines exactly its CLI surface and re-exports the rest.

Usage:
  python3 tools/test_probe_census.py
  python3 tools/test_probe_census.py -v
  python3 tools/test_probe_census.py --family policy
The bare form is the gate, and it is unchanged: every group, in the
aggregate's order. `--family` runs one owner's groups and nothing else,
which is most of the iteration cost gone for five of the six -- the
validation family drives the exhaustive schema surface and dominates the
runtime; the others each finish in well under a second.

Exit codes:
  0 = all tests passed
  1 = one or more failed, or the composition refused to run
  2 = an unrecognized argument, including an unknown --family
"""
from __future__ import annotations

import argparse
import ast
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import selftestlib  # noqa: E402
from selftestlib import FAILURES  # noqa: E402
from probe_census_tests import (  # noqa: E402
    cohort, outcomes, policy, storage, validation,
)
import test_probe_census_promotion as promotion  # type: ignore  # noqa: E402

PACKAGE_DIR = Path(__file__).resolve().parent / "probe_census_tests"
TOOLS_DIR = Path(__file__).resolve().parent

#: The census owners in DEPENDENCY ORDER (#2131), and the facade last.
#: An owner may import an owner ABOVE it and nothing below, and none of
#: them may import the facade -- which imports all five, so an owner
#: importing it back would close a cycle at import time. Written as an
#: order rather than a set of allowed pairs because the order is the
#: contract: contract, then records, then summary, then storage, then
#: promotion, all under the facade.
OWNER_ORDER = ("probe_census_contract", "probe_census_records",
               "probe_census_summary", "probe_census_storage",
               "probe_census_promotion")
FACADE_MODULE = "probe_census"

#: What the facade is allowed to define ITSELF (#2131 requirement 15):
#: argument validation, presentation and dispatch. Every other name it
#: exposes must arrive by import from exactly one owner. A FLOOR is
#: wrong here -- this is an exact set, because the failure being
#: prevented is an implementation body staying behind or coming back.
FACADE_DEFINITIONS = {
    "_acceptable_failures_argument", "_companion_arguments",
    "_deferral_arguments", "_optional_number", "_rate_text",
    "_summary_arguments", "main", "render_summary",
}

#: The six families by the name `--family` accepts, each mapped to the
#: module that owns its groups and the attribute naming that owner's
#: complete ordered inventory. Promotion's is `CASES` because #2034
#: named it that; this façade reads that inventory rather than
#: re-declaring the five group names it holds.
FAMILIES = {
    "storage": (storage, "TESTS"),
    "policy": (policy, "TESTS"),
    "validation": (validation, "TESTS"),
    "cohort": (cohort, "TESTS"),
    "outcomes": (outcomes, "TESTS"),
    "promotion": (promotion, "CASES"),
}

#: The one family whose owner runs its own block. `run_cases()` runs
#: every entry of `CASES` and then asserts that no fixture leaked a
#: patched registry -- an assertion this gate has made since #2034 and
#: which iterating `CASES` from here would drop. A family listed here
#: must contribute exactly one fragment, equal to its whole inventory,
#: which `compose()` enforces.
BLOCK_RUNNERS = {"promotion": promotion.run_cases}

#: Everything in the package that is NOT a case owner. Deriving the
#: roster from the directory means a sixth in-package family cannot be
#: added without either joining `FAMILIES` or failing this check, and a
#: family dropped from `FAMILIES` while its module remains is caught too
#: (the shape `tools/test_location_content_probe.py` uses for #2095).
NON_OWNER_MODULES = {"__init__", "support"}

#: The group count each family carried at the split (#2129): 12 + 11 + 5
#: + 9 + 2 + 5 = 44, the aggregate's count at this PR's base. A FLOOR,
#: not an exact count, so a legitimately added group joins without an
#: edit here; a family declaring FEWER is a truncation and is refused.
MINIMUM_GROUPS = {
    "storage": 12,
    "policy": 11,
    "validation": 5,
    "cohort": 9,
    "outcomes": 2,
    "promotion": 5,
}

#: The aggregate's order, as which family fragment runs when. Eight
#: entries for six families, because the order interleaves them rather
#: than finishing one family before starting the next -- and that order
#: is the one this gate has always had. Each entry names a family and one
#: attribute on its module; no test name appears.
SEQUENCE_FRAGMENTS = (
    ("storage", "TESTS_RECORDS_AND_INGESTION"),
    ("outcomes", "TESTS"),
    ("policy", "TESTS_POLICY"),
    ("validation", "TESTS"),
    ("storage", "TESTS_PATHS_AND_PERSISTENCE"),
    ("policy", "TESTS_CLI"),
    ("cohort", "TESTS"),
    ("promotion", "CASES"),
)

#: The aggregate's group count at the split (#2129), and a FLOOR rather
#: than an exact count so a legitimately added group joins without an
#: edit here. Stated independently of `MINIMUM_GROUPS` above, which sums
#: to the same 44: lowering one family's floor to make room for a
#: truncation still has to get past this one.
AGGREGATE_GROUPS_AT_THE_SPLIT = 44

#: The two narrating bodies #1922 removed, as `tools/test_selftestlib.py`
#: spells the search. Repeated rather than imported: that module owns a
#: gate over `tools/*.py`, and what is checked here is the directory its
#: non-recursive glob cannot reach.
NARRATION = re.compile(r'print\(f"  (OK:|\{.ok  .)')

#: The direct append `test_selftestlib.py` forbids, spelled in halves so
#: this file does not contain the literal it searches the package for.
#: That gate greps `tools/*.py` for the two words joined, and would
#: otherwise read the search itself as the offence.
DIRECT_APPEND = "FAILURES" + ".append"


class CompositionError(Exception):
    """The owners no longer compose into the complete run."""


def _owner_modules_on_disk() -> set[str]:
    """The case-owner module names the package directory actually holds."""
    return {path.stem for path in PACKAGE_DIR.glob("*.py")
            if path.stem not in NON_OWNER_MODULES}


def _check_package_structure() -> None:
    """The three properties `test_selftestlib.py`'s glob cannot see.

    That gate derives its rosters from a NON-recursive `tools/*.py`, so
    every module under `probe_census_tests/` is outside all three of its
    static checks. They are re-made here, over the package's own files,
    because the split is what moved the assertion bodies out of their
    reach -- and `support.py` is checked alongside the owners, since a
    second `expect` there would be the most natural place to grow one.

    Non-vacuous by construction: `inventories()` has already refused a
    package directory that does not hold exactly the five in-package
    families, so this scan can never run over an empty file set.
    """
    for path in sorted(PACKAGE_DIR.glob("*.py")):
        text = path.read_text(encoding="utf-8")
        local = [node.name for node in ast.parse(text).body
                 if isinstance(node, ast.FunctionDef)
                 and node.name == "expect"]
        if local:
            raise CompositionError(
                f"probe_census_tests/{path.name} defines its own expect -- "
                f"every owner asserts through selftestlib's one helper")
        if DIRECT_APPEND in text:
            raise CompositionError(
                f"probe_census_tests/{path.name} uses {DIRECT_APPEND} -- a "
                f"failure registered behind the assertion tally")
        if NARRATION.search(text):
            raise CompositionError(
                f"probe_census_tests/{path.name} narrates a passing "
                f"assertion -- only selftestlib does that, under --verbose")



def _module_source(name: str) -> str:
    return (TOOLS_DIR / f"{name}.py").read_text(encoding="utf-8")


def _defined_names(tree: ast.Module) -> set[str]:
    """Top-level names a module DEFINES, as opposed to imports."""
    names: set[str] = set()
    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef,
                             ast.ClassDef)):
            names.add(node.name)
        elif isinstance(node, ast.Assign):
            names.update(target.id for target in node.targets
                         if isinstance(target, ast.Name))
        elif isinstance(node, ast.AnnAssign) and isinstance(node.target,
                                                            ast.Name):
            names.add(node.target.id)
    return names


def _imported_modules(tree: ast.Module) -> set[str]:
    """Every module name imported ANYWHERE in the file, nesting included.

    Walked rather than read off `tree.body` on purpose: a point-of-use
    import inside a function is exactly how a cycle would be smuggled
    back in, and it is invisible to a top-level scan.
    """
    modules: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            modules.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module and not node.level:
            modules.add(node.module.split(".")[0])
    return modules


def _check_owner_structure() -> None:
    """The three properties #2131 split the census to get, re-checked.

    None of them is visible to a behavioural case: every owner could
    import the facade, or two owners could each define `parse_timestamp`,
    and the 44 groups would still pass -- the census would simply be one
    circular module family again, one edit from the tangle the split
    removed. So they are checked HERE, before anything runs, over the
    files on disk:

      1. no owner imports the facade, and no owner imports an owner
         BELOW it in `OWNER_ORDER` -- together, that the graph is
         acyclic and flows the one direction the split declared;
      2. no two owners define the same top-level name, so a contract,
         parser, transformation or serializer has ONE implementation and
         a compatibility re-export cannot quietly become a second;
      3. the facade defines exactly `FACADE_DEFINITIONS` and re-exports
         everything else, so no owner-specific body survives in it.
    """
    trees = {name: ast.parse(_module_source(name))
             for name in (*OWNER_ORDER, FACADE_MODULE)}

    for position, owner in enumerate(OWNER_ORDER):
        imported = _imported_modules(trees[owner])
        if FACADE_MODULE in imported:
            raise CompositionError(
                f"{owner}.py imports {FACADE_MODULE} -- the facade imports "
                f"every owner, so an owner importing it back is a cycle")
        below = set(OWNER_ORDER[position + 1:]) & imported
        if below:
            raise CompositionError(
                f"{owner}.py imports {sorted(below)}, which come AFTER it in "
                f"the census dependency order {list(OWNER_ORDER)}")

    owners_by_name: dict[str, str] = {}
    for owner in OWNER_ORDER:
        for name in _defined_names(trees[owner]):
            if name in owners_by_name:
                raise CompositionError(
                    f"{name!r} is defined by both {owners_by_name[name]}.py "
                    f"and {owner}.py -- every census contract and "
                    f"transformation has exactly one implementation")
            owners_by_name[name] = owner

    facade_defines = _defined_names(trees[FACADE_MODULE])
    extra = facade_defines - FACADE_DEFINITIONS
    if extra:
        raise CompositionError(
            f"{FACADE_MODULE}.py defines {sorted(extra)} itself -- the facade "
            f"is imports, re-exports, argument validation, dispatch and "
            f"presentation, and holds no owner's implementation body")
    absent = FACADE_DEFINITIONS - facade_defines
    if absent:
        raise CompositionError(
            f"{FACADE_MODULE}.py no longer defines {sorted(absent)} -- the "
            f"CLI surface moved out from under this check")


def inventories() -> dict[str, list]:
    """Every family's declared inventory, each at least its historical size.

    Checked before the run sequence is consulted, so a family that lost
    its inventory, was emptied, or shrank is reported as itself, by name,
    rather than as whatever the sequence notices second. The roster is
    checked against the modules on disk first, because a family omitted
    from `FAMILIES` declares nothing to be short -- which is exactly the
    single-family omission `selftestlib.concluded` structurally cannot
    detect in an aggregate the other five still fill with assertions.
    """
    on_disk = _owner_modules_on_disk()
    in_package = {name for name, (module, _attribute) in FAMILIES.items()
                  if module.__name__.startswith("probe_census_tests.")}
    declared = {FAMILIES[name][0].__name__.rsplit(".", 1)[-1]
                for name in in_package}
    if on_disk != declared:
        raise CompositionError(
            f"the family roster disagrees with the package directory: "
            f"modules present but not registered as a family "
            f"{sorted(on_disk - declared)}, families registered whose module "
            f"is missing {sorted(declared - on_disk)}")

    found: dict[str, list] = {}
    for name, (module, attribute) in FAMILIES.items():
        tests = getattr(module, attribute, None)
        if tests is None:
            raise CompositionError(
                f"family {name!r} ({module.__name__}) declares no "
                f"{attribute} inventory")
        floor = MINIMUM_GROUPS[name]
        if len(tests) < floor:
            raise CompositionError(
                f"family {name!r} ({module.__name__}) declares {len(tests)} "
                f"test group(s), fewer than the {floor} it has always carried "
                f"-- refusing to report a shortened run")
        found[name] = list(tests)
    return found


def compose(family: str | None = None) -> list[tuple[str, tuple]]:
    """The run plan, checked against every family's own inventory.

    Returns the ordered fragments to run, each as its family's name and
    its groups, so the caller knows which of them the owner runs itself.

    Both directions are checked, because either drift is a silent loss of
    coverage: a sequence entry no family declares, a declared group the
    sequence never runs, a group run twice, and a group two families both
    declare all fail here. A family's fragments must also concatenate to
    exactly its own inventory, which is what stops a fragment from being
    dropped out of the order while its groups still look accounted for.

    `family` selects one family's inventory instead, after the same
    checks -- a focused run is the aggregate's groups for that family, in
    the aggregate's order, never a separately maintained list.
    """
    by_family = inventories()
    _check_package_structure()
    _check_owner_structure()

    plan: list[tuple[str, tuple]] = []
    fragments_seen: dict[str, list] = {name: [] for name in FAMILIES}
    for name, attribute in SEQUENCE_FRAGMENTS:
        if name not in FAMILIES:
            raise CompositionError(
                f"the run order names family {name!r}, which is not one of "
                f"{sorted(FAMILIES)}")
        module = FAMILIES[name][0]
        fragment = getattr(module, attribute, None)
        if fragment is None:
            raise CompositionError(
                f"family {name!r} declares no {attribute!r} fragment, which "
                f"the run order names")
        if not fragment:
            raise CompositionError(
                f"family {name!r}'s {attribute!r} fragment is empty")
        plan.append((name, tuple(fragment)))
        fragments_seen[name].extend(fragment)

    for name in BLOCK_RUNNERS:
        mine = [groups for owner, groups in plan if owner == name]
        if len(mine) != 1 or list(mine[0]) != by_family[name]:
            raise CompositionError(
                f"family {name!r} runs its own block, so the order must give "
                f"it exactly one fragment holding its whole inventory "
                f"(it has {len(mine)})")

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
            f"its inventory -- " + "; ".join(detail))

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
    for _name, groups in plan:
        for test in groups:
            if test.__name__ not in declared:
                raise CompositionError(
                    f"the run sequence includes {test.__name__!r}, which no "
                    f"family declares in its inventory")
            if test.__name__ in ran:
                raise CompositionError(
                    f"the run sequence runs {test.__name__!r} more than once")
            ran.append(test.__name__)

    missing = sorted(f"{owner}:{group}" for group, owner in declared.items()
                     if group not in ran)
    if missing:
        raise CompositionError(
            f"declared test groups the run sequence never runs: {missing}")

    if len(set(ran)) < AGGREGATE_GROUPS_AT_THE_SPLIT:
        raise CompositionError(
            f"the composed aggregate runs {len(set(ran))} uniquely named test "
            f"group(s), fewer than the {AGGREGATE_GROUPS_AT_THE_SPLIT} it "
            f"carried at the split -- refusing to report a shortened run")

    if family:
        return [(family, tuple(by_family[family]))]
    return plan


def run(plan: list[tuple[str, tuple]]) -> None:
    """Run each fragment, letting an owner with its own runner run its own.

    Only `promotion` has one, and it is why this iterates fragments
    rather than a flat list of groups: `run_cases()` runs its five cases
    and then asserts that no fixture leaked a patched registry, an
    assertion this gate has made since #2034.
    """
    for name, groups in plan:
        runner = BLOCK_RUNNERS.get(name)
        if runner is not None:
            runner()
        else:
            for test in groups:
                test()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Self-test for the probe census record and its writer.")
    parser.add_argument(
        "--family", choices=sorted(FAMILIES),
        help="run one family's test groups instead of the whole aggregate")
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(argv)
    selftestlib.begin(args.verbose)

    try:
        plan = compose(args.family)
    except CompositionError as error:
        print(f"test_probe_census composition error: {error}", file=sys.stderr)
        return 1

    run(plan)
    print()
    if FAILURES:
        print(f"{len(FAILURES)} FAILED:")
        for message in FAILURES:
            print(f"  - {message}")
        return selftestlib.concluded(1)
    scope = "" if args.family is None else f" ({args.family} family)"
    return selftestlib.concluded(
        0, f"probe_census self-test: all cases pass{scope}")


if __name__ == "__main__":
    sys.exit(main())
