#!/usr/bin/env python3
"""The focused self-test for engine_env_capability_writers.py -- the §5
writing-module scanner (issue #1892, CMA-1; #2059's fail-closed
accessor map; extracted from tools/test_engine_env_capability_audit.py
by issue #2036; split across case owners by issue #2228).

Every case drives the scanner's pure functions against SYNTHETIC
production trees -- a fake capability record plus whichever consumer
fixture a case needs, keyed by the relative path `module_identifier`
maps back to a module name -- never by editing a real module or the
real checked-in map. The three real-repository cases
(`test_projection_completeness_against_the_real_repo`,
`test_the_real_repo_declares_no_unreadable_capability_record`,
`test_writer_map_against_the_real_repo`) are the exceptions and say so;
all three are read-only.

Composition (#2228)
-------------------
This module is argument parsing, owner selection, composition,
execution and reporting only -- it holds no fixture and exactly one
test body, the composition guard below. The other 57 groups live with
four owners, each keeping its own ordered inventory in `TESTS`:

  `test_engine_env_capability_writers_map`          the writer-MAP
      contract: both consumer shapes canonicalizing onto one field, a
      declared write, an undeclared write, a stale entry, the map keys
      against the live field set, the §6.1 permanent exemption, and
      pass-on residue (7 groups);
  `test_engine_env_capability_writers_scanner`      the scanner
      MECHANICS: import scope in every spelling, tokenization of
      comments, strings and line numbers, and mutation-expression
      classification -- infix, strict, type applications, parentheses,
      sections, record-dot, shadowing, multiline (28 groups);
  `test_engine_env_capability_writers_projections`  capability-record
      and PROJECTION discovery: every declaration syntax, every
      binding shape that canonicalizes and every one that must fail
      closed, plus the two projection-side real-repository assertions
      (18 groups);
  `test_engine_env_capability_writers_conformance`  mutation-primitive
      provenance, shadow exemptions, and the writer map against the
      REAL repository (4 groups).

`test_engine_env_capability_writers_support` is the single source of
what two or more owners share: the assertion facility, the synthetic
field and permanent sets, the fake capability record, the two consumer
fixtures more than one owner drives, the generic path-based tree
builder, and the two scan adapters. Dependencies run one way -- support
imports no owner, owners import support and the production modules they
exercise, and only this façade imports the owners.

Runnable on its own for iteration, whole or one owner at a time:

  python3 tools/test_engine_env_capability_writers.py [-v]
  python3 tools/test_engine_env_capability_writers.py --only scanner

A focused run is a complete run of that owner and nothing else: it
imports no sibling owner's cases into its sequence, and `selftestlib`'s
per-invocation ledger is reset by the `begin` below, so an owner's
verdict never depends on which owner ran before it.

but NOT a separate CI gate, whole or focused.
`python3 tools/test_engine_env_capability_audit.py` -- the aggregate
self-test CI and tools/ci-local.sh run -- imports this module's flat
`TESTS` collection and runs every case in its own process, through the
same `selftestlib.FAILURES` collector, so a failure in any owner fails
that command exactly as it did before either split. Registering this
file, or any owner, on one side of the CI/`make ci` pair would fail
tools/ci_parity_audit.py; registering it on both would be the second
gate invocation issue #2036 rules out.

`OWNERS` is this façade's exported topology, and the aggregate reads it
rather than naming a child module: a group defined by a composed owner
still counts as defined by this owner there, so the aggregate's
both-direction inventory check keeps working across the split without
learning what the children are called.

`test_every_case_is_registered` keeps the whole arrangement honest: it
is the composition guard, and it is the last group in `TESTS`, so the
aggregate runs it too and no rearrangement of owners can report a
shortened green run.
Exit codes: 0 = all tests passed, 1 = one or more failed, 2 = the
command line was not understood.
"""
from __future__ import annotations

import argparse
import inspect
import sys
from pathlib import Path
from types import ModuleType

sys.path.insert(0, str(Path(__file__).resolve().parent))
import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402
import test_engine_env_capability_writers_conformance as conformance  # noqa: E402
import test_engine_env_capability_writers_map as writer_map  # noqa: E402
import test_engine_env_capability_writers_projections as projections  # noqa: E402
import test_engine_env_capability_writers_scanner as scanner  # noqa: E402

#: The four owners by `--only` name, in the order their inventories run.
#: `tools/test_engine_env_capability_audit.py` reads this mapping to
#: learn that a group one of these modules defines is still this
#: owner's, which is what lets its inventory check survive the split
#: without naming a child module.
OWNERS: dict[str, ModuleType] = {
    "map": writer_map,
    "scanner": scanner,
    "projections": projections,
    "conformance": conformance,
}

#: Each owner's inventory size at the split. These are FLOORS, not
#: equalities: a legitimately added writer case joins its owner with no
#: edit here, exactly as the aggregate deliberately pins no historical
#: total. What they refuse is the reverse -- an owner quietly losing
#: cases, which a `TESTS` rebuilt from a truncated source would
#: otherwise report as a green run of whatever survived.
OWNER_FLOORS: dict[str, int] = {
    "map": 7,
    "scanner": 28,
    "projections": 18,
    "conformance": 4,
}


def _inventory(module: ModuleType) -> list:
    """One owner's declared inventory, or `[]` if it declares none."""
    return list(getattr(module, "TESTS", ()))


def _defined_cases(module: ModuleType) -> dict[str, object]:
    """Every module-level `test_*` function the owner itself defines.

    Membership is by definition, not by attribute: a function an owner
    merely imported from a sibling is not its case, so it can neither
    pad this owner's inventory nor be run twice under two owners.
    """
    return {
        name: value for name, value in vars(module).items()
        if name.startswith("test_") and inspect.isfunction(value)
        and value.__module__ == module.__name__
    }


def compose() -> list:
    """The aggregate run sequence: every owner's inventory, in owner
    order, followed by this façade's own composition guard.

    Built defensively -- an owner that has stopped declaring `TESTS`
    contributes nothing here rather than raising at import time, so the
    guard below gets to run and say which owner it was.
    """
    sequence: list = []
    for module in OWNERS.values():
        sequence.extend(_inventory(module))
    return sequence


def test_every_case_is_registered():
    """The composition guard, and the 58th group of the aggregate run.

    The pre-split file's registry-truncation guard asked one question:
    is every `test_*` this module defines in `TESTS`? With the cases
    behind four owners that question has to be asked of the whole
    arrangement, because a shortened green run can now come from any of
    nine directions -- and `TESTS` is BUILT from the owners, so nothing
    upstream of this guard would notice. Every one of them is collected
    here:

      1. a required owner is absent from `OWNERS`;
      2. an owner's inventory is empty, or it declares none at all;
      3. an owner declares fewer groups than it did at the split;
      4. an owner defines a `test_*` function its inventory omits;
      5. an inventory holds something that is not a `test_*` function
         that owner defines;
      6. two owners both declare one group;
      7. a declared group the aggregate never runs;
      8. an aggregate entry no owner declares;
      9. the aggregate runs the owners' groups out of order.

    One assertion, reporting every violation it found, exactly as the
    guard it replaces was one: the aggregate's tally is a published
    figure, and a guard that spent an assertion per condition would
    move it without any contract having changed.

    The aggregate self-test runs THIS list, so a case left out of it
    would be a silently disabled part of the CI-visible gate -- the
    same class tools/test_pack_atlas.py guards its own registry
    against."""
    violations: list[str] = []

    absent = sorted(set(OWNER_FLOORS) - set(OWNERS))
    if absent:
        violations.append(f"owners missing from OWNERS: {absent}")

    declared: dict[str, str] = {}
    ordered: list[str] = []
    for name, module in OWNERS.items():
        tests = _inventory(module)
        if not tests:
            violations.append(
                f"owner {name!r} ({module.__name__}) declares an empty or "
                f"absent TESTS inventory")
        floor = OWNER_FLOORS.get(name, 0)
        if len(tests) < floor:
            violations.append(
                f"owner {name!r} declares {len(tests)} groups, fewer than "
                f"the {floor} it owned at the split")

        defined = _defined_cases(module)
        listed = [getattr(test, "__name__", repr(test)) for test in tests]
        foreign = sorted(entry for test, entry in zip(tests, listed)
                         if defined.get(entry) is not test)
        if foreign:
            violations.append(
                f"owner {name!r} lists inventory entries it does not define "
                f"as module-level test_* functions: {foreign}")
        omitted = sorted(set(defined) - set(listed))
        if omitted:
            violations.append(
                f"owner {name!r} defines test_* functions its TESTS "
                f"inventory omits: {omitted}")

        for entry in listed:
            if entry in declared:
                violations.append(
                    f"test group {entry!r} is declared by both "
                    f"{declared[entry]!r} and {name!r}")
            else:
                declared[entry] = name
                ordered.append(entry)

    expected = [*ordered, test_every_case_is_registered.__name__]
    registered = [case.__name__ for case in TESTS]
    if registered != expected:
        violations.append(
            f"TESTS must run every declared group exactly once, in owner "
            f"order, with this guard last; declared but never run: "
            f"{sorted(set(expected) - set(registered))}, run but undeclared: "
            f"{sorted(set(registered) - set(expected))}; got: {registered}")

    expect(not violations,
           f"the owners must compose into the complete writer-scanner run: "
           f"{'; '.join(violations)}")


#: The writer-scanner contract, in order: the four owners' inventories
#: followed by the composition guard. The aggregate self-test imports
#: this list and runs it after its own cases; `main` below runs it, or
#: one owner's share of it, alone.
TESTS = [*compose(), test_every_case_is_registered]


def _run(tests: list, label: str) -> int:
    for case in tests:
        print(f"{case.__name__}:")
        case()

    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s):")
        for f in FAILURES:
            print(f"  {f}")
        return selftestlib.concluded(1)

    return selftestlib.concluded(
        0, f"\nAll {len(tests)} {label} test groups passed")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="the focused self-test for the §5 writing-module scanner")
    parser.add_argument(
        "--only", choices=tuple(OWNERS),
        help="run one owner's groups alone (default: every group, in order)")
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(sys.argv[1:] if argv is None else argv)
    selftestlib.begin(args.verbose)

    if args.only is None:
        return _run(TESTS, "writer-scanner")
    return _run(_inventory(OWNERS[args.only]), args.only)


if __name__ == "__main__":
    raise SystemExit(main())
