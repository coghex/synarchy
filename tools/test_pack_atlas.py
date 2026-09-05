#!/usr/bin/env python3
"""
test_pack_atlas.py — fixture self-test for tools/pack_atlas.py and the
`tools/pack_atlas_<owner>.py` modules behind it (#1257 inventory
validation, #1258 atlas compilation, #2054 production owner split,
#2061 self-test owner split).

Every case builds a complete, isolated unit tree in a temporary
directory (`data/units/` + `assets/textures/units/`) and runs the real
tool against it via `--root`. Nothing here reads, writes, or depends on
the shipped asset tree, so the suite keeps passing while the real
corpus grows — and no case can leave a production atlas behind.

Three registries:

  POSITIVE   a validation fixture that must exit 0.
  NEGATIVE   a validation fixture that must exit non-zero AND print a
             diagnostic naming the actual problem, so a check cannot
             pass by failing for some unrelated reason. Where a case
             tightens a rule, a positive case pins the other direction,
             so over-rejection fails too.
  SCENARIO   a #1258 compiler case or a #1262 budget case, which needs
             more than an exit code: it inspects the emitted atlas
             pixels and index document, or observes which files a
             second run actually wrote.

Validation opens every declared frame (#1311), so the content cases
corrupt real PNG bytes at exact offsets — a truncated stream, a
garbled payload under a correct checksum, a correct payload under a
wrong checksum, a non-image, a valid image of another format — and pair
each with a positive that would fail on over-rejection. Two scenarios
pin why BOTH decode passes exist by showing a file that each one alone
accepts. Compilation necessarily decodes too, so its scenarios assert
pixels.

This file is the suite's only command. Since #2061 the cases themselves
live in three owners it assembles in the order `CASE_OWNERS` names,
over the fixture machinery in `test_pack_atlas_support`:

  test_pack_atlas_validation   12 positive + 72 negative (#1257, #1311)
  test_pack_atlas_compiler     34 scenarios (#1258, #2076)
  test_pack_atlas_budget       14 scenarios (#1262)

None of them parses arguments or runs a case at import.

    python3 tools/test_pack_atlas.py           # run every case
    python3 tools/test_pack_atlas.py -v        # print each case's output
"""
from __future__ import annotations

import argparse
import shutil
import sys
import tempfile
import traceback
from pathlib import Path
from types import ModuleType
from typing import Dict, List, NamedTuple, Optional, Tuple

sys.path.insert(0, str(Path(__file__).resolve().parent))

import test_pack_atlas_budget  # noqa: E402
import test_pack_atlas_compiler  # noqa: E402
import test_pack_atlas_validation  # noqa: E402
from test_pack_atlas_support import Case, Fixture, OwnerCases  # noqa: E402


# --------------------------------------------------------------------
# Assembly
# --------------------------------------------------------------------
#
# The owner sequence is written down here rather than discovered, so
# execution order is this list and not module import order: positive
# cases, then negative cases, then scenarios compiler-before-budget,
# exactly as one file ran them before #2061.

CASE_OWNERS: Tuple[ModuleType, ...] = (
    test_pack_atlas_validation,
    test_pack_atlas_compiler,
    test_pack_atlas_budget,
)


class Floor(NamedTuple):
    """The case count an owner is REQUIRED to contribute."""

    positive: int
    negative: int
    scenario: int


# The suite is only meaningful if it actually built fixtures, so a
# refactor that emptied a collection — or dropped an owner out of
# CASE_OWNERS — must not read as green.
#
# These are per-owner floors, not one suite-wide floor. A single
# combined floor cannot see an emptied owner: before #2061 the check
# was `len(SCENARIO) < 33` against 48 real scenarios, so deleting the
# whole 14-case budget family would still have exited 0. Each owner is
# now checked against its own collection, and this table's key set is
# checked against the assembled owners' names, so an owner dropped from
# CASE_OWNERS fails too.
OWNER_FLOORS: Dict[str, Floor] = {
    "validation": Floor(positive=12, negative=72, scenario=0),
    "compiler": Floor(positive=0, negative=0, scenario=34),
    "budget": Floor(positive=0, negative=0, scenario=14),
}

# What the assembled suite must add up to, checked separately from the
# per-owner floors so that a case lost from every owner fails even if
# the owners themselves are all present and non-empty.
TOTAL_FLOOR = Floor(positive=12, negative=72, scenario=48)


def assemble() -> List[OwnerCases]:
    """Each owner's frozen collections, in CASE_OWNERS order."""
    return [module.CASES for module in CASE_OWNERS]


def registry_failures(owners: List[OwnerCases]) -> List[str]:
    """Every way the assembled suite falls short of its contract."""
    failures: List[str] = []

    assembled = [owner.owner for owner in owners]
    if sorted(assembled) != sorted(OWNER_FLOORS):
        failures.append(
            f"case owners look truncated: assembled {assembled}, "
            f"expected {sorted(OWNER_FLOORS)}")

    for owner in owners:
        floor = OWNER_FLOORS.get(owner.owner)
        if floor is None:
            continue
        counts = Floor(*owner.counts())
        if (counts.positive < floor.positive
                or counts.negative < floor.negative
                or counts.scenario < floor.scenario):
            failures.append(
                f"the {owner.owner} owner looks truncated: "
                f"{counts.positive} positive, {counts.negative} negative, "
                f"{counts.scenario} scenario (expected at least "
                f"{floor.positive}, {floor.negative}, {floor.scenario})")

    total = Floor(sum(len(owner.positive) for owner in owners),
                  sum(len(owner.negative) for owner in owners),
                  sum(len(owner.scenario) for owner in owners))
    if (total.positive < TOTAL_FLOOR.positive
            or total.negative < TOTAL_FLOOR.negative
            or total.scenario < TOTAL_FLOOR.scenario):
        failures.append(
            f"case registries look truncated: {total.positive} positive, "
            f"{total.negative} negative, {total.scenario} scenario")

    return failures


# --------------------------------------------------------------------
# Runner
# --------------------------------------------------------------------

def run_case(build: Case, unit: Optional[str] = None) -> tuple[int, str]:
    """Run one fixture, reporting a validator CRASH as that case's own
    failure rather than letting it abort the suite.

    A traceback escaping here would kill the run before any case was
    reported, so a checker that raises on malformed input would look
    like a suite-wide breakage instead of one failing case — and a
    negative case whose rule was mutated into a crash would silently
    produce no `FAIL:` line at all.
    """
    parent = tempfile.mkdtemp(prefix="pack_atlas_test_")
    try:
        fixture = Fixture(Path(parent) / "repo")
        build(fixture)
        return fixture.run(unit)
    except Exception:  # noqa: BLE001 - a crash IS the finding here
        return 70, ("the validator raised instead of reporting:\n"
                    + traceback.format_exc())
    finally:
        shutil.rmtree(parent, ignore_errors=True)


def run_scenario(build: Case) -> Optional[str]:
    """Run one compiler scenario; return its failure text, or ``None``.

    A scenario drives the tool itself and asserts on what it produced,
    so its verdict is an exception rather than an exit code.
    """
    parent = tempfile.mkdtemp(prefix="pack_atlas_scenario_")
    try:
        build(Fixture(Path(parent) / "repo"))
        return None
    except AssertionError as error:
        return str(error) or traceback.format_exc()
    except Exception:  # noqa: BLE001 - a crash IS the finding here
        return "the compiler raised instead of reporting:\n" + \
            traceback.format_exc()
    finally:
        shutil.rmtree(parent, ignore_errors=True)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    owners = assemble()
    positive = [case for owner in owners for case in owner.positive]
    negative = [case for owner in owners for case in owner.negative]
    scenario = [case for owner in owners for case in owner.scenario]

    failures: List[str] = []
    total = 0

    for name, build, unit in positive:
        total += 1
        code, output = run_case(build, unit)
        if args.verbose:
            print(f"--- positive: {name}\n{output}")
        if code != 0:
            failures.append(
                f"positive '{name}': expected exit 0, got {code}\n{output}")

    for name, build, expect, unit in negative:
        total += 1
        code, output = run_case(build, unit)
        if args.verbose:
            print(f"--- negative: {name}\n{output}")
        if code == 0:
            failures.append(
                f"negative '{name}': expected a nonzero exit, got 0\n{output}")
        elif expect not in output:
            failures.append(
                f"negative '{name}': exited {code} but no diagnostic matched "
                f"{expect!r}\n{output}")

    for name, build in scenario:
        total += 1
        failure = run_scenario(build)
        if args.verbose:
            print(f"--- scenario: {name}\n{failure or 'ok'}")
        if failure is not None:
            failures.append(f"scenario '{name}': {failure}")

    failures.extend(registry_failures(owners))

    if failures:
        for failure in failures:
            print(f"FAIL: {failure}", file=sys.stderr)
        print(f"\ntest_pack_atlas: {len(failures)} of {total} case(s) failed",
              file=sys.stderr)
        return 1

    print(f"test_pack_atlas: all {total} case(s) pass "
          f"({len(positive)} positive, {len(negative)} negative, "
          f"{len(scenario)} scenario)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
