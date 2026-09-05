#!/usr/bin/env python3
"""The probe-result protocol and flakiness-harness gate (#1425, split #2087).

`python3 tools/test_probe_flake.py` is the unconditional gate CI and
`tools/ci-local.sh` invoke, and its no-argument behaviour is what it has
always been: every group, in the order this suite has always run them,
the same assertions, the same skip and failure accounting, and the same
final `probe_flake self-test: all cases pass`.

What this module holds is only that -- selection, ordered aggregation and
reporting. It carries no test body and names no test group. The groups
live with owners under `tools/probe_flake_tests/`, divided along the two
responsibilities this file used to carry at once:

  `harness_*`     the generic contracts, which change when the harness
                  changes: descriptor and event-stream validation,
                  eligibility and reconciliation, ports, leases and
                  concurrency accounting, artifacts, result serialization
                  and rendering, census integration, `run_one`'s
                  interface, and this package's own composition;
  `migration_*`   one module per key in `probe_flake.PROTOCOL_PROBES`,
                  each owning that probe's standalone/protocol
                  compatibility contract and nothing else.

So a probe-result migration adds `migration_<key>.py` and one line to
`SEQUENCE` here, and changing one migrated probe's contract touches
neither the harness modules nor another probe's module -- which is the
whole of what #2087 asked for.

Two focused invocations are served from the same checked inventories the
aggregate composes, so a focused run is the aggregate's groups for that
owner, in the aggregate's order, never a separately maintained list:

  --only harness               every generic harness contract
  --only migration:<probe>     one migrated probe's contract, for any
                               key in `probe_flake.PROTOCOL_PROBES`

An unrecognized `--only` is refused before anything runs, naming the
token and listing every valid selector. A selector that silently matched
nothing and then printed the success banner would be a vacuous pass, and
this toolchain fails loudly instead.

Before any group runs, `compose()` checks the arrangement, because
`selftestlib.concluded`'s vacuity guard cannot: it overrides to failure
only when the whole interpreter run executed NO assertion, so an owner
dropped from the registry would still leave the others' assertions
counted and the aggregate would report success having silently skipped a
probe's entire contract. So the owner roster is cross-checked against the
modules actually on disk in both directions; the migration roster is
cross-checked against the REAL `probe_flake.PROTOCOL_PROBES` in both
directions, so a registered protocol probe with no contract and a
contract for an unregistered probe each fail by name; every owner must
declare at least the groups it carried at the split; each owner's
fragments must concatenate to exactly its own `TESTS`; and the sequence
must then run every declared group exactly once.

The synthetic-only boundary is unchanged and belongs to every owner: no
registered behavior probe is executed, no engine is booted, no world is
generated and no network is used. The harness owners drive the shipped
`probe_flake`/`probe_protocol`/`probe_census` code against a synthetic
probe in a throwaway tree, redirecting and restoring
`probe_engine.REPO_ROOT`, `probe_runner_registry.PROBES`,
`ci_probes.CI_ELIGIBLE`, `probe_flake.PROTOCOL_PROBES` and
`probe_flake.LEASE_ROOT`; the migration owners subprocess the real
`tools/<probe>_probe.py --describe` and monkeypatch each probe's own
boot/console seams, so they must observe those globals UNPATCHED. That
independence is not left to ordering: `run()` snapshots the five seams
before the first group and checks them after the last, on every entry
point, so a focused run never depends on a sibling having run and a
dropped restore fails the gate rather than leaking into whatever runs
next.

Usage:
  python3 tools/test_probe_flake.py
  python3 tools/test_probe_flake.py -v
  python3 tools/test_probe_flake.py --only harness
  python3 tools/test_probe_flake.py --only migration:thermo_altitude
  python3 tools/test_probe_flake.py --list
Exit codes:
  0 = all tests passed
  1 = one or more failed, or the composition refused to run
  2 = an unrecognized argument, including an unknown --only selector
"""
from __future__ import annotations

import argparse
import importlib
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import probe_flake  # type: ignore  # noqa: E402
import selftestlib  # noqa: E402
from selftestlib import FAILURES  # noqa: E402
from probe_flake_tests import support  # noqa: E402

PACKAGE = "probe_flake_tests"
PACKAGE_DIR = Path(__file__).resolve().parent / PACKAGE

#: The nine generic-harness owners, by the module suffix that names them.
#: `--only harness` runs all of them; no finer selector is offered,
#: because the boundary #2087 asked for is harness-versus-migration and a
#: second vocabulary would be one more thing to keep true.
HARNESS_OWNERS = (
    "descriptor", "events", "eligibility", "reconciliation", "ports",
    "artifacts", "results", "census", "runner", "composition",
)

#: `migration_<key>.py` for every key in `probe_flake.PROTOCOL_PROBES`.
#: Derived from the directory rather than listed, so this cannot drift
#: from the modules on disk; `compose` is what checks it against the real
#: registry, in both directions.
MIGRATION_PREFIX = "migration_"
HARNESS_PREFIX = "harness_"

#: Everything in the package that is NOT a case owner. Deriving the owner
#: roster from the directory is what makes the coverage check work in both
#: directions: a tenth harness module cannot be added without either
#: joining `HARNESS_OWNERS` or failing here, and an owner dropped from the
#: registry while its module remains is caught too.
NON_OWNER_MODULES = {"__init__", "support"}

#: The group count each owner carried at this split. A FLOOR, not an exact
#: count, so a legitimately added group joins without an edit here; an
#: owner declaring FEWER is a truncation and is refused. Floored PER OWNER
#: so one owner cannot be emptied while another grows by the same amount.
MINIMUM_GROUPS = {
    "harness:descriptor": 1,
    "harness:events": 3,
    "harness:eligibility": 2,
    "harness:reconciliation": 2,
    "harness:ports": 6,
    "harness:artifacts": 2,
    "harness:results": 3,
    "harness:census": 2,
    "harness:runner": 1,
    "harness:composition": 3,
    "migration:circadian_species": 2,
}
#: Every other migration owner carries exactly one contract.
DEFAULT_MIGRATION_MINIMUM = 1

#: The aggregate's order, as which owner fragment runs when. It is the
#: pre-split order of `main()`: the twenty-one harness groups, then the
#: ten batch-migrated contracts in the order the batch drove them, then
#: circadian_species's own protocol contract and the eleven remaining
#: standalone contracts, then `run_one`'s interface. Naming a fragment is
#: not naming a test: every group still lives with exactly one owner, and
#: `compose` checks that each owner's fragments reconstruct its `TESTS`.
SEQUENCE = (
    ("harness", "descriptor", "TESTS"),
    ("harness", "events", "TESTS"),
    ("harness", "eligibility", "TESTS"),
    ("harness", "reconciliation", "TESTS"),
    ("harness", "ports", "TESTS"),
    ("harness", "artifacts", "TESTS"),
    ("harness", "results", "TESTS"),
    ("harness", "census", "TESTS"),
    ("migration", "blood_decal", "TESTS"),
    ("migration", "circadian_species", "TESTS_BATCH"),
    ("migration", "collapse_crawl", "TESTS"),
    ("migration", "config_state", "TESTS"),
    ("migration", "injury_log", "TESTS"),
    ("migration", "lua_orphan_prune", "TESTS"),
    ("migration", "machine_shop", "TESTS"),
    ("migration", "mental_efficiency", "TESTS"),
    ("migration", "thought", "TESTS"),
    ("migration", "wire", "TESTS"),
    ("migration", "circadian_species", "TESTS_PROTOCOL"),
    ("migration", "blood_impact", "TESTS"),
    ("migration", "meal_waste", "TESTS"),
    ("migration", "role", "TESTS"),
    ("migration", "circadian", "TESTS"),
    ("migration", "concussion_revive", "TESTS"),
    ("migration", "disarm", "TESTS"),
    ("migration", "remote_warning_page_guard", "TESTS"),
    ("migration", "state_of_mind", "TESTS"),
    ("migration", "position_hold", "TESTS"),
    ("migration", "text_encoding", "TESTS"),
    ("migration", "thermo_altitude", "TESTS"),
    ("harness", "runner", "TESTS"),
    ("harness", "composition", "TESTS"),
)

#: The five process-global seams the harness owners redirect. `run`
#: snapshots them before the first group and checks them after the last,
#: so a dropped restore fails every entry point instead of leaking into a
#: migration owner -- which reads the real registry -- or into whatever
#: runs next in the same interpreter.
SEAMS = (
    ("probe_engine", "REPO_ROOT"),
    ("probe_runner_registry", "PROBES"),
    ("ci_probes", "CI_ELIGIBLE"),
    ("probe_flake", "PROTOCOL_PROBES"),
    ("probe_flake", "LEASE_ROOT"),
)


class CompositionError(Exception):
    """The owner inventories no longer compose into the complete run."""


def protocol_probes() -> dict[str, str]:
    """The REAL `probe_flake.PROTOCOL_PROBES`, never a rebound stand-in.

    Read through the module rather than captured at import, so this
    reflects the shipped dict at the moment it is asked for; `run`
    checks the composition before any group runs and the seam guard
    proves nothing left a stand-in behind afterwards.
    """
    return dict(probe_flake.PROTOCOL_PROBES)


def _modules_on_disk(prefix: str) -> set[str]:
    return {path.stem[len(prefix):] for path in PACKAGE_DIR.glob(f"{prefix}*.py")
            if path.stem not in NON_OWNER_MODULES}


def _all_modules_on_disk() -> set[str]:
    return {path.stem for path in PACKAGE_DIR.glob("*.py")
            if path.stem not in NON_OWNER_MODULES}


def owner_module(kind: str, name: str):
    """Import one owner, by the spelling that loaded this facade.

    Relative to `PACKAGE` rather than by bare name, so the owners and the
    fixtures they share are one set of module objects under every import
    spelling of this file.
    """
    prefix = HARNESS_PREFIX if kind == "harness" else MIGRATION_PREFIX
    return importlib.import_module(f"{PACKAGE}.{prefix}{name}")


def selectors() -> list[str]:
    """Every valid `--only` value, in the order the help lists them."""
    return ["harness"] + [f"migration:{key}"
                          for key in sorted(protocol_probes())]


def inventories() -> dict[str, list]:
    """Every owner's declared inventory, each at least its historical size.

    Checked before the run sequence is consulted, so an owner that lost
    its `TESTS`, was emptied, or shrank is reported as itself, by name.
    The rosters are checked first, because an owner omitted from the
    registry declares nothing to be short -- and that omission is exactly
    the silent shortening `selftestlib.concluded` cannot see.
    """
    registered = protocol_probes()

    harness_on_disk = _modules_on_disk(HARNESS_PREFIX)
    if harness_on_disk != set(HARNESS_OWNERS):
        raise CompositionError(
            f"the harness roster disagrees with {PACKAGE}/: modules present "
            f"but not registered {sorted(harness_on_disk - set(HARNESS_OWNERS))}, "
            f"owners registered whose module is missing "
            f"{sorted(set(HARNESS_OWNERS) - harness_on_disk)}")

    migration_on_disk = _modules_on_disk(MIGRATION_PREFIX)
    if migration_on_disk != set(registered):
        missing = sorted(set(registered) - migration_on_disk)
        extra = sorted(migration_on_disk - set(registered))
        detail = []
        if missing:
            detail.append(
                f"probe_flake.PROTOCOL_PROBES registers {missing} with no "
                f"{PACKAGE}/{MIGRATION_PREFIX}<key>.py migration contract")
        if extra:
            detail.append(
                f"{[f'{MIGRATION_PREFIX}{k}.py' for k in extra]} declare a "
                f"migration contract for a probe probe_flake.PROTOCOL_PROBES "
                f"does not register")
        raise CompositionError(
            "the migration contracts and probe_flake.PROTOCOL_PROBES "
            "disagree -- " + "; ".join(detail))

    stray = _all_modules_on_disk() - {
        f"{HARNESS_PREFIX}{name}" for name in HARNESS_OWNERS} - {
        f"{MIGRATION_PREFIX}{key}" for key in registered}
    if stray:
        raise CompositionError(
            f"{sorted(stray)} in {PACKAGE}/ is neither a registered owner nor "
            f"named in NON_OWNER_MODULES")

    found: dict[str, list] = {}
    owners = ([("harness", name) for name in HARNESS_OWNERS]
              + [("migration", key) for key in sorted(registered)])
    for kind, name in owners:
        label = f"{kind}:{name}"
        module = owner_module(kind, name)
        tests = getattr(module, "TESTS", None)
        if tests is None:
            raise CompositionError(
                f"owner {label!r} ({module.__name__}) declares no TESTS "
                f"inventory")
        floor = MINIMUM_GROUPS.get(
            label, DEFAULT_MIGRATION_MINIMUM if kind == "migration" else 1)
        if len(tests) < floor:
            raise CompositionError(
                f"owner {label!r} ({module.__name__}) declares {len(tests)} "
                f"test group(s), fewer than the {floor} it has always carried "
                f"-- refusing to report a shortened run")
        found[label] = list(tests)
    return found


def compose(only: str | None = None) -> list:
    """The run sequence, checked against every owner's own inventory.

    Both directions are checked, because either drift is a silent loss of
    coverage: a sequence entry no owner declares, a declared group the
    sequence never runs, a group run twice, and a group two owners both
    declare all fail here. An owner's fragments must also concatenate to
    exactly its `TESTS`, which is what stops a fragment from being dropped
    out of the order while its groups still look accounted for.

    `only` selects one owner's groups instead, after the same checks --
    `"harness"` for every harness owner in the aggregate's order, or
    `"migration:<key>"` for one migrated probe.
    """
    by_owner = inventories()

    sequence: list = []
    harness_sequence: list = []
    fragments_seen: dict[str, list] = {label: [] for label in by_owner}
    for kind, name, attribute in SEQUENCE:
        label = f"{kind}:{name}"
        if label not in by_owner:
            raise CompositionError(
                f"the run order names owner {label!r}, which is not registered")
        fragment = getattr(owner_module(kind, name), attribute, None)
        if fragment is None:
            raise CompositionError(
                f"owner {label!r} declares no {attribute!r} fragment, which "
                f"the run order names")
        if not fragment:
            raise CompositionError(
                f"owner {label!r}'s {attribute!r} fragment is empty")
        sequence.extend(fragment)
        fragments_seen[label].extend(fragment)
        if kind == "harness":
            harness_sequence.extend(fragment)

    for label, seen in fragments_seen.items():
        if seen == by_owner[label]:
            continue
        ordered = [test.__name__ for test in seen]
        owned = [test.__name__ for test in by_owner[label]]
        detail = []
        if [n for n in owned if n not in ordered]:
            detail.append(f"groups the order never runs: "
                          f"{[n for n in owned if n not in ordered]}")
        if [n for n in ordered if n not in owned]:
            detail.append(f"groups the order runs that the owner does not "
                          f"declare: {[n for n in ordered if n not in owned]}")
        if not detail:
            detail.append(f"the same groups in a different order -- the order "
                          f"runs {ordered}, the owner declares {owned}")
        raise CompositionError(
            f"owner {label!r}'s fragments in the run order do not reconstruct "
            f"its TESTS -- " + "; ".join(detail))

    declared: dict[str, str] = {}
    for label, tests in by_owner.items():
        within: set[str] = set()
        for test in tests:
            if test.__name__ in within:
                raise CompositionError(
                    f"owner {label!r} declares test group {test.__name__!r} "
                    f"more than once")
            within.add(test.__name__)
            if test.__name__ in declared:
                raise CompositionError(
                    f"test group {test.__name__!r} is declared by both "
                    f"{declared[test.__name__]!r} and {label!r}")
            declared[test.__name__] = label

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

    if only is None:
        return sequence
    if only == "harness":
        return harness_sequence
    return by_owner[only]


def _snapshot_seams() -> dict[tuple[str, str], object]:
    modules = {name: sys.modules[name] for name, _ in SEAMS}
    return {(module, attribute): getattr(modules[module], attribute)
            for module, attribute in SEAMS}


def run(tests: list) -> None:
    """Run one selection with the redirectable seams guarded either side.

    The snapshot is taken before the first group and compared after the
    last, whatever the outcome, so a harness owner that stopped restoring
    one of the five globals fails here rather than leaving a synthetic
    registry behind for a migration owner -- or for the next thing to run
    in this interpreter. It is checked on every entry point, because a
    focused run must not depend on a sibling owner having run.
    """
    before = _snapshot_seams()
    try:
        for test in tests:
            test()
    finally:
        after = _snapshot_seams()
        leaked = sorted(f"{module}.{attribute}"
                        for (module, attribute), value in before.items()
                        if after[(module, attribute)] is not value)
        selftestlib.expect(
            not leaked,
            f"every redirected module global is restored when the run ends "
            f"(leaked {leaked})" if leaked else
            "every redirected module global is restored when the run ends")


def main(argv: list[str] | None = None) -> int:
    valid = selectors()
    parser = argparse.ArgumentParser(
        description="Self-test for the probe-result protocol, the flakiness "
                    "harness, and each migrated probe's compatibility "
                    "contract.",
        epilog="--only harness runs the generic harness contracts; "
               "--only migration:<probe> runs one migrated probe's contract, "
               "for any key in probe_flake.PROTOCOL_PROBES.")
    parser.add_argument(
        "--only", metavar="SELECTOR",
        help="run one owner's test groups instead of the whole aggregate: "
             "'harness', or 'migration:<probe-key>'")
    parser.add_argument(
        "--list", action="store_true", dest="list_groups",
        help="print the selected test groups, in run order, without "
             "running them")
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(argv)

    if args.only is not None and args.only not in valid:
        # A selector that matched nothing and then printed the success
        # banner would be a vacuous pass, so this is an error before
        # anything runs rather than an empty run.
        parser.error(
            f"unrecognized --only selector {args.only!r}; valid selectors "
            f"are: {', '.join(valid)}")

    selftestlib.begin(args.verbose)

    try:
        tests = compose(args.only)
    except CompositionError as error:
        print(f"test_probe_flake composition error: {error}", file=sys.stderr)
        return 1

    if args.list_groups:
        scope = args.only or "aggregate"
        print(f"{len(tests)} test group(s) selected by {scope}:")
        for test in tests:
            print(f"  {test.__name__}")
        return 0

    run(tests)

    print()
    if support.SKIPS:
        print(f"{len(support.SKIPS)} skipped:")
        for message in support.SKIPS:
            print(f"  - {message}")
    if FAILURES:
        print(f"\n{len(FAILURES)} FAILED:")
        for message in FAILURES:
            print(f"  - {message}")
        return selftestlib.concluded(1)
    if args.only:
        return selftestlib.concluded(
            0, f"probe_flake self-test: all {args.only} cases pass")
    return selftestlib.concluded(0, "probe_flake self-test: all cases pass")


if __name__ == "__main__":
    sys.exit(main())
