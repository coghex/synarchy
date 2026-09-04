#!/usr/bin/env python3
"""The split's own contract: ownership, selection and loud refusal (#2087).

The other owners test `probe_flake`. This one tests the arrangement that
runs them, because nothing else can: `selftestlib.concluded`'s vacuity
guard overrides a run to failure only when the WHOLE interpreter executed
no assertion, so an owner silently dropped from the registry, a migration
contract that no longer matches `probe_flake.PROTOCOL_PROBES`, or an
`--only` selector that matched nothing would every one of them still
print a passing summary.

Three groups:

  composition   the facade's own checks fire, and fire by name, in both
                drift directions -- a registered protocol probe with no
                contract, and a contract for a probe the registry does
                not name -- reading the REAL registry rather than a
                stand-in a harness fixture rebound;
  selection     `--only` really selects, exhaustively: the harness
                selection and the twenty-two migration selections
                partition the aggregate exactly, and a fresh process
                running one of them executes that owner and no other;
  refusal       an unrecognized selector exits non-zero naming the
                offending token and listing the valid ones.

The selection group deliberately proves the ROSTERS in process, through
`compose`, and the EXECUTED SET in a subprocess, through one focused
migration run's own stdout. Running `--only harness` as a subprocess from
inside this group would re-enter this group, so the harness side's roster
proof is `--list`, which runs nothing; the executed-set proof comes from
the migration side, where no such re-entry exists. The subprocess cases
assert on the script's STDOUT rather than on its exit status, because a
facade that lost its `if __name__ == "__main__"` entry point exits 0
having printed nothing.
"""
from __future__ import annotations

import importlib
import subprocess
import sys
import tempfile
from pathlib import Path

from .support import probe_flake, REPO_ROOT, expect

SCRIPT = "tools/test_probe_flake.py"


def facade():
    """The aggregate module, however this interpreter entered it.

    Run as a script it is `__main__`, and importing `test_probe_flake` by
    name would load a second copy whose `PACKAGE_DIR` and registries are
    a different set of objects from the ones actually driving this run --
    exactly the duplication that would make a composition check inspect
    something other than what ran.
    """
    main = sys.modules.get("__main__")
    if getattr(main, "PACKAGE", None) == "probe_flake_tests":
        return main
    return importlib.import_module("test_probe_flake")


def _run_script(*args: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        [sys.executable, SCRIPT, *args],
        cwd=REPO_ROOT, text=True, capture_output=True, timeout=300)


def test_owner_composition() -> None:
    print("\n-- owner composition --")
    aggregate = facade()

    registered = set(probe_flake.PROTOCOL_PROBES)
    expect(registered == aggregate.protocol_probes().keys(),
           "the composition check reads probe_flake's own registry")

    on_disk = {path.stem[len(aggregate.MIGRATION_PREFIX):]
               for path in aggregate.PACKAGE_DIR.glob(
                   f"{aggregate.MIGRATION_PREFIX}*.py")}
    expect(on_disk == registered,
           f"a migration contract exists for exactly the probes "
           f"probe_flake.PROTOCOL_PROBES registers "
           f"(only in the registry: {sorted(registered - on_disk)}; "
           f"only on disk: {sorted(on_disk - registered)})")

    # The aggregate composes, and its selections partition it.
    everything = [test.__name__ for test in aggregate.compose()]
    expect(len(everything) == len(set(everything)),
           "the aggregate runs each test group exactly once")
    harness = [test.__name__ for test in aggregate.compose("harness")]
    migrations: list[str] = []
    for key in sorted(registered):
        migrations.extend(test.__name__
                          for test in aggregate.compose(f"migration:{key}"))
    expect(sorted(harness + migrations) == sorted(everything),
           "the harness selection and the twenty-two migration selections "
           "partition the aggregate exactly")

    # Both drift directions, by name, against the real registry. The dict
    # object is restored to the SAME object, because the facade's seam
    # guard compares identity.
    saved = probe_flake.PROTOCOL_PROBES
    try:
        probe_flake.PROTOCOL_PROBES = dict(saved, no_such_probe="probe-result/v1")
        try:
            aggregate.compose()
            expect(False, "a registered probe with no migration contract is "
                          "refused")
        except aggregate.CompositionError as error:
            expect("no_such_probe" in str(error),
                   f"a registered probe with no migration contract is refused "
                   f"by name (said {error})")

        dropped = "thermo_altitude"
        probe_flake.PROTOCOL_PROBES = {k: v for k, v in saved.items()
                                       if k != dropped}
        try:
            aggregate.compose()
            expect(False, "a migration contract for an unregistered probe is "
                          "refused")
        except aggregate.CompositionError as error:
            expect(f"{aggregate.MIGRATION_PREFIX}{dropped}.py" in str(error),
                   f"a migration contract for an unregistered probe is refused "
                   f"by its module name (said {error})")
    finally:
        probe_flake.PROTOCOL_PROBES = saved

    # A harness owner dropped from the roster, and a stray module in the
    # package, are refused too -- both checked against the directory, so
    # neither can be satisfied by editing the registry alone.
    saved_dir = aggregate.PACKAGE_DIR
    try:
        with tempfile.TemporaryDirectory(prefix="probe-flake-roster-") as tmp:
            fake = Path(tmp)
            for name in aggregate.HARNESS_OWNERS[:-1]:
                (fake / f"{aggregate.HARNESS_PREFIX}{name}.py").touch()
            for key in registered:
                (fake / f"{aggregate.MIGRATION_PREFIX}{key}.py").touch()
            aggregate.PACKAGE_DIR = fake
            missing_owner = aggregate.HARNESS_OWNERS[-1]
            try:
                aggregate.compose()
                expect(False, "a harness owner whose module is missing is "
                              "refused")
            except aggregate.CompositionError as error:
                expect(missing_owner in str(error),
                       f"a harness owner whose module is missing is refused by "
                       f"name (said {error})")

            (fake / f"{aggregate.HARNESS_PREFIX}{missing_owner}.py").touch()
            (fake / "stowaway.py").touch()
            try:
                aggregate.compose()
                expect(False, "an unregistered module in the package is "
                              "refused")
            except aggregate.CompositionError as error:
                expect("stowaway" in str(error),
                       f"an unregistered module in the package is refused by "
                       f"name (said {error})")
    finally:
        aggregate.PACKAGE_DIR = saved_dir

    expect(aggregate.PACKAGE_DIR == saved_dir
           and probe_flake.PROTOCOL_PROBES is saved,
           "the composition group leaves the registry and the package "
           "directory exactly as it found them")


def _listed_groups(*args: str) -> list[str]:
    done = _run_script("--list", *args)
    groups = [line.strip() for line in done.stdout.splitlines()
              if line.startswith("  ")]
    return groups


def test_focused_selection() -> None:
    print("\n-- focused selection --")
    aggregate = facade()
    registered = sorted(probe_flake.PROTOCOL_PROBES)

    # The entry point is reachable and the script prints: a facade that
    # lost `if __name__ == "__main__"` exits 0 with an empty stdout, which
    # an exit-status assertion cannot see.
    listing = _run_script("--list")
    expect(listing.stdout.strip() != "",
           "the script's entry point is reachable and --list writes to stdout")
    everything = [line.strip() for line in listing.stdout.splitlines()
                  if line.startswith("  ")]
    expect(everything == [test.__name__ for test in aggregate.compose()],
           "a fresh process's aggregate selection is the composed run order")

    harness = _listed_groups("--only", "harness")
    expect(harness and set(harness) < set(everything),
           f"--only harness selects a proper subset of the aggregate "
           f"({len(harness)} of {len(everything)} groups)")
    expect(harness == [test.__name__ for test in aggregate.compose("harness")],
           "--only harness selects the aggregate's harness groups, in the "
           "aggregate's order")

    seen: list[str] = []
    for key in registered:
        selected = _listed_groups("--only", f"migration:{key}")
        expect(selected == [test.__name__
                            for test in aggregate.compose(f"migration:{key}")],
               f"--only migration:{key} selects exactly that probe's contract "
               f"(got {selected})")
        expect(selected and not set(selected) & set(harness),
               f"--only migration:{key} selects no harness group")
        seen.extend(selected)
    expect(sorted(harness + seen) == sorted(everything),
           "every aggregate group is selected by exactly one focused "
           "invocation, and no focused invocation selects anything else")

    # Selection is not just a listing: a fresh process really runs one
    # owner and no other. collapse_crawl is the cheapest contract, and its
    # banner names it.
    done = _run_script("--only", "migration:collapse_crawl")
    expect("-- collapse_crawl probe migration --" in done.stdout,
           f"a focused migration run executes that probe's contract "
           f"(stdout: {done.stdout[-400:]!r})")
    expect("probe_flake self-test: all migration:collapse_crawl cases pass"
           in done.stdout,
           "a focused run reports its own scope in its terminal status line")
    expect(done.returncode == 0,
           f"a focused migration run exits 0 (got {done.returncode})")
    absent = [banner for banner in
              ("-- descriptor --", "-- event stream --", "-- ports --",
               "-- census manifest", "-- run_one's extended interface --",
               "-- thermo_altitude probe migration --",
               "-- owner composition --")
              if banner in done.stdout]
    expect(not absent,
           f"a focused migration run executes no other owner's groups "
           f"(found {absent})")


def test_selector_rejection() -> None:
    print("\n-- selector refusal --")
    valid = facade().selectors()

    for token in ("migrations", "migration:", "migration:no_such_probe",
                  "harness:ports", "HARNESS", ""):
        done = _run_script("--only", token)
        combined = done.stdout + done.stderr
        expect(done.returncode != 0,
               f"--only {token!r} exits non-zero (got {done.returncode})")
        # The exact sentence, not a substring that a quoted listing would
        # satisfy on its own: `''` and `harness` both appear in the valid
        # -selector list, so a looser check would pass on a message that
        # never named the token at all.
        expect(f"unrecognized --only selector {token!r}" in combined,
               f"--only {token!r} names the offending token")
        missing = [selector for selector in valid if selector not in combined]
        expect(not missing,
               f"--only {token!r} lists every valid selector "
               f"(omitted {missing})")
        expect("cases pass" not in combined,
               f"--only {token!r} never reports a passing run")
        expect("-- descriptor --" not in combined
               and "probe migration --" not in combined,
               f"--only {token!r} runs no test group before refusing")

    expect(valid[0] == "harness" and len(valid) == 1 + len(
               probe_flake.PROTOCOL_PROBES),
           f"the valid-selector list is the harness plus one per registered "
           f"protocol probe (got {len(valid)})")


TESTS = (
    test_owner_composition,
    test_focused_selection,
    test_selector_rejection,
)
