#!/usr/bin/env python3
"""The aggregate self-test for engine_env_capability_audit.py -- the one
command CI and tools/ci-local.sh run for the whole gate.

Issue #876's acceptance still governs every case: the audit detects an
intentionally introduced capability-inventory gap using synthetic
fixtures, never by editing the real EngineEnv, the real inventory doc,
a Haskell source or a Cabal file. Mirrors
tools/test_persistence_inventory_audit.py's approach of feeding the
audit's pure functions synthetic record text and synthetic documents,
so the cases stay stable regardless of how EngineEnv or the real
inventory grow.

Composition (#2062)
-------------------
This module is composition, dispatch and reporting only -- it holds no
test body. The test groups live with six owners, each of which keeps
its own ordered inventory in `TESTS`:

  `test_engine_env_capability_audit_inventory`     the SS5 inventory
      rows: missing/duplicate/stale rows, capability headings, the
      lifecycle and thread-role grammar, reader/writer decisions, the
      sync/lifecycle/notes/grounding cells, section-5 bounds, and the
      real-repository inventory assertion (issue #876);
  `test_engine_env_capability_audit_boundary`      the SS6 full-access
      ratchet (import classification, SS6.2 parsing, both-direction
      stale/undocumented checks) and the SS6.1 permanent-boundary
      parse/compare, plus the epic's real-repository end-state case
      (issues #889, #899);
  `test_engine_env_capability_audit_save_load`     the E8 SaveLoad
      projection: total correspondence, transposed/missing/extra
      bindings, Cabal registration, a missing module, and Haddock
      stripping (issue #899);
  `test_engine_env_capability_audit_render_input`  the SS3 main-render
      and SS7.3 LuaThread input structural boundaries: worker-safe
      views, full-record importer restrictions, private-field
      ownership, missing views, stale allowlist entries, comment
      handling, and both real-repository assertions (issues #891, #892);
  `test_engine_env_capability_audit_field_total`   the SS1 audited
      field-total prose: marked-block existence and uniqueness, the
      live count and first/last field span, section and procedure
      anchors, duplicate or stray totals, Markdown fences, section
      bounds, and the two real-inventory assertions (issue #1669);
  `test_engine_env_capability_writers`             the SS5
      writing-module scanner, the focused owner issue #2036 gave it
      (issues #1892, #2059). It is the one owner with a command line
      of its own, runnable alone for iteration; the aggregate composes
      its `TESTS` and never calls its `main`.

`test_engine_env_capability_audit_support` is the single source of what
two or more of the first five share: the #1922 assertion facility, the
synthetic EngineEnv record and inventory-document builders, the two
real-repository readers, and the persistence-inventory audit's
`extract_record_fields`. Dependencies run one way -- support imports no
owner, owners import support and the production audit modules they
exercise, and only this façade imports the owners.

`compose()` builds the run sequence this gate has always used: the
owner inventories concatenated in the order above, with one seam. The
boundary owner exposes its inventory in two fragments (`TESTS_LEADING`,
`TESTS_TRAILING`) so `test_real_repo_end_state` keeps its historical
position -- after every other family's groups and before the writer
scanner's -- because it asks whether the whole epic landed once the
per-family checks have each passed.

Before any group runs, the composition is checked so no arrangement of
owners can report a shortened green run, and without pinning a
historical total (a legitimately added group joins with no edit here).
Every owner's inventory must be non-empty; every module-level `test_*`
function an owner defines must be in its inventory, and every inventory
entry must be a `test_*` function that owner defines -- which catches
both an emptied inventory and a single case silently dropped from one;
the sequence must then run every declared group exactly once, with no
group two owners both declare. `selftestlib.concluded` is the last
guard behind those: a run that executed no assertion at all is a
failure whatever this module believed.

One failure ledger. Every owner's `expect` is `selftestlib`'s, so every
assertion in every owner records into the one `FAILURES` list this
module reports from and derives its exit status from. A failing
assertion in any owner therefore still fails this command and is named
in its output, exactly as before the split.

Usage:
  python3 tools/test_engine_env_capability_audit.py        every group, in order
  python3 tools/test_engine_env_capability_audit.py -v     narrate passing assertions too
The bare form is the gate CI and `tools/ci-local.sh` invoke. Neither
this file nor any owner module is registered separately on either
side: `tools/ci_parity_audit.py` would fail a one-sided invocation, and
a second gate invocation is what issue #2036 ruled out.

Exit codes:
  0 = all tests passed
  1 = one or more tests failed, or the composition refused to run
"""
from __future__ import annotations

import inspect
import sys
from pathlib import Path
from types import ModuleType

sys.path.insert(0, str(Path(__file__).resolve().parent))
import selftestlib  # noqa: E402
from selftestlib import FAILURES  # noqa: E402
import test_engine_env_capability_audit_boundary as boundary  # noqa: E402
import test_engine_env_capability_audit_field_total as field_total  # noqa: E402
import test_engine_env_capability_audit_inventory as inventory  # noqa: E402
import test_engine_env_capability_audit_render_input as render_input  # noqa: E402
import test_engine_env_capability_audit_save_load as save_load  # noqa: E402
import test_engine_env_capability_writers as writers  # noqa: E402

#: The six owners by name, in the order their inventories run.
OWNERS: dict[str, ModuleType] = {
    "inventory": inventory,
    "boundary": boundary,
    "save_load": save_load,
    "render_input": render_input,
    "field_total": field_total,
    "writers": writers,
}


class CompositionError(Exception):
    """The owner inventories no longer compose into the complete run."""


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


def inventories() -> dict[str, list]:
    """Every owner's declared inventory, complete against its own source.

    An owner that has stopped declaring `TESTS`, one whose inventory is
    empty, one that defines a `test_*` function its inventory omits, and
    one whose inventory names something it does not define are all
    refused here -- before the run sequence is consulted -- so an owner
    losing part of its inventory is reported as itself, by name, rather
    than as whatever the sequence notices second.
    """
    found: dict[str, list] = {}
    for name, module in OWNERS.items():
        tests = getattr(module, "TESTS", None)
        if tests is None:
            raise CompositionError(
                f"owner {name!r} ({module.__name__}) declares no TESTS "
                f"inventory")
        tests = list(tests)
        if not tests:
            raise CompositionError(
                f"owner {name!r} ({module.__name__}) declares an empty "
                f"TESTS inventory -- refusing to report a shortened run")
        defined = _defined_cases(module)
        listed = [getattr(test, "__name__", repr(test)) for test in tests]
        foreign = sorted(
            entry for test, entry in zip(tests, listed)
            if defined.get(entry) is not test)
        if foreign:
            raise CompositionError(
                f"owner {name!r} ({module.__name__}) lists inventory "
                f"entries it does not define as module-level test_* "
                f"functions: {foreign}")
        omitted = sorted(set(defined) - set(listed))
        if omitted:
            raise CompositionError(
                f"owner {name!r} ({module.__name__}) defines test_* "
                f"functions its TESTS inventory omits: {omitted}")
        found[name] = tests
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
        *inventory.TESTS,
        *boundary.TESTS_LEADING,
        *save_load.TESTS,
        *render_input.TESTS,
        *field_total.TESTS,
        *boundary.TESTS_TRAILING,
        *writers.TESTS,
    ]

    declared: dict[str, str] = {}
    for name, tests in by_owner.items():
        for test in tests:
            if test.__name__ in declared:
                earlier = declared[test.__name__]
                raise CompositionError(
                    f"test group {test.__name__!r} is declared twice by "
                    f"{name!r}" if earlier == name else
                    f"test group {test.__name__!r} is declared by both "
                    f"{earlier!r} and {name!r}")
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
        print(f"test_engine_env_capability_audit composition error: {error}")
        return 1

    for t in tests:
        print(f"{t.__name__}:")
        t()

    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s):")
        for f in FAILURES:
            print(f"  {f}")
        return selftestlib.concluded(1)

    return selftestlib.concluded(0, f"\nAll {len(tests)} test groups passed")


if __name__ == "__main__":
    raise SystemExit(main())
