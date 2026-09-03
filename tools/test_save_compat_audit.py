#!/usr/bin/env python3
"""Unit tests for the save-compatibility tool (issue #766,
save-overhaul C4; production split across owner modules by issue #2049,
these tests split along those same owners by issue #2073).

Feeds `audit()` synthetic manifests over a temporary directory tree --
never touches the real docs/save_compat/manifest.json or tracked
fixtures -- so these tests stay stable regardless of how the real
manifest grows, and prove the audit actually detects each violation
class it claims to (a "the audit detects an intentionally introduced
violation" gate, mirroring tools/test_persistence_inventory_audit.py's
own convention).

This file is the FAÇADE: it holds no test case of its own. Every member
lives with the owner whose production module it exercises, and this
module's only job is to combine their ordered registries into one run
order and own the command line.

Owner modules (issue #2073)
---------------------------
  test_save_compat_audit_support.py
      Shared fixtures, the real-manifest guard, and the argparse
      stand-in. No test cases; imported by the owners below, never the
      other way round.
  test_save_compat_audit_manifest.py           15 members
      Manifest shape, per-kind field requirements, frozen-DTO
      fingerprint sensitivity.
  test_save_compat_audit_envelope.py           12 members
      Envelope framing, Cabal-inherited extensions, header
      normalization, fingerprint mismatch.
  test_save_compat_audit_register.py           11 members
      `--add-baseline` and `--generate-session` as transactions:
      atomic writes, refusals, `--force`, every rollback path.
  test_save_compat_audit_reproducibility.py     1 member
      The one member that spawns a `cabal repl`, and the source of
      REPRODUCIBILITY_TESTS below.
  test_save_compat_audit_discovery.py          16 members
      `componentCodec` discovery, `csOlderVersions` parsing, component
      source paths, Lua persistence-module discovery.
  test_save_compat_audit_coverage.py           16 members
      Component/version coverage, modern-baseline completeness, B1
      migration policy, orphans, and the real-manifest guards.

Seventy-one members in total. The issue's own table says 69 across
15/12/11/1/14/16 owners; #2098 added
`test_haskell_component_source_paths_is_the_whole_directory` and
`test_dropping_one_owner_from_discovery_changes_the_fingerprint` to the
discovery owner after that table was written, which is the whole of the
difference and is why requirement 11's baseline is the tree as it stands
after #1922 and #2049, not as it stood at filing.

Dependencies run one way (requirement 16): support imports no owner, an
owner imports support, this façade imports the owners, and no owner
module has a command line of its own. `python3 tools/…_coverage.py`
does nothing on purpose -- there is one command, and it is this one.

Selecting what runs (issue #1360)
--------------------------------
Exactly one member of this module,
`test_normalize_fixture_timestamp_makes_generation_reproducible`, spawns
a `cabal repl test:synarchy-test-headless` to build its two envelope
variants. That is ~26 s of a ~58 s module on a warm tree, and it
exercises fixture GENERATION, which only the save format, the fixture
set, or the audit tooling can move. So it -- and only it -- is selected
by changed paths rather than run on every pull request:

  python3 tools/test_save_compat_audit.py
      Everything, the reproducibility member included. The default, so a
      developer running this by hand still gets the whole module.
  python3 tools/test_save_compat_audit.py --without-reproducibility
      Every member EXCEPT the reproducibility one. This is what CI and
      `make ci` run unconditionally.
  python3 tools/test_save_compat_audit.py --only-reproducibility
      Just the reproducibility member. This is what CI and `make ci` run
      when the change touches a save-format, fixture, save-tooling or
      Cabal path -- `tools/ci_expensive_gates.py`'s `save-compat` gate,
      whose pattern table names every such path and whose --self-test
      pins both directions.

The two selective forms partition the module: `REPRODUCIBILITY_TESTS`
below is subtracted from the full list rather than duplicated, so a
member can never be in both or in neither. Nothing is skipped on a push
to master, where CI runs both forms as the post-merge backstop.

This path and both flag spellings are load-bearing beyond those two
invocations: tools/ci_parity_audit.py matches the argv
`["tools/test_save_compat_audit.py"]` and pins both command strings in
its own fixtures, so the façade cannot be renamed or moved and neither
flag can be respelled without editing that audit too.

Usage:
  python3 tools/test_save_compat_audit.py [--without-reproducibility |
                                           --only-reproducibility]
Exit codes: 0 = all tests passed, 1 = one or more failed.

Import and patch ownership (issue #2049)
----------------------------------------
tools/save_compat_audit.py is now a thin façade over seven owner
modules, so each case imports and patches the OWNER of the state or
function it exercises -- `common.MANIFEST_PATH`,
`register._run_real_codec_validation`, `codec.dump_canonical_summary`,
`generate.generate_current_format_session`, and so on. Each test owner
above imports exactly the production owners its own members touch.

It is also load-bearing. Every seam patched is read module-qualified at
CALL time by its owner, so a rebinding is actually seen; a
`from ... import NAME` in an owner would bind the name at import time
and silently ignore the patch. The two failure modes that would cause
are severe and quiet -- rewriting the real tracked
docs/save_compat/manifest.json, and spawning a real `cabal test` plus a
real headless engine inside `--without-reproducibility` -- so each faked
seam additionally asserts that the fake was REACHED, and the
registration and generation cases assert the real manifest's bytes are
unchanged afterwards.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

# The bootstrap runs BEFORE any owner import, because each owner resolves
# its production modules off this same directory (issue #2073). Each
# owner repeats it so it also imports standalone, but the façade cannot
# rely on that ordering accident.
sys.path.insert(0, str(Path(__file__).resolve().parent))
import test_save_compat_audit_coverage as coverage_tests  # noqa: E402
import test_save_compat_audit_discovery as discovery_tests  # noqa: E402
import test_save_compat_audit_envelope as envelope_tests  # noqa: E402
import test_save_compat_audit_manifest as manifest_tests  # noqa: E402
import test_save_compat_audit_register as register_tests  # noqa: E402
import test_save_compat_audit_reproducibility as reproducibility_tests  # noqa: E402

import selftestlib  # noqa: E402
from selftestlib import FAILURES  # noqa: E402


#: The members that spawn a `cabal repl` and are therefore selected by
#: changed paths rather than run on every pull request (issue #1360).
#: Subtracted from ALL_TESTS below rather than listed twice, so the two
#: selective forms provably partition the module. Taken from the
#: reproducibility owner's own registry rather than named again here:
#: requirement 13's "exactly the one reproducibility test" then follows
#: from where the member LIVES, and cannot drift into naming a member
#: the run order no longer contains.
REPRODUCIBILITY_TESTS = list(reproducibility_tests.TESTS)

#: Every member, in run order. `--without-reproducibility` runs this
#: minus REPRODUCIBILITY_TESTS; `--only-reproducibility` runs the
#: intersection; a bare run runs all of it. Plain concatenation of six
#: ordered owner registries reproduces the pre-split order exactly
#: (issue #2073 requirement 12), because each owner's members were
#: already contiguous in it.
ALL_TESTS = [
    *manifest_tests.TESTS,
    *envelope_tests.TESTS,
    *register_tests.TESTS,
    *reproducibility_tests.TESTS,
    *discovery_tests.TESTS,
    *coverage_tests.TESTS,
]


def duplicate_members(tests: list) -> list[str]:
    """Names appearing more than once across the owner registries.

    New with the split (issue #2073). One list could not double-list a
    member without it being visible on the page; six independently
    maintained lists can, and a member listed twice runs twice and is
    reported twice, which quietly inflates the pass count that stands in
    for coverage here. This is a selection invariant like the two guards
    in `main` below -- it constrains the RUNNER, not what any test
    asserts.
    """
    seen: set = set()
    duplicates: list[str] = []
    for fn in tests:
        if fn in seen:
            duplicates.append(fn.__name__)
        else:
            seen.add(fn)
    return duplicates


def selected_tests(only_reproducibility: bool,
                   without_reproducibility: bool) -> list:
    """The members one invocation runs.

    The two flags partition ALL_TESTS: `--only-reproducibility` keeps
    exactly REPRODUCIBILITY_TESTS and `--without-reproducibility` keeps
    exactly the rest, so no member can be run twice or dropped by both.
    """
    expensive = set(REPRODUCIBILITY_TESTS)
    if only_reproducibility:
        return [fn for fn in ALL_TESTS if fn in expensive]
    if without_reproducibility:
        return [fn for fn in ALL_TESTS if fn not in expensive]
    return list(ALL_TESTS)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Unit tests for tools/save_compat_audit.py.",
        epilog="With no flag, every member runs.")
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--only-reproducibility", action="store_true",
        help="run ONLY the cabal-repl reproducibility member (#1360).")
    group.add_argument(
        "--without-reproducibility", action="store_true",
        help="run every member EXCEPT the cabal-repl reproducibility "
             "member (#1360).")
    # This script already owns its command line, and CI drives it through
    # both selective forms, so the shared verbosity flag joins that parser
    # rather than being consumed behind its back; `begin` then
    # starts this invocation's own count (#1922).
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(argv)
    selftestlib.begin(args.verbose)

    # A member listed twice across the six owner registries would run
    # twice and be counted twice (issue #2073).
    duplicates = duplicate_members(ALL_TESTS)
    if duplicates:
        print(f"members listed by more than one owner registry: "
              f"{duplicates}")
        return 1

    # A member listed as expensive but absent from the run order would
    # silently vanish from BOTH selective forms, which is exactly the
    # "coverage quietly stopped running" failure this selection exists
    # to avoid. Fail loudly instead.
    missing = [fn.__name__ for fn in REPRODUCIBILITY_TESTS
               if fn not in ALL_TESTS]
    if missing:
        print(f"REPRODUCIBILITY_TESTS members missing from ALL_TESTS: "
              f"{missing}")
        return 1

    tests = selected_tests(args.only_reproducibility,
                           args.without_reproducibility)
    if not tests:
        print("no tests selected -- refusing to report a vacuous pass")
        return 1
    for fn in tests:
        fn()
    if FAILURES:
        print(f"\n{len(FAILURES)} failure(s)")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, f"\nall tests passed ({len(tests)} of {len(ALL_TESTS)} members)")


if __name__ == "__main__":
    sys.exit(main())
