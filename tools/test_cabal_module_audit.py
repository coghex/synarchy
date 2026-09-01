#!/usr/bin/env python3
"""Unit tests for cabal_module_audit.py (issue #972, CH-28).

Drives the audit's pure functions with synthetic fixtures -- in-memory
cabal text plus synthetic module trees under a temporary directory --
and never by adding a real unlisted `.hs` file under `src/`. A
checked-in unlisted fixture would make the repository-wide audit fail
by construction; this mirrors the convention stated in
tools/test_engine_env_capability_audit.py's docstring ("using synthetic
fixtures, never by editing the real EngineEnv or the real inventory
doc").

The three parser shapes that matter are exercised against BOTH a
synthetic fixture and the real synarchy.cabal, so a future reformat of
the real file that silently breaks parsing fails here rather than
turning the audit into a no-op.

The #1280 conflict cases are pinned the same way: each synthetic tree
is also asserted to pass the bare membership check, so a regression
that quietly drops the duplicate or mismatch check cannot hide behind
the unlisted-module diagnostic.

Usage:
  python3 tools/test_cabal_module_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import cabal_module_audit as cma  # type: ignore

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


# A fixture with every shape the real file uses: a comment line
# adjacent to the field, the first module sharing the field line, a
# comment interleaved with the list, a nested module, an
# `other-modules` list in comma style, termination at the next field
# key, and sibling stanzas whose own `other-modules` must not leak in.
FIXTURE_CABAL = """\
cabal-version: 3.0
name: fixture

common warnings
    ghc-options: -Wall

library
    import: warnings
    -- Expose the modules you want to test
    exposed-modules: UPrelude
                     Engine.Core.Monad
                     -- a comment inside the list
                     World.Generate.Timeline.Erosion

                     World.Weather
    other-modules: Internal.Helper
                 , Internal.Other
    default-extensions: FlexibleInstances
    other-extensions: Strict
                    , StrictData
    hs-source-dirs: src

executable fixture
    main-is: Main.hs
    other-modules: App.Boot
                   App.Cli
    hs-source-dirs: app

test-suite fixture-test
    type: exitcode-stdio-1.0
    other-modules: Test.UPrelude
                   Test.Headless.Harness
    hs-source-dirs: test
"""


def test_parses_every_shape() -> None:
    print("\n[parser] synthetic cabal covering every real-file shape")
    listed = cma.library_modules(FIXTURE_CABAL)
    expect("UPrelude" in listed,
           "the first module SHARES the field line and is captured "
           "(the exact false positive a naive check produces)")
    expect("Engine.Core.Monad" in listed,
           "an ordinary continuation-line module is captured")
    expect("World.Generate.Timeline.Erosion" in listed,
           "a deeply nested module name is captured")
    expect("World.Weather" in listed,
           "a blank line inside the list does not terminate it")
    expect("Internal.Helper" in listed and "Internal.Other" in listed,
           "the library's own comma-style other-modules is captured")
    expect("a" not in listed and "comment" not in listed,
           "a `--` comment line inside the list contributes no modules")


def test_terminates_at_next_field_key() -> None:
    print("\n[parser] the module list ends at the next field key")
    listed = cma.library_modules(FIXTURE_CABAL)
    for leaked in ("FlexibleInstances", "Strict", "StrictData", "src"):
        expect(leaked not in listed,
               f"`{leaked}` from a following field is not a module")


def test_scoped_to_library_stanza() -> None:
    print("\n[scope] sibling stanzas' other-modules do not leak in")
    listed = cma.library_modules(FIXTURE_CABAL)
    for leaked in ("App.Boot", "App.Cli", "Test.UPrelude",
                   "Test.Headless.Harness"):
        expect(leaked not in listed,
               f"`{leaked}` belongs to another stanza and is excluded")
    # The point of the scoping: an executable entry must not be able to
    # satisfy a src/ module of the same name.
    unlisted = cma.audit(FIXTURE_CABAL, [("App.Boot", "src/App/Boot.hs")])
    expect([n for n, _ in unlisted] == ["App.Boot"],
           "a src/ module named only by the executable stanza is "
           "still reported unlisted (not masked)")


def test_absent_other_modules_is_empty_not_an_error() -> None:
    print("\n[parser] an absent other-modules is an empty set")
    only_exposed = """\
library
    exposed-modules: UPrelude
    default-extensions: FlexibleInstances
"""
    listed = cma.library_modules(only_exposed)
    expect(listed == {"UPrelude"},
           "a library with no other-modules field parses to just its "
           "exposed-modules")
    expect(cma.parse_module_field(
        cma.library_stanza_lines(only_exposed), "other-modules") == [],
        "parse_module_field returns [] rather than raising")


def test_audit_reports_unlisted_only() -> None:
    print("\n[audit] unlisted modules are reported, listed ones are not")
    sources = [
        ("UPrelude", "src/UPrelude.hs"),
        ("Engine.Core.Monad", "src/Engine/Core/Monad.hs"),
        ("World.Log", "src/World/Log.hs"),
        ("Engine.Graphics.Vulkan.Types.Core",
         "src/Engine/Graphics/Vulkan/Types/Core.hs"),
    ]
    unlisted = cma.audit(FIXTURE_CABAL, sources)
    expect([n for n, _ in unlisted] == [
        "Engine.Graphics.Vulkan.Types.Core", "World.Log"],
        "exactly the two unlisted modules are reported, sorted")
    expect(all(p.endswith(".hs") for _, p in unlisted),
           "each finding carries its source path")
    expect(cma.audit(FIXTURE_CABAL, sources[:2]) == [],
           "an all-listed source tree produces no findings")


def _write_tree(root: Path, modules: dict[str, str]) -> Path:
    """Materialise synthetic `module X where` files under root/src."""
    source_root = root / "src"
    for name, decl in modules.items():
        path = source_root / (name.replace(".", "/") + ".hs")
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(f"module {decl} where\n", encoding="utf-8")
    return source_root


def test_end_to_end_exit_codes() -> None:
    print("\n[run] exit codes over a synthetic source tree")
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        source_root = _write_tree(root, {
            "UPrelude": "UPrelude",
            "Engine.Core.Monad": "Engine.Core.Monad",
            "World.Generate.Timeline.Erosion":
                "World.Generate.Timeline.Erosion",
            "Internal.Helper": "Internal.Helper",
            "Internal.Other": "Internal.Other",
            "World.Weather": "World.Weather",
        })
        expect(cma.run(FIXTURE_CABAL, source_root) == 0,
               "a fully listed source tree exits 0")
        found = cma.collect_source_modules(source_root)
        expect(("World.Generate.Timeline.Erosion",
                str(source_root / "World/Generate/Timeline/Erosion.hs"))
               in found,
               "a nested module is discovered by recursive glob")

        (source_root / "World").mkdir(parents=True, exist_ok=True)
        (source_root / "World" / "Log.hs").write_text(
            "module World.Log where\n", encoding="utf-8")
        expect(cma.run(FIXTURE_CABAL, source_root) == 1,
               "an unlisted module makes the audit exit non-zero")


def test_declared_name_wins_over_path() -> None:
    print("\n[discovery] the module DECLARATION names the module")
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        source_root = root / "src"
        (source_root / "World").mkdir(parents=True)
        # A file whose declaration disagrees with its path is a module
        # cabal cannot resolve; it must be reported under the name it
        # actually declares, not the one its path implies.
        (source_root / "World" / "Log.hs").write_text(
            "module World.Mislabelled where\n", encoding="utf-8")
        found = cma.collect_source_modules(source_root)
        expect([n for n, _ in found] == ["World.Mislabelled"],
               "the declared name is used, not the path-derived one")
        # No declaration at all: fall back to the path.
        (source_root / "World" / "Log.hs").write_text(
            "-- no module header\n", encoding="utf-8")
        found = cma.collect_source_modules(source_root)
        expect([n for n, _ in found] == ["World.Log"],
               "a file with no module header falls back to its path")


def test_duplicate_declaration_orphan_fails() -> None:
    print("\n[conflict] a second file declaring a LISTED name fails")
    # The exact hole #1280 closes: `src/Orphan.hs` impersonates the
    # listed name `Existing`, whose canonical path is `src/Existing.hs`.
    # Membership alone is satisfied by both files, so the pre-#1280
    # audit printed `1 module / 2 files` and exited 0 while cabal
    # compiled only one of them.
    with tempfile.TemporaryDirectory() as tmp:
        source_root = _write_tree(Path(tmp), {
            "UPrelude": "UPrelude",
            "Orphan": "UPrelude",
        })
        files = cma.collect_source_files(source_root)
        dupes = cma.duplicate_modules(files)
        expect([name for name, _ in dupes] == ["UPrelude"],
               "the repeated name is reported exactly once")
        paths = dupes[0][1] if dupes else []
        expect(len(paths) == 2
               and any(p.endswith("UPrelude.hs") for p in paths)
               and any(p.endswith("Orphan.hs") for p in paths),
               f"BOTH conflicting paths are named (got {paths})")
        expect(cma.audit(FIXTURE_CABAL,
                         [(f.name, f.display) for f in files]) == [],
               "membership alone still passes -- which is precisely why "
               "the duplicate check has to exist")
        expect(cma.run(FIXTURE_CABAL, source_root) == 1,
               "a duplicate declaration against a listed name exits "
               "non-zero")


def test_declaration_path_mismatch_fails() -> None:
    print("\n[conflict] a declaration contradicting its path fails")
    with tempfile.TemporaryDirectory() as tmp:
        # Reuses a LISTED name from a path that does not spell it: the
        # file is unselectable, but its declared identity is inventoried.
        source_root = _write_tree(Path(tmp), {"Orphan": "UPrelude"})
        files = cma.collect_source_files(source_root)
        mismatches = cma.path_mismatches(files)
        expect(len(mismatches) == 1,
               "the mismatched file is reported exactly once")
        path, declared, path_name = mismatches[0]
        expect(path.endswith("Orphan.hs") and declared == "UPrelude"
               and path_name == "Orphan",
               "the diagnostic carries the path, the declared name, and "
               f"the path-derived name (got {mismatches[0]})")
        expect(cma.audit(FIXTURE_CABAL,
                         [(f.name, f.display) for f in files]) == [],
               "the impersonated name passes the membership check on "
               "its own")
        expect(cma.run(FIXTURE_CABAL, source_root) == 1,
               "a declaration/path mismatch reusing a listed name exits "
               "non-zero")


def test_no_declaration_is_not_a_mismatch() -> None:
    print("\n[conflict] a header-less file is not a mismatch")
    with tempfile.TemporaryDirectory() as tmp:
        source_root = Path(tmp) / "src"
        (source_root / "World").mkdir(parents=True)
        (source_root / "World" / "Log.hs").write_text(
            "-- no module header\n", encoding="utf-8")
        files = cma.collect_source_files(source_root)
        expect(cma.path_mismatches(files) == [],
               "a file with no declaration has nothing to contradict "
               "and is left to the ordinary unlisted check")
        expect([f.name for f in files] == ["World.Log"],
               "it is still audited under its path-derived name")


def test_path_identity_is_relative_to_source_root() -> None:
    print("\n[conflict] path identity comes from the audited root")
    # A synthetic tree lives outside REPO_ROOT; deriving the path name
    # from anything but the supplied source_root would make every file
    # here read as a mismatch.
    with tempfile.TemporaryDirectory() as tmp:
        source_root = _write_tree(Path(tmp), {
            "World.Generate.Timeline.Erosion":
                "World.Generate.Timeline.Erosion",
        })
        files = cma.collect_source_files(source_root)
        expect([f.path_name for f in files]
               == ["World.Generate.Timeline.Erosion"],
               "a nested path outside REPO_ROOT derives its real module "
               "name")
        expect(cma.path_mismatches(files) == [],
               "and therefore agrees with its own declaration")


def test_conflicts_do_not_suppress_unlisted_findings() -> None:
    print("\n[run] conflict and unlisted findings both reach the exit")
    with tempfile.TemporaryDirectory() as tmp:
        source_root = _write_tree(Path(tmp), {
            "UPrelude": "UPrelude",
            "Orphan": "UPrelude",       # duplicate + mismatch
            "World.Log": "World.Log",   # plain unlisted
        })
        files = cma.collect_source_files(source_root)
        expect([n for n, _ in cma.audit(
            FIXTURE_CABAL, [(f.name, f.display) for f in files])]
            == ["World.Log"],
            "the ordinary unlisted diagnostic is still produced "
            "alongside the conflicts")
        expect(len(cma.duplicate_modules(files)) == 1
               and len(cma.path_mismatches(files)) == 1,
               "the conflicting file is reported by both conflict checks")
        expect(cma.run(FIXTURE_CABAL, source_root) == 1,
               "the combined tree exits non-zero")


def test_real_cabal_file() -> None:
    print("\n[real] the parser still matches the real synarchy.cabal")
    text = cma.CABAL_PATH.read_text(encoding="utf-8")
    listed = cma.library_modules(text)
    expect(len(listed) > 600,
           f"the real library inventory parses ({len(listed)} modules) "
           "rather than collapsing to a no-op")
    expect("UPrelude" in listed,
           "the real field-line module `UPrelude` is captured")
    expect("World.Weather.Log" in listed,
           "a real continuation-line module is captured")
    for leaked in ("FlexibleInstances", "Strict", "StrictData"):
        expect(leaked not in listed,
               f"the real `default-extensions`/`other-extensions` value "
               f"`{leaked}` is not mistaken for a module")
    for leaked in ("App.Boot", "Test.UPrelude", "Test.Headless.Harness"):
        expect(leaked not in listed,
               f"the real executable/test-suite entry `{leaked}` is "
               "not folded into the library inventory")


def test_real_source_tree_has_no_conflicts() -> None:
    print("\n[real] the real src/ tree trips neither conflict check")
    files = cma.collect_source_files(cma.SOURCE_ROOT)
    dupes = cma.duplicate_modules(files)
    expect(dupes == [],
           f"no module name is claimed by two files (got {dupes[:3]})")
    mismatches = cma.path_mismatches(files)
    expect(mismatches == [],
           "every declaration agrees with its path "
           f"(got {mismatches[:3]})")


def test_runs_from_any_directory() -> None:
    print("\n[cwd] both scripts run from any working directory")
    tools = Path(__file__).resolve().parent
    with tempfile.TemporaryDirectory() as tmp:
        result = subprocess.run(
            [sys.executable, str(tools / "cabal_module_audit.py")],
            cwd=tmp, capture_output=True, text=True)
        expect(result.returncode == 0,
               "tools/cabal_module_audit.py exits 0 on this repository "
               f"when run from an unrelated cwd (got {result.returncode}: "
               f"{result.stdout.strip()[-200:]})")


def main() -> int:
    selftest.parse_verbose()
    test_parses_every_shape()
    test_terminates_at_next_field_key()
    test_scoped_to_library_stanza()
    test_absent_other_modules_is_empty_not_an_error()
    test_audit_reports_unlisted_only()
    test_end_to_end_exit_codes()
    test_declared_name_wins_over_path()
    test_duplicate_declaration_orphan_fails()
    test_declaration_path_mismatch_fails()
    test_no_declaration_is_not_a_mismatch()
    test_path_identity_is_relative_to_source_root()
    test_conflicts_do_not_suppress_unlisted_findings()
    test_real_cabal_file()
    test_real_source_tree_has_no_conflicts()
    test_runs_from_any_directory()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for f in FAILURES:
            print(f"  {f}")
        return selftest.concluded(1)
    return selftest.concluded(0, "\nAll cabal_module_audit tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
