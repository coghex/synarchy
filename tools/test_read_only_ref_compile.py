#!/usr/bin/env python3
"""Mechanically demonstrate `Engine.Core.ReadOnlyRef`'s structural
boundary (issue #1896, CMA-2 of the capability mutation-authority epic
#1890) by COMPILING fixtures and checking what the compiler says.

`ReadOnlyRef` denies mutation by construction: its newtype constructor
is not exported, so a holder can only `readReadOnlyRef` it. That claim
is only worth what a compiler will confirm, and a commented-out snippet
in a source file confirms nothing -- so this script writes real fixture
modules, invokes a real GHC on them against the real built library, and
prints the exact command and the complete diagnostic for every one.

__What the fixtures may import.__ Every fixture below imports only
PUBLIC interfaces -- `Engine.Core.ReadOnlyRef`,
`Engine.Core.Capability.ContentRegistriesView`,
`Building.Knowledge.Live` and ordinary types. None reaches for an
`.Internal` companion, because there deliberately is none (see
`Engine.Core.ReadOnlyRef`'s header); N3 is what fails if one is ever
added and the constructor leaks through it.

__Positive controls are load-bearing, not decoration.__ A negative
fixture "failing to compile" proves nothing on its own: a missing
package, a stale build or a typo'd import fails exactly the same way.
Each negative is therefore paired with a positive that differs from it
ONLY in the operation attempted, and a positive that does not compile
is reported as a broken check rather than a pass.

Fixtures:

  P1 read a selected `ContentRegistriesViewCapability` field   MUST compile
  N1 write that same field                                     MUST fail
  P2 read `Building.Knowledge.Live.ContainerObserver.coItems`  MUST compile
  N2 write that same field, after the handle has been passed
     into that production context record                       MUST fail
  N3 unwrap via `ReadOnlyRef`'s constructor                    MUST fail

N1 is requirement's fixture (1) -- a direct write on the view -- and N2
is fixture (2), the pass-on: the boundary survived being packed into a
context record that mixes three capabilities. N3 pins the abstraction
itself: the constructor is not exported, so the wrapper cannot be
unwrapped back into a writable `IORef`.

A negative must fail for the RIGHT reason. It is accepted only when the
diagnostic names both `ReadOnlyRef` and `IORef` (N1/N2), or refuses the
constructor as unexported (N3) -- never merely by exiting non-zero.

This is a MANUAL gate, deliberately: it needs a built library and a GHC
invocation per fixture, and the property it pins is enforced on every
ordinary build anyway (the tree does not compile if a reader writes).
It is neither in `.github/workflows/ci.yml` nor in `tools/ci-local.sh`,
so `tools/ci_parity_audit.py` has nothing to reconcile. Run it when
touching `ReadOnlyRef`, the view capability, or `ContainerObserver`.

Usage:
  python3 tools/test_read_only_ref_compile.py
Exit codes: 0 = every positive compiled and every negative failed for
the expected reason, 1 = otherwise.
"""
from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

# `cabal exec` puts the locally built `synarchy` library in GHC's package
# environment; `-package synarchy` then exposes it (it is hidden by
# default, being a library rather than a dependency of the shell).
# `-fno-code` keeps this to type checking -- the only phase that can
# produce the diagnostics below -- and the explicit output directories
# keep GHC's artifacts inside the temp tree.
GHC_FLAGS = ["-fno-code", "-package", "synarchy", "-fdiagnostics-color=never"]

# One GHC invocation can take a while the first time `cabal exec`
# resolves the environment; the compilations themselves are seconds.
TIMEOUT_SECONDS = 900


class Fixture:
    def __init__(self, name: str, must_compile: bool, source: str,
                 expect: tuple[tuple[str, ...], ...] = (),
                 why: str = "") -> None:
        self.name = name
        self.must_compile = must_compile
        self.source = source
        #: What the diagnostic must SAY for a MUST-fail fixture to count
        #: -- so a fixture that fails for an unrelated reason (missing
        #: package, stale build, typo) is reported as broken rather than
        #: silently accepted. One entry per required claim; each entry
        #: is a tuple of accepted spellings, because GHC rewords its
        #: messages between releases and this check is about the
        #: BOUNDARY, not about one compiler's phrasing.
        self.expect = expect
        self.why = why


# --------------------------------------------------------------------------
# The fixtures. Each is a complete module; nothing here imports an
# internal module, a constructor, or anything the public interface does
# not export (N3 deliberately TRIES to, which is the point of it).
# --------------------------------------------------------------------------

P1 = Fixture(
    "P1_read_view_field", True,
    """module P1_read_view_field where
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.ContentRegistriesView
  (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Item.Types (ItemManager)

-- The legitimate operation: read the registry the view carries.
readItems :: EngineEnv -> IO ItemManager
readItems env =
  readReadOnlyRef (crvItemManagerRef (toContentRegistriesViewCapability env))
""",
    why="the control for N1: identical imports and field, read instead of "
        "written")

N1 = Fixture(
    "N1_write_view_field", False,
    """module N1_write_view_field where
import Data.IORef (writeIORef)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.ContentRegistriesView
  (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Item.Types (ItemManager)

-- Requirement's fixture (1): a direct mutation of a selected
-- ContentRegistriesViewCapability field.
clobberItems :: EngineEnv -> ItemManager -> IO ()
clobberItems env replacement =
  writeIORef (crvItemManagerRef (toContentRegistriesViewCapability env))
             replacement
""",
    expect=(("ReadOnlyRef",), ("IORef",)),
    why="a non-writer cannot mutate a selected registry through the view")

P2 = Fixture(
    "P2_read_observer_field", True,
    """module P2_read_observer_field where
import Building.Knowledge.Live (ContainerObserver(..))
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Item.Types (ItemManager)

-- The legitimate operation on the production pass-on: read the handle
-- back out of the context record it was packed into.
readObservedItems :: ContainerObserver -> IO ItemManager
readObservedItems co = readReadOnlyRef (coItems co)
""",
    why="the control for N2: identical imports and field, read instead of "
        "written")

N2 = Fixture(
    "N2_write_observer_field", False,
    """module N2_write_observer_field where
import Data.IORef (writeIORef)
import Building.Knowledge.Live (ContainerObserver(..))
import Item.Types (ItemManager)

-- Requirement's fixture (2): the pass-on. The handle has already been
-- packed into ContainerObserver -- a real production context record
-- mixing three capabilities -- and the restriction travelled with it.
clobberObservedItems :: ContainerObserver -> ItemManager -> IO ()
clobberObservedItems co replacement = writeIORef (coItems co) replacement
""",
    expect=(("ReadOnlyRef",), ("IORef",)),
    why="the boundary survives being stored in a production context record")

N3 = Fixture(
    "N3_unwrap_constructor", False,
    """module N3_unwrap_constructor where
import Data.IORef (IORef)
import Engine.Core.ReadOnlyRef (ReadOnlyRef(..))

-- The abstraction itself. `ReadOnlyRef(..)` is deliberately written the
-- greediest way an importer can ask -- and it is legal, because it
-- requests every constructor the module exports, which is none. What
-- does not typecheck is USING one, so this fixture pattern-matches on
-- it: that is the only shape that distinguishes "no constructor is
-- exported" from "the import list happened not to mention it".
unwrap :: ReadOnlyRef a -> IORef a
unwrap (ReadOnlyRef ref) = ref
""",
    # GHC 9.12 reports the unexported newtype constructor as an
    # "Illegal term-level use of the type constructor"; older releases
    # said "Data constructor not in scope". Either spelling is the same
    # refusal.
    expect=(("ReadOnlyRef",), ("term-level use", "not in scope")),
    why="the newtype constructor is not exported, so the wrapper cannot be "
        "unwrapped back into a writable IORef")

FIXTURES = (P1, N1, P2, N2, N3)


def compile_fixture(fixture: Fixture, workdir: Path
                    ) -> tuple[list[str], int, str]:
    """Write `fixture` into `workdir` and type-check it. Returns the exact
    command, the exit status, and the complete combined diagnostic."""
    source_path = workdir / f"{fixture.name}.hs"
    source_path.write_text(fixture.source, encoding="utf-8")
    outdir = workdir / "out"
    outdir.mkdir(exist_ok=True)
    command = (["cabal", "exec", "--", "ghc"] + GHC_FLAGS
               + ["-hidir", str(outdir), "-odir", str(outdir),
                  str(source_path)])
    proc = subprocess.run(command, cwd=REPO_ROOT, capture_output=True,
                          text=True, timeout=TIMEOUT_SECONDS)
    return command, proc.returncode, (proc.stdout or "") + (proc.stderr or "")


def main() -> int:
    print("Engine.Core.ReadOnlyRef structural-boundary compile check "
          "(issue #1896)")
    print(f"Repository: {REPO_ROOT}")
    print("Building the library first -- the fixtures type-check against "
          "it, so a stale or absent build would make every negative fail "
          "for the wrong reason.")
    try:
        build = subprocess.run(["cabal", "build", "lib:synarchy"],
                               cwd=REPO_ROOT, capture_output=True, text=True,
                               timeout=TIMEOUT_SECONDS)
    except FileNotFoundError:
        print("FAIL: 'cabal' was not found on PATH.")
        return 1
    if build.returncode != 0:
        print("FAIL: `cabal build lib:synarchy` failed; fix the build first.")
        print((build.stdout or "") + (build.stderr or ""))
        return 1

    failures: list[str] = []
    with tempfile.TemporaryDirectory(prefix="read_only_ref_compile_") as tmp:
        workdir = Path(tmp)
        for fixture in FIXTURES:
            kind = "MUST COMPILE" if fixture.must_compile else "MUST FAIL"
            print()
            print("=" * 74)
            print(f"{fixture.name} -- {kind}")
            print(f"  {fixture.why}")
            print("-" * 74)
            print(fixture.source.rstrip())
            print("-" * 74)
            try:
                command, status, diagnostic = compile_fixture(fixture, workdir)
            except subprocess.TimeoutExpired:
                print(f"FAIL: compilation timed out after {TIMEOUT_SECONDS}s")
                failures.append(f"{fixture.name}: timed out")
                continue
            print("command: " + " ".join(command))
            print(f"exit status: {status}")
            print("compiler diagnostic (complete):")
            print(diagnostic if diagnostic.strip() else "  (no output)")

            if fixture.must_compile:
                if status == 0:
                    print(f"OK: {fixture.name} compiled, as required.")
                else:
                    print(f"FAIL: {fixture.name} MUST compile but did not. "
                          "The check environment is broken -- every negative "
                          "below would fail for that reason rather than for "
                          "the boundary.")
                    failures.append(
                        f"{fixture.name}: positive control did not compile")
                continue

            if status == 0:
                print(f"FAIL: {fixture.name} compiled. The read-only "
                      "boundary is GONE -- this mutation is now accepted.")
                failures.append(f"{fixture.name}: compiled but must not")
                continue
            missing = [alternatives for alternatives in fixture.expect
                       if not any(token in diagnostic
                                  for token in alternatives)]
            if missing:
                unseen = ["/".join(repr(token) for token in alternatives)
                          for alternatives in missing]
                print(f"FAIL: {fixture.name} failed, but not for the "
                      "expected reason -- the diagnostic never mentions "
                      + ", ".join(unseen) + ".")
                failures.append(
                    f"{fixture.name}: unexpected diagnostic "
                    f"(missing {', '.join(unseen)})")
                continue
            print(f"OK: {fixture.name} was rejected, naming "
                  + " and ".join("/".join(repr(token)
                                          for token in alternatives)
                                 for alternatives in fixture.expect)
                  + ".")

    print()
    print("=" * 74)
    if failures:
        print(f"{len(failures)} failure(s):")
        for failure in failures:
            print(f"  - {failure}")
        return 1
    positives = sum(1 for f in FIXTURES if f.must_compile)
    print(f"All {len(FIXTURES)} fixtures behaved as required "
          f"({positives} compiled, {len(FIXTURES) - positives} were "
          "rejected for the expected reason).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
