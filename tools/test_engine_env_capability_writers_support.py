#!/usr/bin/env python3
"""Shared support for `tools/test_engine_env_capability_writers.py`'s case
owners (#2228).

This is the ONE source of what two or more of the owner modules --
`test_engine_env_capability_writers_map`,
`test_engine_env_capability_writers_scanner`,
`test_engine_env_capability_writers_projections` and
`test_engine_env_capability_writers_conformance` -- share:

* the assertion facility. `FAILURES` is the ONE list `expect` records
  into, and since #1922 both are `tools/selftestlib.py`'s, re-exported
  here so the owners import them from the single place they already
  import everything else shared from. Four owners each holding a
  private ledger would let the façade exit 0 while a sibling owner had
  recorded a failure; the façade's failure report and exit status read
  this one ledger and nothing else;
* the synthetic field set and permanent-module set every owner scans
  against -- `WRITER_FIELDS` and `WRITER_PERMANENT`;
* the fake capability record `writer_sources` puts in every tree, and
  the two consumer fixtures more than one owner drives: the declared
  writer (the map and conformance owners) and the three-trap module
  (the map and scanner owners);
* the synthetic-tree builder and the two scan adapters -- `writer_sources`,
  `full_scan` and `scan`.

`writer_sources` is generic over the tree it builds: it takes the
caller's own fixture-key-to-path table and returns the fake capability
plus whichever fixtures that call names. The pre-split file carried one
catch-all table of all sixty-five paths, so every owner's fixture
inventory was spelled in one place none of them owned; now each owner
declares the paths it actually uses and `SHARED_PATHS` carries the two
that belong to the fixtures above.

Deliberately NOT here: anything with exactly one consumer. The map's
undeclared and permanent writers, the scanner's thirty-one syntax
fixtures, the projection owner's twenty-eight capability modules and
the conformance owner's three shadowing fixtures each live with the
owner that drives them. A shared module whose contents serve one owner
is a catch-all, not support.

Nothing here runs a test group and this module is not a gate of its
own: `python3 tools/test_engine_env_capability_writers.py` is the
focused command and
`python3 tools/test_engine_env_capability_audit.py` the CI-visible
gate. Every fixture is synthetic text; nothing here writes to the
working tree.
"""
from __future__ import annotations

import sys
from collections.abc import Mapping
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_writers import (  # type: ignore  # noqa: E402
    scan_capability_writes,
)

import selftestlib  # noqa: E402,F401
from selftestlib import FAILURES, expect  # noqa: E402,F401


# ----- SS5 writing-module map (issue #1892, CMA-1) ----------------------
#
# The map pins each `EngineEnv` field's DIRECT writing modules, checked
# in both directions. These fixtures exercise `scan_capability_writes`
# and `audit_writer_modules` against a synthetic three-field production
# tree -- never by editing a real module or the real map -- so each of
# the scan's three honesty gates (import scope, local shadowing, and
# "must head a mutation primitive's first argument") gets a case that
# fails without it.

WRITER_FIELDS = ["fieldOne", "fieldTwo", "fieldThree"]
WRITER_PERMANENT = frozenset({"Permanent.Mod"})

FAKE_CAPABILITY = """\
module Engine.Core.Capability.Fake
  ( FakeCapability(..)
  , toFakeCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data FakeCapability = FakeCapability
  { fkFieldOne ∷ IORef Int
  , fkFieldTwo ∷ IORef Text
  }

toFakeCapability ∷ EngineEnv → FakeCapability
toFakeCapability env = FakeCapability
  { fkFieldOne = fieldOne env
  , fkFieldTwo = fieldTwo env
  }
"""

# A capability write (`fkFieldOne` -> `fieldOne`) and a raw-accessor
# write (`fieldTwo`) from the same module: the two consumer shapes the
# scan must treat as one boundary.
DECLARED_WRITER = """\
module Consumer.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldTwo)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

bumpCapability ∷ EngineEnv → IO ()
bumpCapability env = writeIORef (fkFieldOne (toFakeCapability env)) 1

bumpRaw ∷ EngineEnv → IO ()
bumpRaw env = writeIORef (fieldTwo env) 2
"""

# The three false-positive traps, one per honesty gate.
TRAP_MODULE = """\
module Trap.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldTwo)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

-- | Documentation may name a write it does not perform:
--   writeIORef (fieldTwo env) 7 -- and so may this trailing comment.
{- A block comment: modifyIORef' (fkFieldTwo (toFakeCapability env)) id -}
documented ∷ EngineEnv → IO ()
documented _ = pure ()

-- `fieldTwo` here is this equation's OWN parameter, not the accessor.
shadowed ∷ IORef Int → IO ()
shadowed fieldTwo = writeIORef fieldTwo 3

-- The handle is passed onward, never mutated inline: residue, and the
-- module must not become a declared writer because of it.
handOff ∷ EngineEnv → IO ()
handOff env = someHelper (fkFieldTwo (toFakeCapability env))
"""

#: The one path the fake capability record always occupies.
FAKE_CAPABILITY_PATH = "src/Engine/Core/Capability/Fake.hs"

#: The paths of the two fixtures above that more than one owner drives.
#: An owner's own table (its `_PATHS`) carries only the paths of the
#: fixtures it owns, so no path is spelled twice across the four.
SHARED_PATHS = {
    "declared": "src/Consumer/Mod.hs",
    "trap": "src/Trap/Mod.hs",
}


def writer_sources(paths: Mapping[str, str],
                   modules: Mapping[str, str]) -> dict[str, str]:
    """Synthetic production tree: the fake capability record plus
    whichever consumer fixtures a case asks for, keyed by the relative
    path `module_identifier` maps back to the module name.

    `paths` is the calling owner's own fixture-key-to-path table, read
    ahead of `SHARED_PATHS` so an owner could shadow a shared key if it
    ever needed to; `modules` is the fixture keys that one case names.
    An unknown key is a mistake in the owner's table, not a silently
    absent module, so it raises rather than building a tree missing the
    fixture the case is about.
    """
    table = {**SHARED_PATHS, **paths}
    sources = {FAKE_CAPABILITY_PATH: FAKE_CAPABILITY}
    for key, body in modules.items():
        if key not in table:
            raise KeyError(
                f"fixture key {key!r} has no path; add it to this owner's "
                f"own path table, or to SHARED_PATHS if a second owner "
                f"drives the same fixture")
        sources[table[key]] = body
    return sources


def full_scan(sources: dict[str, str], exemptions=None):
    return scan_capability_writes(
        sources, WRITER_FIELDS, permanent=WRITER_PERMANENT,
        definer="Engine.Core.State", exemptions=exemptions or {})


def scan(sources: dict[str, str]):
    """`(writes, residue)` -- the two halves most cases assert on."""
    result = full_scan(sources)
    return result.writes, result.residue
