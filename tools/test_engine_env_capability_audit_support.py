#!/usr/bin/env python3
"""Shared support for `tools/test_engine_env_capability_audit.py`'s case
owners (#2062).

This is the ONE source of what two or more of the owner modules --
`test_engine_env_capability_audit_inventory`,
`test_engine_env_capability_audit_boundary`,
`test_engine_env_capability_audit_save_load`,
`test_engine_env_capability_audit_render_input` and
`test_engine_env_capability_audit_field_total` -- share:

* the assertion facility. `FAILURES` is the ONE list `expect` records
  into, and since #1922 both are `tools/selftestlib.py`'s, re-exported
  here so the owners import them from the single place they already
  import everything else shared from. Five owners each holding a
  private ledger would let the aggregate exit 0 while a sibling owner
  had recorded a failure; the aggregate's failure report and exit
  status read this one ledger and nothing else;
* the synthetic EngineEnv record and inventory-document builders --
  `SYNTHETIC_ENGINE_ENV`, the three fixture rows, `INVENTORY_HEADER`,
  `inventory_doc` and the complete document `SYNTHETIC_INVENTORY_COMPLETE`
  it builds. The inventory owner drives every row/heading/grammar
  rejection over them, and the field-total owner grows the same record
  by one field to prove the row audit passes a document whose SS1 total
  went stale (issue #1669's recurrence);
* the real-repository inputs -- `real_engine_env_source` and
  `real_inventory_text` read the live `src/Engine/Core/State.hs` and
  `docs/engineenv_capability_inventory.md` for every owner's
  real-repository case, so the two paths are spelled once; and
  `extract_record_fields`, the persistence-inventory audit's record
  parser this gate borrows for the live-field derivation
  (`persistence_inventory_audit_haskell`, the cross-audit import the
  pre-split aggregate carried), re-exported so no owner repeats it.

Deliberately NOT here: anything with exactly one consumer. The SS6.2
and SS6.1 fixture tables live with the boundary owner, the SaveLoad
projection source with the save-load owner, the synthetic render and
input trees with the render/input owner, and the SS1/SS6.2 prose
builder with the field-total owner. A shared module whose contents
serve one owner is a catch-all, not support.

Nothing here runs a test group and this module is not a gate of its
own: `python3 tools/test_engine_env_capability_audit.py` remains the
only invocation, in CI and in `make ci` alike. Every fixture is
synthetic text; nothing here writes to the working tree.
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore  # noqa: E402
    ENGINE_ENV_FILE, INVENTORY_PATH, REPO_ROOT,
)
from persistence_inventory_audit_haskell import extract_record_fields  # type: ignore  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

__all__ = [
    "FAILURES",
    "FIELD_ONE_ROW",
    "FIELD_THREE_ROW",
    "FIELD_TWO_ROW",
    "INVENTORY_HEADER",
    "SYNTHETIC_ENGINE_ENV",
    "SYNTHETIC_INVENTORY_COMPLETE",
    "expect",
    "extract_record_fields",
    "inventory_doc",
    "real_engine_env_source",
    "real_inventory_text",
]


# ----- Real-repository inputs -------------------------------------------

def real_engine_env_source() -> str:
    """The live `EngineEnv` declaration's source text."""
    return (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8")


def real_inventory_text() -> str:
    """The live capability inventory document."""
    return INVENTORY_PATH.read_text(encoding="utf-8")


# ----- Synthetic fixtures -----------------------------------------------

SYNTHETIC_ENGINE_ENV = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne   ∷ IORef Int
    -- ^ a documented field, with a stray brace in prose: {not real}
  , fieldTwo   ∷ IORef Text
  , fieldThree ∷ Q.Queue Int
  } deriving (Eq)

data SomethingElse = SomethingElse { unrelated ∷ Int }
"""

# A complete, valid inventory: two capability groups, one single-writer
# field, one genuinely multi-reader/multi-writer field, and one
# immutable (justified no-writers) field -- proving requirement 9's
# "valid multi-reader/multi-writer and immutable-field classifications
# pass" alongside the failure-case fixtures below.
INVENTORY_HEADER = "| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |\n|---|---|---|---|---|---|---|---|\n"

FIELD_ONE_ROW = (
    "| `fieldOne` | boot-process | `MainRender` (`src/Fake/Reader.hs:10`) "
    "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` | `src/Fake/Init.hs:5` "
    "| None | — |\n")
FIELD_TWO_ROW = (
    "| `fieldTwo` | session-replaced "
    "| `WorldThread` (`src/Fake/World.hs:1`), `LuaThread` (`src/Fake/Lua.hs:2`) "
    "| `WorldThread` (`src/Fake/World.hs:9`), `LuaThread` (`src/Fake/Lua.hs:20`) "
    "| `IORef Text`, multi-writer | `src/Fake/Init.hs:6` | None | — |\n")
FIELD_THREE_ROW = (
    "| `fieldThree` | boot-process | `AnyThread` (`src/Fake/AnyReader.hs:1`) "
    "| None (immutable boot configuration, never mutated after "
    "`src/Fake/Init.hs:7`) | `Q.Queue Int`, read-only after boot "
    "| `src/Fake/Init.hs:7` | None | — |\n")


def inventory_doc(*, core_init_heading="### core-init",
                  core_init_rows=FIELD_ONE_ROW,
                  render_heading="### render-gpu-asset",
                  render_rows=FIELD_TWO_ROW + FIELD_THREE_ROW,
                  preamble="") -> str:
    return (
        "# Fake capability inventory\n\n"
        "## 5. Field inventory\n\n"
        + preamble
        + f"{core_init_heading}\n\n{INVENTORY_HEADER}{core_init_rows}\n"
        f"{render_heading}\n\n{INVENTORY_HEADER}{render_rows}\n"
        "## 6. Something else entirely\n\n"
        "not part of section 5 at all\n"
    )


SYNTHETIC_INVENTORY_COMPLETE = inventory_doc()
