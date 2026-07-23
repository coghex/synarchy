#!/usr/bin/env python3
"""Unit tests for engine_env_capability_audit.py (issue #876 acceptance:
the audit detects an intentionally introduced capability-inventory gap
using synthetic fixtures, never by editing the real EngineEnv or the
real inventory doc).

Mirrors tools/test_persistence_inventory_audit.py's own approach: feed
the audit's pure functions synthetic Haskell record text and a
synthetic inventory doc, so these tests stay stable regardless of how
EngineEnv or the real inventory doc grow.

Usage:
  python3 tools/test_engine_env_capability_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_audit import (  # type: ignore
    audit, parse_inventory, ENGINE_ENV_FILE, ENGINE_ENV_PATTERN,
)
from persistence_inventory_audit import extract_record_fields  # type: ignore

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


# ----- Fixtures ---------------------------------------------------------

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
_HEADER = "| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |\n|---|---|---|---|---|---|---|---|\n"

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


def _doc(*, core_init_heading="### core-init", core_init_rows=FIELD_ONE_ROW,
          render_heading="### render-gpu-asset",
          render_rows=FIELD_TWO_ROW + FIELD_THREE_ROW,
          preamble="") -> str:
    return (
        "# Fake capability inventory\n\n"
        "## 5. Field inventory\n\n"
        + preamble
        + f"{core_init_heading}\n\n{_HEADER}{core_init_rows}\n"
        f"{render_heading}\n\n{_HEADER}{render_rows}\n"
        "## 6. Something else entirely\n\n"
        "not part of section 5 at all\n"
    )


SYNTHETIC_INVENTORY_COMPLETE = _doc()


def test_complete_inventory_has_no_violations():
    violations = audit(SYNTHETIC_ENGINE_ENV, SYNTHETIC_INVENTORY_COMPLETE)
    expect(violations == [],
           f"a fully valid inventory (single-writer + genuinely "
           f"multi-reader/multi-writer + justified-immutable fields) "
           f"should have zero violations, got: {violations}")


def test_missing_row_detected():
    doc = _doc(render_rows=FIELD_TWO_ROW)  # fieldThree's row dropped
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldThree" in v and "no row" in v for v in violations),
           "dropping fieldThree's row entirely must be flagged as missing")


def test_duplicate_row_detected():
    doc = _doc(render_rows=FIELD_TWO_ROW + FIELD_THREE_ROW + FIELD_ONE_ROW)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "more than one inventory row" in v
               for v in violations),
           "fieldOne appearing under two different capability headings "
           "must be flagged as a duplicate row")


def test_stale_row_detected():
    fake_row = (
        "| `fieldFour` | boot-process | `MainRender` (`src/Fake/Reader.hs:1`) "
        "| `Boot` (`src/Fake/Init.hs:1`) | `IORef Int` | `src/Fake/Init.hs:1` "
        "| None | — |\n")
    doc = _doc(core_init_rows=FIELD_ONE_ROW + fake_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldFour" in v and "no longer exists" in v for v in violations),
           "a row for a field absent from the live EngineEnv declaration "
           "must be flagged as stale")


def test_unknown_capability_heading_detected():
    doc = _doc(core_init_heading="### misc")
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "not one of" in v for v in violations),
           "a generic bucket heading ('misc') must not satisfy the "
           "capability-owner requirement")


def test_row_with_no_enclosing_heading_detected():
    # A table row appears in section 5 before ANY '### <capability>'
    # heading has been seen.
    preamble = f"{_HEADER}{FIELD_ONE_ROW}\n"
    doc = _doc(preamble=preamble, core_init_rows="", render_rows=FIELD_TWO_ROW + FIELD_THREE_ROW)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("no enclosing" in v for v in violations),
           "a table row with no capability heading in scope at all must "
           "be flagged, not silently ignored")


def test_unknown_lifecycle_detected():
    bad_row = FIELD_ONE_ROW.replace("boot-process", "some-made-up-lifecycle")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Lifecycle cell" in v for v in violations),
           "an unrecognized lifecycle identifier must be rejected")


def test_unknown_thread_role_detected():
    bad_row = (
        "| `fieldOne` | boot-process | `SomeMadeUpThread` (`src/Fake/Reader.hs:1`) "
        "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` | `src/Fake/Init.hs:5` "
        "| None | — |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Readers cell" in v
               and "SomeMadeUpThread" in v for v in violations),
           "a Readers cell naming no recognized thread role must be rejected")


def test_mixed_valid_and_unknown_role_detected():
    # A cell with a GENUINELY valid role (`MainRender`) sitting beside an
    # unrecognized one (`AlienThread`) must still be rejected -- the
    # presence of one valid role must never let an invalid one slip
    # through silently.
    bad_row = (
        "| `fieldOne` | boot-process "
        "| `MainRender` (`src/Fake/Reader.hs:1`), `AlienThread` (`src/Fake/Alien.hs:1`) "
        "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` | `src/Fake/Init.hs:5` "
        "| None | — |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Readers cell" in v
               and "AlienThread" in v for v in violations),
           "a Readers cell mixing one valid role (MainRender) with one "
           "unrecognized role (AlienThread) must still be rejected, not "
           "silently accepted on the strength of the valid one")


def test_bare_unquoted_unknown_role_detected():
    # The literal shape from round-3 review: a valid, backtick-quoted
    # role followed by a BARE, unquoted, uncited role-shaped word. The
    # audit must not silently ignore the unquoted one just because it
    # isn't wrapped in backticks.
    bad_row = (
        "| `fieldOne` | boot-process "
        "| `MainRender` (`src/Fake/Reader.hs:10`), AlienThread "
        "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` | `src/Fake/Init.hs:5` "
        "| None | — |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Readers cell" in v
               and "AlienThread" in v for v in violations),
           "a bare, unquoted, uncited role-shaped word (AlienThread) "
           "sitting beside a valid quoted role must still be rejected")


def test_blank_reader_decision_detected():
    bad_row = (
        "| `fieldOne` | boot-process |  "
        "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` | `src/Fake/Init.hs:5` "
        "| None | — |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "no Readers decision" in v for v in violations),
           "a blank Readers cell must be flagged as a missing decision, "
           "distinct from an unrecognized-role cell")


def test_unjustified_none_writer_detected():
    bad_row = (
        "| `fieldOne` | boot-process | `MainRender` (`src/Fake/Reader.hs:10`) "
        "| None | `IORef Int` | `src/Fake/Init.hs:5` | None | — |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Writers cell" in v for v in violations),
           "a bare 'None' with no parenthetical justification must be "
           "rejected -- only a JUSTIFIED no-writers claim is valid")


def test_justified_none_writer_accepted():
    # fieldThree in the complete fixture already exercises this; a
    # focused re-check in isolation guards against the two cases being
    # accidentally conflated.
    violations = audit(SYNTHETIC_ENGINE_ENV, SYNTHETIC_INVENTORY_COMPLETE)
    expect(not any("fieldThree" in v for v in violations),
           "fieldThree's justified 'None (immutable boot configuration...)' "
           "writers cell must be accepted, not flagged")


def test_missing_sync_contract_detected():
    bad_row = FIELD_ONE_ROW.replace("`IORef Int`", "-")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Sync cell" in v for v in violations),
           "a placeholder Sync cell ('-') must be rejected")


def test_missing_grounding_evidence_detected():
    bad_row = (
        "| `fieldOne` | boot-process | `MainRender` (somewhere) "
        "| `Boot` (elsewhere) | IORef Int | boot init | None | — |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "cites no source-location evidence" in v
               for v in violations),
           "a row with no backtick-quoted .hs/.lua citation anywhere must "
           "be flagged for missing grounding evidence")


def test_valid_multi_reader_multi_writer_field_passes():
    violations = audit(SYNTHETIC_ENGINE_ENV, SYNTHETIC_INVENTORY_COMPLETE)
    expect(not any("fieldTwo" in v for v in violations),
           "fieldTwo's genuinely multi-reader/multi-writer classification "
           "(WorldThread + LuaThread on both sides) must pass cleanly")


def test_parse_inventory_only_scans_section_5():
    # A '### <capability>'-shaped heading and table appearing OUTSIDE
    # section 5 (e.g. under an unrelated later section) must not be
    # picked up as inventory rows at all.
    doc = (SYNTHETIC_INVENTORY_COMPLETE +
           "\n### core-init\n\n" + _HEADER +
           "| `fieldFour` | boot-process | `MainRender` (`x.hs:1`) "
           "| `Boot` (`x.hs:1`) | `IORef Int` | `x.hs:1` | None | — |\n")
    rows, _ = parse_inventory(doc)
    names = {r.field for r in rows}
    expect("fieldFour" not in names,
           "a table appearing after section 5 has ended must be ignored, "
           "even if it reuses a real capability heading")


def test_audit_against_the_real_repo():
    real_source = (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8")
    real_inventory = (REPO_ROOT / "docs" /
                       "engineenv_capability_inventory.md").read_text(encoding="utf-8")
    violations = audit(real_source, real_inventory)
    expect(violations == [],
           f"the real EngineEnv + the real inventory doc should have zero "
           f"violations, got: {violations}")
    live_fields = extract_record_fields(real_source, ENGINE_ENV_PATTERN)
    expect(len(live_fields) == 81,
           f"expected 81 live EngineEnv fields (issue #876's own count), "
           f"got {len(live_fields)}")


def main() -> int:
    tests = [
        test_complete_inventory_has_no_violations,
        test_missing_row_detected,
        test_duplicate_row_detected,
        test_stale_row_detected,
        test_unknown_capability_heading_detected,
        test_row_with_no_enclosing_heading_detected,
        test_unknown_lifecycle_detected,
        test_unknown_thread_role_detected,
        test_mixed_valid_and_unknown_role_detected,
        test_bare_unquoted_unknown_role_detected,
        test_blank_reader_decision_detected,
        test_unjustified_none_writer_detected,
        test_justified_none_writer_accepted,
        test_missing_sync_contract_detected,
        test_missing_grounding_evidence_detected,
        test_valid_multi_reader_multi_writer_field_passes,
        test_parse_inventory_only_scans_section_5,
        test_audit_against_the_real_repo,
    ]

    for t in tests:
        print(f"{t.__name__}:")
        t()

    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s):")
        for f in FAILURES:
            print(f"  {f}")
        return 1

    print(f"\nAll {len(tests)} test groups passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
