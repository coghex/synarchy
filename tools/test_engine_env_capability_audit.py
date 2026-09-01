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
    classify_state_import, parse_temporary_boundary, audit_ratchet,
    scan_production_unrestricted_importers, audit_render_boundary,
    scan_production_sources, RENDER_CAPABILITY_MODULE, RENDER_VIEW_MODULE,
    audit_input_boundary, INPUT_CAPABILITY_MODULE, INPUT_VIEW_MODULE,
    CAPABILITIES, PERMANENT_IMPORTERS, PERMANENT_DEFINER, TEMPORARY_CEILING,
    parse_permanent_boundary, audit_permanent_boundary,
    audit_field_total, extract_marked_spans, section_bounds,
    FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE, ONE_ROW_PHRASE,
    SECTION_1_HEADING, SECTION_6_2_HEADING, PROCEDURE_ITEM_ANCHOR,
    audit_save_load_projection, parse_projection_bindings,
    SAVE_LOAD_CAPABILITY_MODULE, SAVE_LOAD_CAPABILITY_FILE,
    SAVE_LOAD_FIELD_MAP, SAVE_LOAD_PROJECTION,
    _import_chunks, _strip_haskell_comments,
    CAPABILITY_WRITER_MODULES, capability_accessor_map,
    discover_capability_records, canonical_projection_accessor,
    capability_record_fields, undiscovered_capability_declarations,
    parse_projection_binding_expressions,
    audit_capability_projection_completeness,
    scan_capability_writes, audit_writer_modules, format_residue,
    tokenize_haskell, parse_imports, imports_name,
    _first_argument_head, _infix_left_operand_head, _applied_head,
    classify_mutation_site, audit_mutation_sites, audit_shadow_exemptions,
    SHADOW_EXEMPTIONS, resolve_primitive,
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


def test_malformed_capability_heading_resets_scope():
    # Round-10 review: a malformed '### ' heading (blank, or otherwise
    # not matching HEADING_RE) between two valid capability sections
    # used to be silently ignored, leaving `current_capability` holding
    # the PRECEDING section's value -- rows after it wrongly inherited
    # that capability instead of being flagged as unclassified. A bare
    # "###" (no name at all) is the malformed heading here.
    doc = (
        "# Fake capability inventory\n\n"
        "## 5. Field inventory\n\n"
        f"### core-init\n\n{_HEADER}{FIELD_ONE_ROW}\n"
        "###\n\n"
        f"{_HEADER}{FIELD_TWO_ROW}{FIELD_THREE_ROW}\n"
        "## 6. Something else entirely\n\n"
    )
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("malformed" in v.lower() and "###" in v for v in violations),
           "a malformed '### ' heading itself must be reported")
    expect(any("no enclosing" in v for v in violations),
           "rows after a malformed heading must NOT silently inherit the "
           "preceding section's capability -- they must be reported as "
           "having no enclosing heading in scope")
    expect(any("fieldTwo" in v and "has no row" in v for v in violations),
           "fieldTwo must end up with no valid classification at all (not "
           "wrongly attributed to core-init), since its row was skipped "
           "for lacking an enclosing heading")


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


def test_lower_camel_unknown_role_detected():
    # Round-4 review: a mistyped role can be lower-camel-cased
    # ("alienThread") rather than PascalCase -- the leading-token scan
    # must not silently skip it just because it doesn't start with an
    # uppercase letter.
    bad_row = (
        "| `fieldOne` | boot-process "
        "| `MainRender` (`src/Fake/Reader.hs:10`), alienThread "
        "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` | `src/Fake/Init.hs:5` "
        "| None | — |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Readers cell" in v
               and "alienThread" in v for v in violations),
           "a lower-camel-cased, unquoted, uncited role-shaped word "
           "(alienThread) sitting beside a valid quoted role must still "
           "be rejected")


def test_conjunction_joined_unknown_role_detected():
    # Round-7 review's literal shape: a valid role and an invalid one
    # joined by the word "and" within the SAME segment, rather than by
    # "/" or a comma -- the leading-run scan must chain through " and "
    # as a continuation joiner, not just "/".
    bad_row = (
        "| `fieldOne` | boot-process "
        "| `MainRender` and AlienThread (`src/Fake/Reader.hs:10`) "
        "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` | `src/Fake/Init.hs:5` "
        "| None | — |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Readers cell" in v
               and "AlienThread" in v for v in violations),
           "an 'and'-joined unknown role (AlienThread) sitting beside a "
           "valid one (MainRender) in the same segment must still be "
           "rejected, not silently accepted because 'and' isn't a "
           "recognized joiner")


def test_wrong_shaped_quoted_role_detected():
    # Round-8 review's literal shapes: a backtick-quoted role attempt
    # that does NOT end in "Thread"/"Render" and isn't "Boot" --
    # AlienWorker, Mainrender (lowercase r), and LuaThreadish (extra
    # suffix) -- must still be rejected. A shape-restricted check
    # (round 7's design) missed these; a leading-run scan catches them
    # regardless of shape, since it validates WHATEVER token occupies
    # the leading position, not just ones matching a fixed suffix.
    for bad_token in ("AlienWorker", "Mainrender", "LuaThreadish"):
        bad_row = (
            "| `fieldOne` | boot-process "
            f"| `MainRender`, `{bad_token}` (`src/Fake/Reader.hs:10`) "
            "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` | `src/Fake/Init.hs:5` "
            "| None | — |\n")
        doc = _doc(core_init_rows=bad_row)
        violations = audit(SYNTHETIC_ENGINE_ENV, doc)
        expect(any("fieldOne" in v and "Readers cell" in v
                   and bad_token in v for v in violations),
               f"a wrong-shaped quoted role ({bad_token}) sitting beside a "
               f"valid one (MainRender) in its own comma segment must "
               f"still be rejected")


def test_arbitrary_joiner_unknown_role_detected():
    # Round-9 review: the reviewer keeps finding a new joiner word each
    # round ("and" in round 7, then ";"/"plus" here) -- rather than
    # enumerate yet another one, the strict grammar rejects ANY text
    # between roles/after a role's own trailing paren that isn't a bare
    # "/"-joined role list, so no joiner word is special-cased at all.
    # Exercises semicolon- and "plus"-joined forms explicitly, since
    # those are the literal words this round's review named.
    for joiner in ("; ", " plus "):
        bad_row = (
            "| `fieldOne` | boot-process "
            f"| `MainRender`{joiner}`AlienThread` (`src/Fake/Reader.hs:1`) "
            "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` | `src/Fake/Init.hs:5` "
            "| None | — |\n")
        doc = _doc(core_init_rows=bad_row)
        violations = audit(SYNTHETIC_ENGINE_ENV, doc)
        expect(any("fieldOne" in v and "Readers cell" in v
                   and ("grammar" in v or "AlienThread" in v)
                   for v in violations),
               f"an unknown role joined by {joiner!r} rather than '/' or a "
               f"comma must still be rejected -- the segment doesn't match "
               f"the required grammar at all, which is itself a violation")


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


def test_whitespace_only_none_justification_detected():
    bad_row = (
        "| `fieldOne` | boot-process | `MainRender` (`src/Fake/Reader.hs:10`) "
        "| None (   ) | `IORef Int` | `src/Fake/Init.hs:5` | None | — |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Writers cell" in v for v in violations),
           "a 'None ( )' cell whose parenthetical holds only whitespace "
           "must be rejected -- it records no actual reason, so it is "
           "just as unjustified as a bare 'None' with no parenthetical "
           "at all")


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


def test_blank_init_shutdown_notes_detected():
    # Round-4 review: a synthetic row with Init/Shutdown/Notes all blank
    # used to return zero violations -- only Sync was ever checked.
    bad_row = (
        "| `fieldOne` | boot-process | `MainRender` (`src/Fake/Reader.hs:10`) "
        "| `Boot` (`src/Fake/Init.hs:5`) | `IORef Int` |  |  |  |\n")
    doc = _doc(core_init_rows=bad_row)
    violations = audit(SYNTHETIC_ENGINE_ENV, doc)
    expect(any("fieldOne" in v and "Init cell" in v for v in violations),
           "a blank Init cell must be flagged")
    expect(any("fieldOne" in v and "Shutdown cell" in v for v in violations),
           "a blank Shutdown cell must be flagged")
    expect(any("fieldOne" in v and "Notes cell" in v for v in violations),
           "a blank Notes cell must be flagged")


def test_em_dash_notes_accepted():
    # Notes is the one column where "nothing further to add" is itself
    # a legitimate, deliberate answer -- a bare em-dash there (this
    # document's own convention throughout) must NOT be rejected the
    # way a blank Sync/Init/Shutdown cell is.
    violations = audit(SYNTHETIC_ENGINE_ENV, SYNTHETIC_INVENTORY_COMPLETE)
    expect(not any("Notes cell" in v for v in violations),
           "an em-dash Notes cell (used throughout the complete fixture) "
           "must be accepted, not flagged as blank")


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


# ----- SS6 full-access ratchet (issue #889) -----------------------------


def test_bare_import_detected():
    src = "module Foo where\nimport Engine.Core.State\nfoo ∷ Int\nfoo = 1\n"
    expect(classify_state_import(src) == "bare",
           "a plain `import Engine.Core.State` with no import list at all "
           "must classify as bare (grants full access to every export)")


def test_bare_import_qualified_and_aliased_detected():
    cases = (
        "import qualified Engine.Core.State\n",
        "import Engine.Core.State as ST\n",
        "import qualified Engine.Core.State as ST\n",
    )
    for src in cases:
        full = "module Foo where\n" + src + "foo ∷ Int\nfoo = 1\n"
        expect(classify_state_import(full) == "bare",
               f"a qualified and/or aliased bare import must still "
               f"classify as bare: {src!r}")


def test_bare_import_multiline_detected():
    src = "module Foo where\nimport\n  Engine.Core.State\nfoo ∷ Int\nfoo = 1\n"
    expect(classify_state_import(src) == "bare",
           "a bare import whose module name wraps onto a continuation "
           "line must still classify as bare")


def test_bare_import_as_last_import_not_swallowed_by_later_code():
    # A bare import that happens to be the FILE'S LAST import has no
    # following `^import` line to bound it against -- naively scanning
    # "from this import to the next import-or-EOF" would swallow the
    # entire rest of the module (every subsequent function body's
    # parentheses included) and misreport it as non-bare. The real
    # Engine.Core.Monad.hs is exactly this shape.
    src = (
        "module Foo where\n"
        "import Data.IORef (readIORef)\n"
        "import Engine.Core.State\n"
        "\n"
        "-- | some docs with (parens) and (more parens)\n"
        "data Something = Something { field ∷ Int → (Int, Int) }\n"
    )
    expect(classify_state_import(src) == "bare",
           "a bare import that is the file's last import must not be "
           "misread as narrow merely because later top-level code "
           "contains parentheses")


def test_explicit_engineenv_ordinary_qualified_and_multiline_detected():
    cases = (
        "import Engine.Core.State (EngineEnv(..))\n",
        "import qualified Engine.Core.State as ST (EngineEnv(..))\n",
        "import Engine.Core.State (SomeOtherType, EngineEnv (..))\n",
        "import Engine.Core.State\n  ( EngineEnv(..)\n  , SomeOtherType\n  )\n",
    )
    for src in cases:
        full = "module Foo where\n" + src + "foo ∷ Int\nfoo = 1\n"
        expect(classify_state_import(full) == "explicit",
               f"an explicit EngineEnv(..) import (ordinary, qualified, "
               f"aliased, or multiline) must classify as explicit: {src!r}")


def test_narrow_import_not_classified_unrestricted():
    cases = (
        "import Engine.Core.State (EngineEnv)\n",
        "import Engine.Core.State (loggerRef)\n",
        "import Engine.Core.State (EngineEnv, loggerRef)\n",
    )
    for src in cases:
        full = "module Foo where\n" + src + "foo ∷ Int\nfoo = 1\n"
        expect(classify_state_import(full) == "narrow",
               f"a strictly narrower import (bare EngineEnv type or "
               f"individual field accessors, no (..)) must not classify "
               f"as unrestricted: {src!r}")


def test_no_state_import_returns_none():
    src = "module Foo where\nimport Data.Text (Text)\nfoo ∷ Int\nfoo = 1\n"
    expect(classify_state_import(src) is None,
           "a file that never imports Engine.Core.State must classify as "
           "None, not bare/explicit/narrow")


FAKE_INVENTORY_6_2 = """\
### 6.2 Temporary compatibility boundary (production)

| Target capability | Modules | Roadmap entry |
|---|---|---|
| `core-init` | `Foo.Bar`, `Foo.Baz` | §7.1 |
| `save-load-coordination` | *(none — every module whose dominant field \
usage is save/load coordination is already a permanent orchestration \
exception; `Some.Explanatory.Module` was previously assigned here for its \
`someField` read, but its dominant usage is elsewhere)* | §7.8 |

### 6.3 Test-only exceptions

not part of SS6.2 at all
"""


def test_section_6_2_parser_extracts_real_assignments():
    parsed = parse_temporary_boundary(FAKE_INVENTORY_6_2)
    expect(parsed.get("core-init") == {"Foo.Bar", "Foo.Baz"},
           "a normal SS6.2 row must parse its backtick-quoted module names")


def test_section_6_2_parser_ignores_explanatory_backtick_references():
    parsed = parse_temporary_boundary(FAKE_INVENTORY_6_2)
    expect(parsed.get("save-load-coordination") == set(),
           "a Modules cell that is pure explanatory prose (wrapped "
           "entirely in *(...)*) must parse to zero assigned modules, "
           "even though it cites backtick-quoted names for context -- "
           "the real save-load-coordination row is exactly this shape")


def test_new_full_access_module_rejected():
    permanent = frozenset({"Perm.Mod"})
    ceiling = {"core-init": frozenset({"Temp.Mod"})}
    doc = {"core-init": {"Temp.Mod"}}
    unrestricted = {"Perm.Mod", "Temp.Mod", "New.Unclassified.Mod"}
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("New.Unclassified.Mod" in v for v in violations),
           "a module with unrestricted access that is neither permanent "
           "nor in the checked-in temporary ceiling must be rejected")


def test_shrinking_migration_accepted():
    # A migration narrows Temp.B away (it's no longer unrestricted at
    # all) and the checked-in ceiling + SS6.2 doc are updated together
    # to drop it -- this must be a clean, zero-violation migration.
    permanent = frozenset({"Perm.Mod"})
    ceiling = {"core-init": frozenset({"Temp.A"})}
    doc = {"core-init": {"Temp.A"}}
    unrestricted = {"Perm.Mod", "Temp.A"}
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(violations == [],
           "a migration that shrinks the live temporary set, with the "
           "checked-in ceiling and SS6.2 doc updated in tandem, must be "
           "accepted with zero violations")


def test_documented_but_ungoverned_addition_still_rejected():
    # Someone adds real unrestricted access to a new module AND
    # documents it in SS6.2, but never grows the checked-in
    # TEMPORARY_CEILING itself -- the strict ceiling must still reject
    # it; documentation alone never admits a new full-access module.
    permanent = frozenset({"Perm.Mod"})
    ceiling = {"core-init": frozenset({"Temp.A"})}
    doc = {"core-init": {"Temp.A", "Sneaky.New.Mod"}}
    unrestricted = {"Perm.Mod", "Temp.A", "Sneaky.New.Mod"}
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("Sneaky.New.Mod" in v for v in violations),
           "a new full-access module must still be rejected even when "
           "SS6.2 is ALSO edited to document it -- only growing the "
           "checked-in ceiling itself admits a new temporary module")


def test_stale_ceiling_entry_rejected():
    # A migration narrows a module (it no longer has live unrestricted
    # access) but never shrinks the checked-in ceiling or its SS6.2
    # row to match -- the ratchet must reject this drift in EITHER
    # direction, not just growth: SS6.2's accounting must stay an
    # exact mirror of the live temporary set, not merely an upper bound.
    permanent = frozenset({"Perm.Mod"})
    ceiling = {"core-init": frozenset({"Temp.A", "Temp.Stale"})}
    doc = {"core-init": {"Temp.A", "Temp.Stale"}}
    unrestricted = {"Perm.Mod", "Temp.A"}  # Temp.Stale no longer unrestricted
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("Temp.Stale" in v for v in violations),
           "a checked-in ceiling/SS6.2 entry that no longer has live "
           "unrestricted access must be flagged as stale, not silently "
           "tolerated as a mere upper bound")


def test_stale_permanent_importer_rejected():
    # A permanent (SS6.1) module is narrowed by a later change (no
    # longer live-unrestricted) but PERMANENT_IMPORTERS is never
    # updated to drop it -- this must fail just like a stale temporary
    # ceiling entry does; the permanent allowlist is not exempt from
    # the live-scan agreement requirement.
    permanent = frozenset({"Perm.Stale"})
    ceiling = {"core-init": frozenset({"Temp.Live"})}
    doc = {"core-init": {"Temp.Live"}}
    unrestricted = {"Temp.Live"}  # Perm.Stale no longer unrestricted
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("Perm.Stale" in v for v in violations),
           "a checked-in PERMANENT_IMPORTERS entry that no longer has live "
           "unrestricted access must be flagged as stale, matching the "
           "temporary-ceiling side's same requirement")


def test_ceiling_and_doc_mismatch_detected():
    permanent: frozenset[str] = frozenset()
    ceiling = {"core-init": frozenset({"Temp.A", "Temp.B"})}
    doc = {"core-init": {"Temp.A"}}  # Temp.B undocumented
    unrestricted = {"Temp.A", "Temp.B"}
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("Temp.B" in v and "core-init" in v for v in violations),
           "a checked-in ceiling entry missing from SS6.2's documented "
           "accounting must be flagged as a doc/ceiling mismatch")


def test_real_repo_ratchet_consistency():
    real_inventory = (REPO_ROOT / "docs" /
                       "engineenv_capability_inventory.md").read_text(encoding="utf-8")
    unrestricted = scan_production_unrestricted_importers(REPO_ROOT)
    doc_temporary = parse_temporary_boundary(real_inventory)
    violations = audit_ratchet(unrestricted, doc_temporary)
    expect(violations == [],
           f"the real repo's live-scanned production importer set, the "
           f"checked-in PERMANENT_IMPORTERS/TEMPORARY_CEILING constants, "
           f"and SS6.2 as documented should all agree after issue #889's "
           f"core-init migration, got: {violations}")


# ----- SS6.1 permanent-boundary parse/compare (issue #899, E8) ----------
#
# `audit_ratchet` pins the checked-in constants to the LIVE source in
# both directions, but before #899 nothing pinned them to SS6.1's
# DOCUMENTED table -- so growing a live importer AND the Python
# constant together passed with the inventory never recording why the
# module is a genuine whole-session orchestration boundary. These
# fixtures exercise the pure parse/compare core synthetically; the
# real-repository assertion lives in the end-state test below.

_SECTION_6_1 = """\
### 6.1 Permanent (production)

| Module(s) | Category | Reason |
|---|---|---|
| `Perm.One` | Permanent initialization infrastructure | Defines the record; see `Some.Other.Module` for context. |
| `Perm.Two`, `Perm.Three` | Permanent orchestration infrastructure | Boot wire-up: constructs everything, by job description. |

### 6.2 Temporary compatibility boundary (production)

| Target capability | Modules | Roadmap entry |
|---|---|---|
| `core-init` | *(none)* | SS7.1 |
"""


def test_section_6_1_parser_reads_first_column_only():
    rows = parse_permanent_boundary(_SECTION_6_1)
    modules = set()
    for names, _cat, _reason in rows:
        modules |= names
    expect(modules == {"Perm.One", "Perm.Two", "Perm.Three"},
           f"SS6.1's parser must read the Module(s) column ONLY -- the "
           f"Reason cells deliberately cite other backtick-quoted module "
           f"names as supporting context, and those are NOT permanent "
           f"allowlist entries, got: {sorted(modules)}")
    expect(not any("Some.Other.Module" in names for names, _c, _r in rows),
           "a module named only inside a Reason cell must never be "
           "admitted to the documented permanent set")


def test_section_6_1_matching_constants_accepted():
    violations = audit_permanent_boundary(
        _SECTION_6_1,
        permanent=frozenset({"Perm.Two", "Perm.Three"}), definer="Perm.One")
    expect(violations == [],
           f"a SS6.1 table whose documented set equals PERMANENT_DEFINER + "
           f"PERMANENT_IMPORTERS, with real Category/Reason cells, must "
           f"pass, got: {violations}")


def test_section_6_1_undocumented_permanent_importer_rejected():
    violations = audit_permanent_boundary(
        _SECTION_6_1,
        permanent=frozenset({"Perm.Two", "Perm.Three", "Perm.Sneaky"}),
        definer="Perm.One")
    expect(any("Perm.Sneaky" in v for v in violations),
           "growing PERMANENT_IMPORTERS without a matching SS6.1 row (the "
           "'Python constant change without the matching inventory "
           "justification' case) must be rejected")


def test_section_6_1_documented_but_ungoverned_module_rejected():
    violations = audit_permanent_boundary(
        _SECTION_6_1,
        permanent=frozenset({"Perm.Two"}), definer="Perm.One")
    expect(any("Perm.Three" in v for v in violations),
           "documenting a module in SS6.1 without adding it to the "
           "checked-in constants must be rejected -- documentation alone "
           "does not admit a permanent importer")


def test_section_6_1_placeholder_justification_rejected():
    doc = _SECTION_6_1.replace(
        "| `Perm.One` | Permanent initialization infrastructure | Defines "
        "the record; see `Some.Other.Module` for context. |",
        "| `Perm.One` | — | — |")
    violations = audit_permanent_boundary(
        doc, permanent=frozenset({"Perm.Two", "Perm.Three"}),
        definer="Perm.One")
    expect(sum("Perm.One" in v for v in violations) == 2,
           f"a name-only SS6.1 row with placeholder Category AND Reason "
           f"cells must be rejected on BOTH -- otherwise it satisfies the "
           f"module-set equality while providing none of the explicit "
           f"justification SS6.1/SS6.4 demand, got: {violations}")


def test_section_6_1_missing_table_rejected():
    violations = audit_permanent_boundary(
        "## 6. Full-EngineEnv compatibility boundary\n\nno table here\n",
        permanent=frozenset({"Perm.Two"}), definer="Perm.One")
    expect(len(violations) == 1 and "could not be parsed" in violations[0],
           f"an unparseable/absent SS6.1 table must fail loudly rather "
           f"than silently comparing against an empty documented set, "
           f"got: {violations}")


# ----- SaveLoadCapability projection correspondence (#899, E8) ----------

_SAVE_LOAD_GOOD = """\
module Engine.Core.Capability.SaveLoad where

-- | Haddock mentioning slSaveBarrierRef = loadStatusRef env must not count.
data SaveLoadCapability = SaveLoadCapability
  { slLoadStatusRef ∷ LoadStatusRef
  }

toSaveLoadCapability ∷ EngineEnv → SaveLoadCapability
toSaveLoadCapability env = SaveLoadCapability
  { slLoadStatusRef         = loadStatusRef env
  , slPendingLoadRef        = pendingLoadRef env
  , slSaveBarrierRef        = saveBarrierRef env
  , slLastSaveTimeRef       = lastSaveTimeRef env
  , slNextItemInstanceIdRef = nextItemInstanceIdRef env
  }
"""

_SAVE_LOAD_CABAL = "                     Engine.Core.Capability.SaveLoad\n"


def test_save_load_projection_clean_case_passes():
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: _SAVE_LOAD_GOOD}, _SAVE_LOAD_CABAL)
    expect(violations == [],
           f"a projection binding all five handles from their matching "
           f"EngineEnv accessors, with the module listed in the cabal "
           f"file, must pass, got: {violations}")


def test_save_load_projection_transposed_binding_rejected():
    bad = _SAVE_LOAD_GOOD.replace(
        "slPendingLoadRef        = pendingLoadRef env",
        "slPendingLoadRef        = loadStatusRef env")
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: bad}, _SAVE_LOAD_CABAL)
    expect(any("slPendingLoadRef" in v and "loadStatusRef" in v
               for v in violations),
           "a field bound from the WRONG EngineEnv accessor must be "
           "caught: the static check is what a Python audit can do about "
           "an aliasing mistake that typechecks silently")


def test_save_load_projection_missing_field_rejected():
    bad = _SAVE_LOAD_GOOD.replace(
        "  , slSaveBarrierRef        = saveBarrierRef env\n", "")
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: bad}, _SAVE_LOAD_CABAL)
    expect(any("slSaveBarrierRef" in v for v in violations),
           "a projection that is not TOTAL over the five documented "
           "handles must be rejected")


def test_save_load_projection_extra_field_rejected():
    bad = _SAVE_LOAD_GOOD.replace(
        "  }\n", "  , slSomethingElse         = engineConfig env\n  }\n")
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: bad}, _SAVE_LOAD_CABAL)
    expect(any("slSomethingElse" in v for v in violations),
           "silently widening the record past SS5's five documented "
           "handles must be rejected")


def test_save_load_projection_unlisted_in_cabal_rejected():
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: _SAVE_LOAD_GOOD}, "no module list here\n")
    expect(any("synarchy.cabal" in v for v in violations),
           "an unlisted source file is never compiled, so a warning-clean "
           "build says nothing about it -- the cabal listing must be "
           "checked explicitly")


def test_save_load_projection_missing_module_rejected():
    violations = audit_save_load_projection({}, _SAVE_LOAD_CABAL)
    expect(len(violations) == 1
           and SAVE_LOAD_CAPABILITY_MODULE in violations[0],
           f"a missing capability module must fail on its own, not as five "
           f"per-field errors, got: {violations}")


def test_save_load_projection_ignores_haddock_bindings():
    bindings = parse_projection_bindings(_SAVE_LOAD_GOOD, SAVE_LOAD_PROJECTION)
    expect(bindings.get("slSaveBarrierRef") == "saveBarrierRef",
           f"a `field = accessor env` pair inside a Haddock comment must "
           f"never be read as a real binding (the fixture's comment says "
           f"`slSaveBarrierRef = loadStatusRef env`), got: "
           f"{bindings.get('slSaveBarrierRef')}")


# ----- SS3 main-render boundary (issue #891, capability split E3) -------
#
# The SS3 boundary is what makes `render-gpu-asset`'s two-interface
# split a real access boundary rather than a documented convention:
# worker-thread code must have NO interface through which it can
# construct or inspect a record containing `engineStateRef`. These
# fixtures exercise `audit_render_boundary`'s pure core with synthetic
# sources, never by editing real production modules.

_MAIN = "Main.Render.Mod"
_WORKER = "Worker.Mod"


def _boundary_sources(*, worker_imports_full=False, worker_names_ref=False,
                      view_names_ref=False, include_view=True,
                      main_imports_full=True):
    """Minimal synthetic production tree: one MainRender module, one
    worker module, and the worker-safe view module itself."""
    view_body = "module Engine.Core.Capability.RenderView where\n"
    if view_names_ref:
        view_body += "  rvEngineStateRef = engineStateRef env\n"
    else:
        view_body += "  rvCameraRef = cameraRef env\n"

    main_body = f"module {_MAIN} where\n"
    if main_imports_full:
        main_body = f"import {RENDER_CAPABILITY_MODULE}\n" + main_body

    worker_body = f"module {_WORKER} where\n"
    if worker_imports_full:
        worker_body = f"import {RENDER_CAPABILITY_MODULE}\n" + worker_body
    else:
        worker_body = f"import {RENDER_VIEW_MODULE}\n" + worker_body
    if worker_names_ref:
        worker_body += "  x = readIORef (rcEngineStateRef cap)\n"

    sources = {
        "src/Main/Render/Mod.hs": main_body,
        "src/Worker/Mod.hs": worker_body,
    }
    if include_view:
        sources["src/Engine/Core/Capability/RenderView.hs"] = view_body
    return sources


def test_boundary_clean_tree_has_no_violations():
    violations = audit_render_boundary(
        _boundary_sources(),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(violations == [],
           f"a tree where only the MainRender module imports the full "
           f"render capability, the worker imports only the view, and the "
           f"view never names engineStateRef must pass, got: {violations}")


def test_boundary_worker_importing_full_capability_rejected():
    violations = audit_render_boundary(
        _boundary_sources(worker_imports_full=True),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(any(_WORKER in v and RENDER_CAPABILITY_MODULE in v
               for v in violations),
           "a non-MainRender production module importing the full "
           "RenderCapability must be rejected -- that record carries "
           "engineStateRef, which SS3 makes main-render private")


def test_boundary_non_owner_naming_engine_state_ref_rejected():
    violations = audit_render_boundary(
        _boundary_sources(worker_names_ref=True),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(any(_WORKER in v and "engineStateRef" in v for v in violations),
           "a production module outside ENGINE_STATE_REF_OWNERS naming "
           "engineStateRef/rcEngineStateRef must be rejected")


def test_boundary_engine_state_ref_in_a_comment_is_not_a_violation():
    # Haddock on the view legitimately EXPLAINS why the field is absent.
    # Only live code counts, or the enforcement would forbid documenting
    # its own rule.
    sources = _boundary_sources()
    sources["src/Engine/Core/Capability/RenderView.hs"] = (
        "-- | Deliberately contains no engineStateRef field.\n"
        "module Engine.Core.Capability.RenderView where\n"
        "  rvCameraRef = cameraRef env  -- not engineStateRef\n")
    violations = audit_render_boundary(
        sources, main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(violations == [],
           f"a Haddock/line comment mentioning engineStateRef must not "
           f"count as naming it, got: {violations}")


def test_boundary_view_carrying_engine_state_ref_rejected():
    violations = audit_render_boundary(
        _boundary_sources(view_names_ref=True),
        main_only=frozenset({_MAIN}),
        state_ref_owners=frozenset({RENDER_VIEW_MODULE}))
    expect(any(RENDER_VIEW_MODULE in v for v in violations),
           "the worker-visible view must be rejected if it so much as "
           "names engineStateRef -- even being listed as an owner must "
           "not buy it an exemption from the structural check")


def test_boundary_missing_view_module_rejected():
    violations = audit_render_boundary(
        _boundary_sources(include_view=False),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(any(RENDER_VIEW_MODULE in v and "missing" in v
               for v in violations),
           "deleting the worker-safe view must fail loudly -- SS3's "
           "boundary has no enforcement without it")


def test_boundary_stale_main_only_entry_rejected():
    # Same both-directions discipline as the SS6 ratchet: a module listed
    # as MainRender that no longer imports the full record is drift.
    violations = audit_render_boundary(
        _boundary_sources(main_imports_full=False),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(any(_MAIN in v and "stale" in v for v in violations),
           "a stale RENDER_MAIN_ONLY_MODULES entry must be flagged, so the "
           "checked-in set stays an exact mirror of the live one")


def test_boundary_stale_state_ref_owner_rejected():
    violations = audit_render_boundary(
        _boundary_sources(),
        main_only=frozenset({_MAIN}),
        state_ref_owners=frozenset({"Ghost.Owner"}))
    expect(any("Ghost.Owner" in v and "stale" in v for v in violations),
           "a stale ENGINE_STATE_REF_OWNERS entry must be flagged too")


def test_real_repo_render_boundary_holds():
    violations = audit_render_boundary(scan_production_sources(REPO_ROOT))
    expect(violations == [],
           f"the real repo must satisfy SS3's main-render boundary after "
           f"issue #891's render-gpu-asset migration, got: {violations}")


# ----- SS7.3 LuaThread input boundary (issue #892, capability split E4) -
#
# Same shape as the SS3 fixtures above, for `input-lua-transport`'s two
# LuaThread-private fields. A worker-thread module must have NO
# interface through which it can allocate a barrier token
# (`inputBarrierNextRef`) or reach the `onKeyDown` current-key handoff
# (`currentKeyDownRef`) -- it gets the barrier WATERMARK and nothing
# else. These fixtures exercise `audit_input_boundary`'s pure core with
# synthetic sources, never by editing real production modules.

_LUA = "Lua.Api.Mod"
_INPUT_WORKER = "Input.Worker.Mod"


def _input_sources(*, worker_imports_full=False, worker_names_alloc=False,
                   worker_names_keydown=False, view_names_field=False,
                   include_view=True, lua_imports_full=True):
    """Minimal synthetic production tree: one LuaThread module, one
    input-thread worker module, and the worker-safe view module."""
    view_body = "module Engine.Core.Capability.InputView where\n"
    if view_names_field:
        view_body += "  ivInputBarrierNextRef = inputBarrierNextRef env\n"
    else:
        view_body += "  ivInputBarrierRef = inputBarrierRef env\n"

    lua_body = f"module {_LUA} where\n"
    if lua_imports_full:
        lua_body = f"import {INPUT_CAPABILITY_MODULE}\n" + lua_body

    worker_body = f"module {_INPUT_WORKER} where\n"
    if worker_imports_full:
        worker_body = f"import {INPUT_CAPABILITY_MODULE}\n" + worker_body
    else:
        worker_body = f"import {INPUT_VIEW_MODULE}\n" + worker_body
    if worker_names_alloc:
        worker_body += "  t = newBarrierToken (icInputBarrierNextRef cap)\n"
    if worker_names_keydown:
        worker_body += "  k = readIORef (currentKeyDownRef env)\n"

    sources = {
        "src/Lua/Api/Mod.hs": lua_body,
        "src/Input/Worker/Mod.hs": worker_body,
    }
    if include_view:
        sources["src/Engine/Core/Capability/InputView.hs"] = view_body
    return sources


def test_input_boundary_clean_tree_has_no_violations():
    violations = audit_input_boundary(
        _input_sources(),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(violations == [],
           f"a tree where only the LuaThread module imports the full input "
           f"capability, the worker imports only the view, and the view "
           f"names neither private field must pass, got: {violations}")


def test_input_boundary_worker_importing_full_capability_rejected():
    violations = audit_input_boundary(
        _input_sources(worker_imports_full=True),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(_INPUT_WORKER in v and INPUT_CAPABILITY_MODULE in v
               for v in violations),
           "a non-LuaThread production module importing the full "
           "InputCapability must be rejected -- that record carries the "
           "barrier-token allocator and the onKeyDown current-key handoff, "
           "which SS5 makes LuaThread-private")


def test_input_boundary_non_owner_naming_allocator_rejected():
    violations = audit_input_boundary(
        _input_sources(worker_names_alloc=True),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(_INPUT_WORKER in v and "inputBarrierNextRef" in v
               for v in violations),
           "a production module outside INPUT_LUA_ONLY_FIELD_OWNERS naming "
           "inputBarrierNextRef/icInputBarrierNextRef must be rejected -- "
           "the input thread publishes the watermark, it never allocates")


def test_input_boundary_non_owner_naming_current_key_rejected():
    violations = audit_input_boundary(
        _input_sources(worker_names_keydown=True),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(_INPUT_WORKER in v and "currentKeyDownRef" in v
               for v in violations),
           "a production module outside INPUT_LUA_ONLY_FIELD_OWNERS naming "
           "currentKeyDownRef/icCurrentKeyDownRef must be rejected too -- "
           "both private fields are covered, not just the barrier one")


def test_input_boundary_watermark_is_not_confused_with_allocator():
    # The whole point of the split: `inputBarrierRef` (the watermark the
    # input thread publishes) must stay freely nameable, or the check
    # would forbid the very access the view exists to grant. A substring
    # -blind rule would flag it, since `inputBarrierNextRef` contains no
    # `inputBarrierRef` but a sloppy `inputBarrier` prefix match would
    # catch both.
    sources = _input_sources()
    sources["src/Input/Worker/Mod.hs"] += (
        "  w = modifyTVar' (ivInputBarrierRef view) (max tok)\n")
    violations = audit_input_boundary(
        sources, lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(violations == [],
           f"naming the barrier WATERMARK (inputBarrierRef/"
           f"ivInputBarrierRef) must never be a violation -- it is exactly "
           f"what the worker-safe view grants, got: {violations}")


def test_input_boundary_private_field_in_a_comment_is_not_a_violation():
    # Haddock on the view legitimately EXPLAINS why the fields are
    # absent; only live code counts, or the enforcement would forbid
    # documenting its own rule.
    sources = _input_sources()
    sources["src/Engine/Core/Capability/InputView.hs"] = (
        "-- | Deliberately carries no inputBarrierNextRef and no\n"
        "--   currentKeyDownRef field.\n"
        "module Engine.Core.Capability.InputView where\n"
        "  ivInputBarrierRef = inputBarrierRef env  -- not the allocator\n")
    violations = audit_input_boundary(
        sources, lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(violations == [],
           f"a Haddock/line comment mentioning either private field must "
           f"not count as naming it, got: {violations}")


def test_input_boundary_view_carrying_private_field_rejected():
    violations = audit_input_boundary(
        _input_sources(view_names_field=True),
        lua_only=frozenset({_LUA}),
        field_owners=frozenset({INPUT_VIEW_MODULE}))
    expect(any(INPUT_VIEW_MODULE in v for v in violations),
           "the worker-visible view must be rejected if it so much as "
           "names a LuaThread-private field -- even being listed as an "
           "owner must not buy it an exemption from the structural check")


def test_input_boundary_missing_view_module_rejected():
    violations = audit_input_boundary(
        _input_sources(include_view=False),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(INPUT_VIEW_MODULE in v and "missing" in v
               for v in violations),
           "deleting the worker-safe input view must fail loudly -- "
           "SS7.3's boundary has no enforcement without it")


def test_input_boundary_stale_lua_only_entry_rejected():
    violations = audit_input_boundary(
        _input_sources(lua_imports_full=False),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(_LUA in v and "stale" in v for v in violations),
           "a stale INPUT_LUA_ONLY_MODULES entry must be flagged, so the "
           "checked-in LuaThread set stays an exact mirror of the live one")


def test_input_boundary_stale_field_owner_rejected():
    violations = audit_input_boundary(
        _input_sources(),
        lua_only=frozenset({_LUA}),
        field_owners=frozenset({"Ghost.Owner"}))
    expect(any("Ghost.Owner" in v and "stale" in v for v in violations),
           "a stale INPUT_LUA_ONLY_FIELD_OWNERS entry must be flagged too")


def test_real_repo_input_boundary_holds():
    violations = audit_input_boundary(scan_production_sources(REPO_ROOT))
    expect(violations == [],
           f"the real repo must satisfy SS7.3's LuaThread input boundary "
           f"after issue #892's input-lua-transport migration, got: "
           f"{violations}")


def test_audit_against_the_real_repo():
    real_source = (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8")
    real_inventory = (REPO_ROOT / "docs" /
                       "engineenv_capability_inventory.md").read_text(encoding="utf-8")
    violations = audit(real_source, real_inventory)
    expect(violations == [],
           f"the real EngineEnv + the real inventory doc should have zero "
           f"violations, got: {violations}")
    live_fields = extract_record_fields(real_source, ENGINE_ENV_PATTERN)
    expect(len(live_fields) == 90,
           f"expected 90 live EngineEnv fields (issue #876's own count of 81, "
           f"plus #907's `windowPosRef`, #957's `tutorialRegistryRef`, "
           f"#913's `playerIntentGenRef`, #1693's "
           f"`framebufferMinimizeGenRef`, #1730's `enginePauseGenRef`, "
           f"#1712's `structureWallCatalogRef`, #1842's "
           f"`structureArtCatalogRef`, #1921's `sceneStatsRef` and "
           f"#2020's `maxImageDimensionRef`), "
           f"got {len(live_fields)}")


# ----- SS1's audited field total and field span (issue #1669) ----------
#
# Every rule the new prose check adds is mutation-tested here in BOTH
# directions: each rejects a crafted violating document, and the REAL
# inventory is accepted. Issue #1669 requirement 5 exists because
# hand-rolled prose validators have shipped here that rejected nothing
# (#704, #1128, #1309), so "the real file passes" on its own is not
# evidence that a rule is enforced.

# Three fields whose FIRST and LAST are what SS1's span claim names.
_FT_LIVE = ["fieldOne", "fieldTwo", "fieldThree"]


def _field_total_doc(total_body: str | None = None,
                     procedure_item: str | None = None,
                     *, total_blocks: int = 1,
                     scope_prefix: str = "",
                     scope_suffix: str = "",
                     trailing_section: str = "") -> str:
    """A minimal document shaped like the real one: SS1 opening with the
    marked block, and SS6.2 whose first numbered item is the audited
    assignment-method sentence."""
    if total_body is None:
        total_body = ("\n\n`Fake.hs` declares it with exactly **3** fields, "
                      "`fieldOne` through `fieldThree`, and every one of them "
                      f"has {ONE_ROW_PHRASE} below.\n\n")
    if procedure_item is None:
        procedure_item = (f"For each module, scan its source for every "
                          f"occurrence of one of the "
                          f"{PROCEDURE_ITEM_ANCHOR}.")
    parts = [f"# Fake inventory\n\n{SECTION_1_HEADING}\n\n"]
    if scope_prefix:
        parts.append(f"{scope_prefix}\n\n")
    for _ in range(total_blocks):
        parts.append(f"{FIELD_TOTAL_OPEN}{total_body}{FIELD_TOTAL_CLOSE}\n\n")
    if scope_suffix:
        parts.append(f"{scope_suffix}\n\n")
    parts.append("## 6. Boundary\n\nprose\n\n"
                 f"{SECTION_6_2_HEADING}\n\nintro\n\n"
                 f"1. {procedure_item}\n"
                 "2. A later step that legitimately counts 4 modules.\n\n")
    if trailing_section:
        parts.append(f"{trailing_section}\n\n")
    return "".join(parts)


def test_field_total_clean_fixture_accepted():
    violations = audit_field_total(_FT_LIVE, _field_total_doc())
    expect(violations == [],
           f"a document whose marked block states the live count and the "
           f"real first/last field should have zero violations, got: "
           f"{violations}")


def test_field_total_stale_count_rejected_while_rows_stay_synchronized():
    """Issue #1669 requirement 4, in the shape the issue review pinned:
    the recurrence that ESCAPES today is a live field change whose SS5
    row was added correctly while the SS1 prose stayed stale. A wholly
    unamended document already fails `audit`'s missing-row check, so
    that case proves nothing about this one."""
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldOne` through `fieldFour`, and every one of them "
                    f"has {ONE_ROW_PHRASE} below.\n\n"))
    grown = _FT_LIVE + ["fieldFour"]
    violations = audit_field_total(grown, doc)
    expect(any("states 3 fields" in v and "declares 4" in v
               for v in violations),
           f"a fourth live field whose SS1 total was not amended must be "
           f"rejected, got: {violations}")


def test_field_total_synchronized_rows_alone_do_not_save_a_stale_block():
    """The same recurrence proven end-to-end against the ROW audit:
    `audit` (rows vs live field set) passes on a document whose SS5 rows
    were updated for a new field, and only `audit_field_total` catches
    the stale SS1 total. Without this pairing, the new check could be
    passing for a reason the old one already covered."""
    grown_env = SYNTHETIC_ENGINE_ENV.replace(
        "  } deriving (Eq)",
        "  , fieldFour  ∷ IORef Bool\n  } deriving (Eq)")
    field_four_row = (
        "| `fieldFour` | boot-process | `Boot` (`src/Fake/Init.hs:8`) "
        "| `Boot` (`src/Fake/Init.hs:8`) | `IORef Bool` "
        "| `src/Fake/Init.hs:8` | None | — |\n")
    rows_doc = _doc(render_rows=FIELD_TWO_ROW + FIELD_THREE_ROW
                    + field_four_row)
    expect(audit(grown_env, rows_doc) == [],
           "the row audit must accept a new field whose SS5 row was added "
           "-- that is the case whose SS1 prose then goes stale unnoticed")
    live = extract_record_fields(grown_env, ENGINE_ENV_PATTERN)
    expect(len(live) == 4, f"fixture should now declare 4 fields, got {live}")
    stale = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldOne` through `fieldThree`, and every one of them "
                    f"has {ONE_ROW_PHRASE} below.\n\n"))
    expect(audit_field_total(live, stale) != [],
           "the field-total check must reject the SS1 block the row audit "
           "just accepted the document for")


def test_field_total_missing_block_rejected():
    doc = _field_total_doc(total_blocks=0)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("has no" in v and FIELD_TOTAL_OPEN in v for v in violations),
           f"deleting the marked block must be a violation, not a way to "
           f"turn the check off, got: {violations}")


def test_field_total_duplicate_block_rejected():
    doc = _field_total_doc(total_blocks=2)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("2 " in v and FIELD_TOTAL_OPEN in v for v in violations),
           f"two total blocks can disagree with each other, so a second "
           f"one must be rejected, got: {violations}")


def test_field_total_unclosed_block_rejected():
    doc = _field_total_doc().replace(FIELD_TOTAL_CLOSE, "", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("never closed" in v for v in violations),
           f"an unbalanced marker pair must be reported as malformed "
           f"markup, got: {violations}")


def test_field_total_reintroduced_line_anchor_rejected():
    """Issue #1669 requirement 3: the stale `State.hs:NNN` anchors are
    gone, and the one-number rule is what keeps them gone."""
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs:12` declares it with exactly **3** "
                    "fields, `fieldOne` through `fieldThree`, and every one "
                    f"of them has {ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("contains 2 numbers" in v for v in violations),
           f"a hand-written source line anchor inside the block is a "
           f"second number and must be rejected, got: {violations}")


def test_field_total_absent_number_rejected():
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares some fields, `fieldOne` through "
                    f"`fieldThree`, and every one of them has "
                    f"{ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("states no field total" in v for v in violations),
           f"a block that states no total at all is not a passing block, "
           f"got: {violations}")


def test_field_total_wrong_span_field_rejected():
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldOne` through `fieldTwo`, and every one of them "
                    f"has {ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("first and last field" in v for v in violations),
           f"a span claim naming a field that is not the record's last "
           f"must be rejected, got: {violations}")


def test_field_total_reversed_span_rejected():
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldThree` through `fieldOne`, and every one of them "
                    f"has {ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("first and last field" in v for v in violations),
           f"the span claim is ordered -- first THROUGH last -- so a "
           f"reversed pair must be rejected, got: {violations}")


def test_field_total_missing_one_row_contract_rejected():
    """Requirement 2: the one-row-per-field contract is the useful half
    of the sentence and must survive independently of the number."""
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` declares it with exactly **3** fields, "
                    "`fieldOne` through `fieldThree`.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("one-row-per-field contract" in v for v in violations),
           f"dropping the one-row-per-field contract must be rejected, "
           f"got: {violations}")


def test_field_total_section_references_are_not_counts():
    """The section sign is navigation, not a number: a block citing
    §5 and §7.3 alongside the one real total is still a
    one-number block."""
    doc = _field_total_doc(
        total_body=("\n\n`Fake.hs` (see §7.3) declares it with exactly "
                    "**3** fields, `fieldOne` through `fieldThree`, and every "
                    f"one of them has {ONE_ROW_PHRASE} below.\n\n"))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"section references must not be read as a second field total, "
           f"got: {violations}")


def test_procedure_item_reintroduced_total_rejected():
    """Requirement 1: SS1 and the SS6.2 procedure sentence must not be
    able to disagree. They cannot, because only SS1 may state a total --
    and this is the rule that keeps the second copy from coming back."""
    doc = _field_total_doc(
        procedure_item=(f"For each module, scan its source for every "
                        f"occurrence of one of the 83 "
                        f"{PROCEDURE_ITEM_ANCHOR}."))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("must state no field total" in v for v in violations),
           f"a field count reintroduced into SS6.2's procedure sentence "
           f"must be rejected, got: {violations}")


def test_procedure_item_agreeing_total_still_rejected():
    """Even a CORRECT second copy is rejected: two hand-maintained
    numbers is the defect, not one wrong one."""
    doc = _field_total_doc(
        procedure_item=(f"For each module, scan its source for every "
                        f"occurrence of one of the 3 "
                        f"{PROCEDURE_ITEM_ANCHOR}."))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("must state no field total" in v for v in violations),
           f"a second copy of the total is rejected even when it agrees "
           f"today, got: {violations}")


def test_procedure_item_total_in_its_tail_rejected():
    """The whole item is audited, not its opening clause: a total added
    after the recognizable phrase is still a second copy."""
    doc = _field_total_doc(
        procedure_item=(f"For each module, scan its source for every "
                        f"occurrence of one of the "
                        f"{PROCEDURE_ITEM_ANCHOR}, all 83 of them."))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("must state no field total" in v for v in violations),
           f"a total in the item's tail must be rejected, got: "
           f"{violations}")


def test_procedure_item_reworded_away_rejected():
    """The sentence is bound by its own wording as well as by position,
    so it cannot be rewritten past recognition (or displaced by a new
    item 1) while a stale total returns under a new phrasing."""
    doc = _field_total_doc(
        procedure_item="Tally each module's hits against the 83 names.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("no longer contains" in v for v in violations),
           f"an item 1 that is no longer the audited sentence must be "
           f"rejected, got: {violations}")


def test_procedure_item_displaced_by_a_new_first_item_rejected():
    doc = _field_total_doc()
    displaced = doc.replace(
        "1. For each module",
        "1. A newly inserted first step.\n2. For each module", 1)
    violations = audit_field_total(_FT_LIVE, displaced)
    expect(any("no longer contains" in v for v in violations),
           f"inserting a new item 1 must not move the audited sentence "
           f"out from under the check, got: {violations}")


def test_procedure_item_missing_section_rejected():
    doc = _field_total_doc().replace(SECTION_6_2_HEADING, "### 6.2 Gone", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("has no" in v and SECTION_6_2_HEADING in v
               for v in violations),
           f"renaming the procedure's section must be rejected, got: "
           f"{violations}")


def test_procedure_item_later_items_may_count_legitimately():
    """SS6.2's other steps legitimately state their own tallies; only
    the one audited sentence is held to no-number."""
    violations = audit_field_total(_FT_LIVE, _field_total_doc())
    expect(violations == [],
           f"item 2's legitimate '4 modules' count must not be flagged, "
           f"got: {violations}")


def test_scope_block_must_be_section_ones_first_content():
    """Same-section relocation: the pair still sits in SS1, but an
    unaudited paragraph -- carrying a stale total -- now stands in
    front of it."""
    doc = _field_total_doc(
        scope_prefix="The record has exactly 83 fields.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("not the first content" in v for v in violations),
           f"an unaudited paragraph placed ahead of the block must be "
           f"rejected, got: {violations}")


def test_scope_section_may_state_no_other_number():
    """The other half of same-section relocation: the pair stays first,
    and the stale copy is appended after it instead."""
    doc = _field_total_doc(
        scope_suffix="Historically the record had 83 of them.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "83" in v for v in violations),
           f"a second count later in SS1 must be rejected, got: "
           f"{violations}")


def test_scope_section_allows_code_spans_and_references():
    """The rule above must not flag what SS1 legitimately carries: a
    source reference inside a code span, a section reference, and an
    issue reference."""
    doc = _field_total_doc(
        scope_suffix="Out of scope: `EngineState` "
                     "(`src/Engine/Core/State.hs:446`), see §7.3 and "
                     "issue #1669.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"code spans and section/issue references must not read as "
           f"field counts, got: {violations}")


def test_scope_section_code_span_total_rejected():
    """Code font does not make a field total a citation: a bare
    `` `83` `` in SS1 is the stale count a reader sees, so it is
    rejected even though it sits inside backticks."""
    doc = _field_total_doc(scope_suffix="It has exactly `83` fields.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "83" in v for v in violations),
           f"a code-span field total in SS1 must be rejected, got: "
           f"{violations}")


def test_procedure_item_code_span_total_rejected():
    doc = _field_total_doc(
        procedure_item=(f"For each module, scan its source for every "
                        f"occurrence of one of the `83` "
                        f"{PROCEDURE_ITEM_ANCHOR}."))
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("must state no field total" in v for v in violations),
           f"a code-span field total in the procedure sentence must be "
           f"rejected, got: {violations}")


def test_source_location_spans_stay_exempt():
    """The narrow exemption still has to cover what the document really
    carries: a path with a line anchor, and a path with a line range."""
    doc = _field_total_doc(
        scope_suffix="See `src/Engine/Core/State.hs:446` and "
                     "`docs/persistence_state_inventory.md:12-20`.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"source-location code spans must stay exempt, got: "
           f"{violations}")


def test_stray_engineenv_total_anywhere_rejected():
    """The document-wide backstop: the one unambiguous reintroduction
    shape is rejected wherever it appears, not only in the two governed
    places."""
    doc = _field_total_doc(
        trailing_section="## 9. Appendix\n\nA reminder that there are `83` "
                         "`EngineEnv` fields in total.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "EngineEnv" in v
               for v in violations),
           f"an EngineEnv field total stated in an unrelated section must "
           f"be rejected, got: {violations}")


def test_bare_field_counts_elsewhere_are_not_flagged():
    """The backstop is deliberately narrow: SS5's capability groups and
    SS7's roadmap state their own record sizes, and a rule that flagged
    those is a rule maintainers route around."""
    doc = _field_total_doc(
        trailing_section="## 9. Appendix\n\nThe render capability covers "
                         "21 fields; content-registries is a 7-field "
                         "record.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"a capability record's own field count must not be mistaken "
           f"for the EngineEnv total, got: {violations}")


def test_field_total_block_outside_section_one_rejected():
    """The escape the section binding closes: both marker pairs still
    exist, still well-formed and self-consistent, but they were lifted
    out of the prose they govern -- which is then free to carry a stale
    hand-maintained total again."""
    doc = _field_total_doc(trailing_section="## 9. Appendix")
    spans, _ = extract_marked_spans(doc, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    block = doc[spans[0].start:spans[0].end]
    moved = doc.replace(block, "It has exactly 83 fields.", 1) + block + "\n"
    violations = audit_field_total(_FT_LIVE, moved)
    expect(any("is not inside" in v and SECTION_1_HEADING in v
               for v in violations),
           f"a total block relocated out of the scope section must be "
           f"rejected, got: {violations}")


def test_field_total_renamed_section_heading_rejected():
    """Renaming the section is the other half of relocating the block:
    the pair stays put and the heading moves away from it."""
    doc = _field_total_doc().replace(SECTION_1_HEADING, "## 1. Purpose", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("has no" in v and SECTION_1_HEADING in v
               for v in violations),
           f"renaming the governed section must be rejected, got: "
           f"{violations}")


def test_fenced_heading_does_not_end_the_scope_section():
    """A fenced code block containing a heading-shaped line must not
    end SS1: Markdown renders everything after it inside SS1 still, so
    a stale total placed there would be inside the document and outside
    the audit."""
    doc = _field_total_doc(
        scope_suffix="```\n## example\n```\n\nIt has exactly 83 fields.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "83" in v for v in violations),
           f"prose hidden behind a fenced pseudo-heading must still be "
           f"audited, got: {violations}")


def test_fenced_heading_does_not_end_the_procedure_section():
    doc = _field_total_doc().replace(
        "1. For each module",
        "```\n### 6.9 not a heading\n```\n\n1. For each module", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"a fenced pseudo-heading before the procedure item must not "
           f"hide the item from the check, got: {violations}")


def test_fenced_scope_heading_does_not_start_the_section():
    """The same rule in the other role: a fenced `## 1. Scope` must not
    be mistaken for the section's start, which would put the real
    section's prose outside the audited range."""
    doc = _field_total_doc().replace(
        "# Fake inventory\n",
        f"# Fake inventory\n\n```\n{SECTION_1_HEADING}\n```\n", 1)
    violations = audit_field_total(_FT_LIVE, doc)
    expect(violations == [],
           f"a fenced copy of the scope heading must be ignored, got: "
           f"{violations}")


def test_tilde_fences_and_longer_closers_are_handled():
    """Tilde fences count too, and a closing fence must be at least as
    long as its opener -- a shorter run inside the block does not end
    it."""
    doc = _field_total_doc(
        scope_suffix="~~~~\n## example\n~~~\nstill fenced\n~~~~\n\n"
                     "It has exactly 83 fields.")
    violations = audit_field_total(_FT_LIVE, doc)
    expect(any("outside its" in v and "83" in v for v in violations),
           f"tilde fences must be tracked with the same length rule, "
           f"got: {violations}")


def test_real_inventory_fenced_heading_escape_rejected():
    """The same escape, on the real document."""
    real = (REPO_ROOT / "docs" /
            "engineenv_capability_inventory.md").read_text(encoding="utf-8")
    live = extract_record_fields(
        (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8"),
        ENGINE_ENV_PATTERN)
    spans, _ = extract_marked_spans(real, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    escaped = (real[:spans[0].end]
               + "\n\n```\n## example\n```\n\nIt has exactly 83 fields.\n"
               + real[spans[0].end:])
    expect(any("outside its" in v for v in audit_field_total(live, escaped)),
           "a fenced pseudo-heading must not carve a stale total out of "
           "the real SS1")


def test_section_bounds_stops_at_the_next_peer_heading():
    doc = _field_total_doc()
    bounds = section_bounds(doc, SECTION_1_HEADING, ("## ",))
    expect(bounds is not None, "SS1's bounds must be found")
    start, end = bounds
    expect("## 6. Boundary" not in doc[start:end],
           "SS1's body must stop at the next top-level heading")
    expect(FIELD_TOTAL_OPEN in doc[start:end],
           "SS1's body must contain the total block it governs")


def test_section_bounds_keeps_subsections_inside_a_top_level_section():
    """`"## "` must not match `"### "` -- otherwise every top-level
    section would end at its own first subsection."""
    doc = "## 6. Boundary\n\nintro\n\n### 6.2 Sub\n\ntail\n\n## 7. Next\n"
    bounds = section_bounds(doc, "## 6. Boundary", ("## ",))
    expect(bounds is not None, "the section must be found")
    start, end = bounds
    expect("### 6.2 Sub" in doc[start:end] and "tail" in doc[start:end],
           f"a subsection must stay inside its parent section, got: "
           f"{doc[start:end]!r}")
    expect("## 7. Next" not in doc[start:end],
           "the next peer heading must end the section")


def test_field_total_against_the_real_repo():
    real_source = (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8")
    real_inventory = (REPO_ROOT / "docs" /
                      "engineenv_capability_inventory.md").read_text(
                          encoding="utf-8")
    live_fields = extract_record_fields(real_source, ENGINE_ENV_PATTERN)
    violations = audit_field_total(live_fields, real_inventory)
    expect(violations == [],
           f"the real inventory's SS1 block must state the real live "
           f"count and span, got: {violations}")
    spans, marker_violations = extract_marked_spans(
        real_inventory, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    expect(marker_violations == [] and len(spans) == 1,
           f"the real document must carry exactly one well-formed total "
           f"block, got {len(spans)} and {marker_violations}")
    body = spans[0].body
    stale_body = body.replace(str(len(live_fields)),
                              str(len(live_fields) - 1), 1)
    expect(stale_body != body,
           "the real block must actually contain the live count for this "
           "mutation to mean anything")
    stale = real_inventory.replace(body, stale_body, 1)
    expect(audit_field_total(live_fields, stale) != [],
           "the real inventory with its own block's total decremented by "
           "one must be rejected -- proving the check reads THIS "
           "document's block, not only synthetic fixtures")
    anchored_body = body.replace(
        "`src/Engine/Core/State.hs`", "`src/Engine/Core/State.hs:70`", 1)
    expect(anchored_body != body,
           "the real block must name the source file for this mutation to "
           "mean anything")
    anchored = real_inventory.replace(body, anchored_body, 1)
    expect(audit_field_total(live_fields, anchored) != [],
           "a hand-written source line anchor put back into the real "
           "block must be rejected (issue #1669 requirement 3)")

    # Relocation: lift the real pair out of SS1 and re-append it,
    # unchanged and self-consistent, at the very end of the document,
    # leaving a stale hand-maintained total behind in SS1's prose. Every
    # between-the-markers rule still passes on the moved block; only the
    # section binding catches it.
    whole_block = real_inventory[spans[0].start:spans[0].end]
    relocated = (real_inventory.replace(
        whole_block,
        "`src/Engine/Core/State.hs` declares exactly 83 fields.", 1)
        + "\n\n## 9. Appendix\n\n" + whole_block + "\n")
    expect(any("is not inside" in v for v in
               audit_field_total(live_fields, relocated)),
           "moving the real total block out of SS1 must be rejected, or "
           "SS1's prose could carry a stale total again with the markers "
           "parked somewhere inert")

    # Same-section relocation on the REAL document: the block stays in
    # SS1 but an unaudited paragraph carrying a stale total is placed
    # ahead of it.
    shadowed = real_inventory.replace(
        f"{SECTION_1_HEADING}\n",
        f"{SECTION_1_HEADING}\n\nThe record has exactly 83 fields.\n", 1)
    expect(any("not the first content" in v for v in
               audit_field_total(live_fields, shadowed)),
           "an unaudited scope paragraph placed ahead of the real block "
           "must be rejected")

    # The real SS6.2 procedure sentence, given its old total back --
    # plain, and again wearing code font.
    for restored in ("83", "`83`"):
        procedure = real_inventory.replace(
            f"one of the\n   {PROCEDURE_ITEM_ANCHOR}",
            f"one of the\n   {restored} {PROCEDURE_ITEM_ANCHOR}", 1)
        expect(procedure != real_inventory,
               f"the real procedure sentence must be found for the "
               f"{restored} mutation to mean anything")
        expect(any("must state no field total" in v or "outside its" in v
                   for v in audit_field_total(live_fields, procedure)),
               f"restoring the second copy of the total ({restored}) in "
               f"the real SS6.2 procedure sentence must be rejected")

    # And the real SS1, given a code-font copy after its block.
    spans2, _ = extract_marked_spans(
        real_inventory, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    tail = real_inventory[:spans2[0].end] + \
        "\n\nThe record has `83` fields.\n" + \
        real_inventory[spans2[0].end:]
    expect(any("outside its" in v for v in
               audit_field_total(live_fields, tail)),
           "a code-font second count after the real block must be "
           "rejected")


def test_real_repo_end_state():
    """Issue #899 (E8) requirement 7: the epic's END STATE, asserted
    against the REAL repository rather than a fixture.

    Every other real-repo test above asks "do the checked-in constants
    and the live source still agree?" -- which stays true no matter how
    large the temporary ceiling is. This one asserts the ceiling is
    GONE: the boundary is permanent-only, and the record set is
    complete. It is deliberately a set of narrow, independently
    diagnosable assertions rather than one `audit() == []`, so a
    regression names which part of the end state slipped."""
    real_inventory = (REPO_ROOT / "docs" /
                       "engineenv_capability_inventory.md").read_text(encoding="utf-8")
    sources = scan_production_sources(REPO_ROOT)
    unrestricted = scan_production_unrestricted_importers(REPO_ROOT)

    # 1. The live unrestricted importer set IS the permanent allowlist --
    #    checked as set equality in both directions, so neither a new
    #    full-access module nor a stale allowlist entry passes.
    expect(unrestricted == set(PERMANENT_IMPORTERS),
           f"the live unrestricted production importer set must equal "
           f"PERMANENT_IMPORTERS exactly (SS6.1's permanent-only "
           f"boundary); extra: {sorted(unrestricted - set(PERMANENT_IMPORTERS))}, "
           f"missing: {sorted(set(PERMANENT_IMPORTERS) - unrestricted)}")

    # 2. Every TEMPORARY_CEILING value is empty -- the flip itself.
    nonempty = {cap: sorted(mods) for cap, mods in TEMPORARY_CEILING.items()
                if mods}
    expect(nonempty == {},
           f"every TEMPORARY_CEILING value must be empty after #899's "
           f"flip -- there is no legal path left for a production module "
           f"to take unrestricted access, got: {nonempty}")

    # 3. SS6.2's documented table still carries all eight capability keys,
    #    each with an EMPTY module set. The keys matter as much as the
    #    emptiness: `audit_ratchet`'s doc/ceiling cross-check iterates the
    #    UNION of both key sets, so a dropped row would silently stop
    #    cross-checking that capability rather than fail.
    doc_temporary = parse_temporary_boundary(real_inventory)
    expect(set(doc_temporary) == set(CAPABILITIES),
           f"SS6.2's table must have exactly the eight CAPABILITIES keys, "
           f"got: {sorted(doc_temporary)}")
    expect(set(TEMPORARY_CEILING) == set(CAPABILITIES),
           f"TEMPORARY_CEILING must retain all eight CAPABILITIES keys "
           f"mapped to empty frozensets, not be reduced to {{}} -- "
           f"otherwise this test's own emptiness assertions go vacuous; "
           f"got: {sorted(TEMPORARY_CEILING)}")
    documented_nonempty = {cap: sorted(mods)
                           for cap, mods in doc_temporary.items() if mods}
    expect(documented_nonempty == {},
           f"every SS6.2 row must document an empty module set, got: "
           f"{documented_nonempty}")

    # 4. SS6.1's DOCUMENTED set matches the permanent constants, with a
    #    real justification on every row.
    permanent_violations = audit_permanent_boundary(real_inventory)
    expect(permanent_violations == [],
           f"SS6.1's documented permanent set must equal PERMANENT_DEFINER "
           f"+ PERMANENT_IMPORTERS, with a non-placeholder Category and "
           f"Reason on every row, got: {permanent_violations}")

    # 5. The E8 record exists, is listed in synarchy.cabal, and its
    #    projection binds exactly the five documented handles from their
    #    matching EngineEnv accessors. (Runtime container identity is
    #    proven separately by Test.Headless.Capability.SaveLoad's
    #    sameContainer assertions -- a Python audit cannot observe it.)
    cabal_text = (REPO_ROOT / "synarchy.cabal").read_text(encoding="utf-8")
    save_load_violations = audit_save_load_projection(sources, cabal_text)
    expect(save_load_violations == [],
           f"`{SAVE_LOAD_CAPABILITY_MODULE}` must exist, be listed in "
           f"synarchy.cabal, and alias all five "
           f"`save-load-coordination` handles, got: {save_load_violations}")

    # 6. ...and the five handles it aliases are exactly the five fields
    #    SS5's `save-load-coordination` table classifies -- so the record
    #    and the inventory cannot drift apart.
    inventory_rows, _ = parse_inventory(real_inventory)
    inventory_fields = {row.field for row in inventory_rows
                        if row.capability == "save-load-coordination"}
    expect(inventory_fields == set(SAVE_LOAD_FIELD_MAP.values()),
           f"SAVE_LOAD_FIELD_MAP must name exactly SS5's "
           f"`save-load-coordination` fields, got inventory: "
           f"{sorted(inventory_fields)} vs map: "
           f"{sorted(SAVE_LOAD_FIELD_MAP.values())}")

    # 7. World.Thread -- requirement 2's named real consumer -- must
    #    actually adopt the record: its `Engine.Core.State` import may
    #    name NO save-load-coordination accessor, and its barrier access
    #    must go through the capability. Without this, #899 could ship an
    #    unadopted twelfth-plus record, which E1's convention forbids.
    world_thread = sources.get("src/World/Thread.hs", "")
    expect(bool(world_thread),
           "src/World/Thread.hs must exist in the production sources")
    state_import = "".join(
        chunk for chunk in _import_chunks(_strip_haskell_comments(world_thread))
        if chunk.startswith("import Engine.Core.State"))
    leaked = sorted(f for f in SAVE_LOAD_FIELD_MAP.values()
                    if f in state_import)
    expect(leaked == [],
           f"`World.Thread`'s Engine.Core.State import must name no "
           f"save-load-coordination field accessor after #899 -- it goes "
           f"through SaveLoadCapability now; got: {leaked}")
    expect("slSaveBarrierRef" in world_thread
           and "toSaveLoadCapability" in world_thread,
           "`World.Thread` must reach the save barrier through "
           "`slSaveBarrierRef (toSaveLoadCapability env)` -- E1 forbids "
           "introducing a capability record with no real consumer")


# ----- SS5 writing-module map (issue #1892, CMA-1) ----------------------
#
# The map pins each `EngineEnv` field's DIRECT writing modules, checked
# in both directions. These fixtures exercise `scan_capability_writes`
# and `audit_writer_modules` against a synthetic three-field production
# tree -- never by editing a real module or the real map -- so each of
# the scan's three honesty gates (import scope, local shadowing, and
# "must head a mutation primitive's first argument") gets a case that
# fails without it.

_WRITER_FIELDS = ["fieldOne", "fieldTwo", "fieldThree"]
_WRITER_PERMANENT = frozenset({"Permanent.Mod"})

_FAKE_CAPABILITY = """\
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
_DECLARED_WRITER = """\
module Consumer.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldTwo)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

bumpCapability ∷ EngineEnv → IO ()
bumpCapability env = writeIORef (fkFieldOne (toFakeCapability env)) 1

bumpRaw ∷ EngineEnv → IO ()
bumpRaw env = writeIORef (fieldTwo env) 2
"""

# Same write, from a module the map does not list.
_UNDECLARED_WRITER = """\
module Interloper.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldTwo)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

sneakCapability ∷ EngineEnv → IO ()
sneakCapability env = modifyIORef' (fkFieldOne (toFakeCapability env)) (+ 1)

sneakRaw ∷ EngineEnv → IO ()
sneakRaw env = writeIORef (fieldTwo env) 9
"""

# SS6.1's cohort (D-4): writes the same field, must not be reported and
# must not be admitted into the map.
_PERMANENT_WRITER = """\
module Permanent.Mod where

import Data.IORef

import Engine.Core.State

seedEverything ∷ EngineEnv → IO ()
seedEverything env = writeIORef (fieldOne env) 0
"""

# The three false-positive traps, one per honesty gate.
_TRAP_MODULE = """\
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

# `Engine.Core.State` is imported for the TYPE only, so an identically
# named local is not the field -- the live shape of
# `src/Unit/Thread/Movement.hs`'s `utsRef` parameter.
_TYPE_ONLY_IMPORTER = """\
module Narrow.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)

tick ∷ EngineEnv → IORef Int → IO ()
tick _ fieldOne = writeIORef fieldOne 5
"""

# A module-local helper that shares an accessor's name and is APPLIED
# exactly like the real thing, so nothing about the write's SHAPE
# distinguishes it. `Engine.Core.State` is imported for the `EngineEnv`
# type alone, which is the only reason this is not the field -- the
# import-scope gate on its own.
_LOCAL_HOMONYM = """\
module Homonym.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)

fieldOne ∷ EngineEnv → IORef Int
fieldOne _ = error "this module's own helper, not the accessor"

tick ∷ EngineEnv → IO ()
tick env = writeIORef (fieldOne env) 5
"""

# Qualified spellings, through the module's own name and through an
# `as` alias. Both name the field exactly as the bare spelling does.
_QUALIFIED_WRITER = """\
module Qualified.Mod where

import Data.IORef

import qualified Engine.Core.State as State
import qualified Engine.Core.Capability.Fake as Cap

bumpRaw ∷ State.EngineEnv → IO ()
bumpRaw env = writeIORef (State.fieldTwo env) 4

bumpCapability ∷ State.EngineEnv → IO ()
bumpCapability env =
    writeIORef (Cap.fkFieldOne (Cap.toFakeCapability env)) 5
"""

# The two ways a qualified spelling must NOT resolve: a prefix this
# module establishes for a different module, and the aliased module's
# own name, which the alias replaces.
_MISQUALIFIED = """\
module Misqualified.Mod where

import Data.IORef

import qualified Engine.Core.State as State
import qualified Data.Map as Other

wrongModule ∷ State.EngineEnv → IO ()
wrongModule env = writeIORef (Other.fieldTwo env) 6

replacedName ∷ State.EngineEnv → IO ()
replacedName env = writeIORef (Engine.Core.State.fieldTwo env) 7
"""

# A mutation primitive is itself under a qualifier too. Missing this
# spelling would let an undeclared writer through in silence.
_QUALIFIED_PRIMITIVE = """\
module QualPrim.Mod where

import Data.IORef

import qualified Data.IORef as Ref
import Engine.Core.State (EngineEnv, fieldOne)

bump ∷ EngineEnv → IO ()
bump env = Ref.writeIORef (fieldOne env) 1
"""

# `qualified` removes the UNQUALIFIED spelling from scope, so this
# module's own `fieldOne` helper is not the field even though the owner
# is imported -- while `State.fieldTwo` in the same module is.
_QUALIFIED_ONLY = """\
module QualOnly.Mod where

import Data.IORef

import qualified Engine.Core.State as State

fieldOne ∷ State.EngineEnv → IORef Int
fieldOne _ = error "this module's own helper, not the accessor"

viaHomonym ∷ State.EngineEnv → IO ()
viaHomonym env = writeIORef (fieldOne env) 2

viaQualifier ∷ State.EngineEnv → IO ()
viaQualifier env = writeIORef (State.fieldTwo env) 3
"""

# A bare first argument: never the accessor (it projects out of a
# handle, so it cannot BE the `IORef`), and for a capability accessor
# it surfaces in the residue rather than being silently dropped.
_BARE_ARGUMENT = """\
module Bare.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldTwo)
import Engine.Core.Capability.Fake (FakeCapability(..))

viaWildcard ∷ FakeCapability → Int → IO ()
viaWildcard FakeCapability{..} newValue = writeIORef fkFieldOne newValue

viaParenthesizedLocal ∷ IORef Text → IO ()
viaParenthesizedLocal fieldTwo = writeIORef (fieldTwo) 9
"""

# `hiding` brings in everything EXCEPT the listed names, which is how a
# module legally defines its own `fieldOne` while importing the rest.
_HIDING_IMPORTER = """\
module Hiding.Mod where

import Data.IORef

import Engine.Core.State hiding (fieldOne)

fieldOne ∷ EngineEnv → IORef Int
fieldOne _ = error "this module's own helper, not the accessor"

shadowed ∷ EngineEnv → IO ()
shadowed env = writeIORef (fieldOne env) 1

visible ∷ EngineEnv → IO ()
visible env = writeIORef (fieldTwo env) 2
"""

# Any two-argument function may be written infix, so a backticked
# primitive is the same direct write with its arguments swapped --
# qualified spelling included.
_INFIX_WRITER = """\
module Infix.Mod where

import Data.IORef

import qualified Data.IORef as Ref
import Engine.Core.State (EngineEnv, fieldOne)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

raw ∷ EngineEnv → IO ()
raw env = (fieldOne env) `writeIORef` 1

viaCapability ∷ EngineEnv → IO ()
viaCapability env = (fkFieldTwo (toFakeCapability env)) `Ref.writeIORef` 2
"""

# A backtick operator binds looser than application, so an infix
# operand needs no parentheses at all.
_BARE_OPERAND = """\
module BareOperand.Mod where

import Data.IORef

import qualified Data.IORef as Ref
import Engine.Core.State (EngineEnv, fieldThree)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

raw ∷ EngineEnv → IO ()
raw env = fieldThree env `writeIORef` 1

viaCapability ∷ EngineEnv → IO ()
viaCapability env = fkFieldOne (toFakeCapability env) `Ref.writeIORef` 2
"""

# Redundant parentheses change nothing -- around the primitive in a
# prefix application, and around an infix operand. But a parenthesized
# primitive that something else is APPLYING to is an argument being
# passed on, not a write here.
_PARENTHESIZED = """\
module Parens.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

aroundThePrimitive ∷ EngineEnv → IO ()
aroundThePrimitive env = (writeIORef) (fieldOne env) 1

aroundTheOperand ∷ EngineEnv → IO ()
aroundTheOperand env = ((fieldTwo env)) `writeIORef` 2

passedOnward ∷ EngineEnv → IO ()
passedOnward env = withLogging (writeIORef) (fieldThree env) 3
"""

# Parentheses around the ACCESSOR itself, prefix and infix.
_PARENTHESIZED_ACCESSOR = """\
module ParenAccessor.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

prefix ∷ EngineEnv → IO ()
prefix env = writeIORef ((fieldOne) env) 1

infixForm ∷ EngineEnv → IO ()
infixForm env = ((fieldTwo) env) `writeIORef` 2

unapplied ∷ IORef Int → IO ()
unapplied _ = writeIORef (fieldThree) 3
"""

# A visible type application is not the value argument. Legal under
# GHC2024's default `TypeApplications`, and invisible to a scan that
# expects the accessor immediately after the primitive.
_TYPE_APPLICATION = """\
module TypeApp.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

simple ∷ EngineEnv → IO ()
simple env = writeIORef @Int (fieldOne env) 1

grouped ∷ EngineEnv → IO ()
grouped env = writeIORef @(IORef Text) (fieldTwo env) 2

-- The type application sits INSIDE parentheses around the primitive.
insideParentheses ∷ EngineEnv → IO ()
insideParentheses env = (writeIORef @Int) (fieldThree env) 3
"""

# `$!` is the strict sibling of `$` and groups its argument the same
# way; the tokenizer splits it into two punctuation tokens.
_STRICT_APPLICATION = """\
module Strict.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

strict ∷ EngineEnv → IO ()
strict env = (writeIORef $! fieldOne env) 1

lazyControl ∷ EngineEnv → IO ()
lazyControl env = (writeIORef $ fieldTwo env) 2
"""

# All six recognized mutation primitives, so no spelling can leave the
# closed set unnoticed.
_ALL_PRIMITIVES = """\
module AllPrims.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

a, b, c, d, e, f ∷ EngineEnv → IO ()
a env = writeIORef (fieldOne env) 1
b env = atomicWriteIORef (fieldOne env) 2
c env = modifyIORef (fieldOne env) (+ 1)
d env = modifyIORef' (fieldOne env) (+ 1)
e env = atomicModifyIORef (fieldOne env) (\\n → (n, ()))
f env = atomicModifyIORef' (fieldOne env) (\\n → (n, ()))
"""

# A BARE import grants everything the module exports; the remaining
# import shape the scan must honour at scan level.
_BARE_IMPORTER = """\
module BareImport.Mod where

import Data.IORef

import Engine.Core.State

bump ∷ EngineEnv → IO ()
bump env = writeIORef (fieldOne env) 1
"""

# An argument is plainly being formed and its head is not an
# identifier: requirement 6's blocking case. Beside it, two shapes that
# form NO argument here and are therefore ordinary non-writes.
_UNREADABLE = """\
module Unreadable.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

unboxed ∷ EngineEnv → IO ()
unboxed env = writeIORef (# fieldOne env #) 1
"""

# `OverloadedRecordDot` field access. The scan cannot read it as an
# accessor application, and taking `env` as the argument head would
# quietly make this a non-write -- so it is unclassifiable, and
# requirement 6 reports it. Spaced composition is ordinary code and
# must NOT be swept up with it.
_RECORD_DOT = """\
module RecordDot.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

viaDot ∷ EngineEnv → IO ()
viaDot env = modifyIORef' (env.fieldOne) id
"""

_COMPOSED_ARGUMENT = """\
module Composed.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

viaComposition ∷ EngineEnv → IO ()
viaComposition env = modifyIORef' (chooseRef . pick $ env) id
"""

# A primitive handed to another function UNPARENTHESIZED is still
# being handed on: the tokens after it are that function's arguments,
# not its own. The capability half must also keep its residue entry,
# which a phantom inline use would swallow.
_UNPARENTHESIZED_VALUE = """\
module PassedOn.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

raw ∷ EngineEnv → IO ()
raw env = withLogging writeIORef (fieldOne env) 1

viaCapability ∷ EngineEnv → IO ()
viaCapability env =
    withLogging writeIORef (fkFieldTwo (toFakeCapability env)) 2
"""

# A keyword lexes as an identifier but applies to nothing, so a
# primitive after one IS in head position -- the shape at
# `src/Unit/Thread/Movement/Climb.hs:86`.
# The same hand-off spread over lines. A newline does not end an
# application; the continuation is indented past the line that opened
# it, and that is what distinguishes it from a sibling statement.
_MULTILINE_VALUE = """\
module MultiPassed.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

raw ∷ EngineEnv → IO ()
raw env = withLogging
    writeIORef
    (fieldOne env)
    1

viaCapability ∷ EngineEnv → IO ()
viaCapability env = withLogging
    writeIORef
    (fkFieldTwo (toFakeCapability env))
    2
"""

# Sibling statements at the same column are NOT continuations, however
# the previous one ended -- and it very often ends in `)`.
_SIBLING_STATEMENTS = """\
module Siblings.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldThree)

run ∷ EngineEnv → IO ()
run env = do
    pure ()
    writeIORef (fieldThree env) 1
"""

# An operator SECTION applied prefix. `($)` applies its arguments and
# `(.)` composes them -- opposite consequences for whether a write
# happens here, and a textual scan cannot tell which. Unreadable, so
# it blocks rather than passing silently.
_OPERATOR_SECTION = """\
module Section.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

applied ∷ EngineEnv → IO ()
applied env = ($) writeIORef (fieldOne env) 1
"""

# A parenthesized group holding a real expression is not a section, and
# is the ordinary passed-on case.
_PARENTHESIZED_CALLEE = """\
module Callee.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldTwo)

handedOn ∷ EngineEnv → IO ()
handedOn env = (chooseLogger env) writeIORef (fieldTwo env) 2
"""

_AFTER_KEYWORD = """\
module AfterKeyword.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldThree)

pick ∷ EngineEnv → Bool → IO ()
pick env done =
    if done
        then pure ()
        else writeIORef (fieldThree env) 1
"""

_PRIMITIVE_AS_VALUE = """\
module AsValue.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

handedOn ∷ [IORef Int] → IO ()
handedOn refs = mapM_ (writeIORef) refs
"""

# A module-local `writeIORef` is a different function, and calling it
# mutates no `IORef`. Attributing its argument would invent a write out
# of code that performs none.
_LOCAL_PRIMITIVE = """\
module LocalPrim.Mod where

import Engine.Core.State (EngineEnv, fieldOne)

writeIORef ∷ (EngineEnv → IORef Int) → Int → IO ()
writeIORef _ _ = pure ()

use ∷ EngineEnv → IO ()
use env = writeIORef (fieldOne env) 1
"""

# The same, qualified: `Other.writeIORef` is whatever `Other` exports,
# not `Data.IORef`'s. The control beside it proves the resolution is
# not simply refusing every qualified spelling.
_QUALIFIED_HOMONYM = """\
module QualHomonym.Mod where

import qualified Vendor.Refs as Other
import qualified Data.IORef as Ref
import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

foreign ∷ EngineEnv → IO ()
foreign env = Other.writeIORef (fieldOne env) 1

genuine ∷ EngineEnv → IO ()
genuine env = Ref.writeIORef (fieldTwo env) 2
"""

# The same two writes, but importing the accessors BY NAME rather than
# through `FakeCapability(..)`: the import list itself then contains the
# accessor tokens, so it is what proves an import declaration is not a
# use. `fkFieldTwo` is imported and never used.
_EXPLICIT_IMPORTER = """\
module Explicit.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Fake
  ( fkFieldOne
  , fkFieldTwo
  , toFakeCapability
  )

bump ∷ EngineEnv → IO ()
bump env = writeIORef (fkFieldOne (toFakeCapability env)) 1
"""

# One expression, four lines: nothing here is findable by a line-wise
# scan, matching the real
# `Engine.Scripting.Lua.API.StructureArt`/`Engine.Input.Thread.Dispatch`
# multiline mutations.
_MULTILINE_WRITER = """\
module Multi.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

bump ∷ EngineEnv → IO ()
bump env =
    atomicModifyIORef'
        (fkFieldOne
            (toFakeCapability env))
        (\\n → (n + 1, ()))
"""


# Two capability records exporting the SAME selector, projecting
# different fields. A consumer imports one of them qualified, so its
# imports are what say which `sharedRef` it means.
_ALPHA_CAPABILITY = """\
module Engine.Core.Capability.Alpha
  ( AlphaCapability(..)
  , toAlphaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne)

data AlphaCapability = AlphaCapability
  { sharedRef ∷ IORef Int
  }

toAlphaCapability ∷ EngineEnv → AlphaCapability
toAlphaCapability env = AlphaCapability
  { sharedRef = fieldOne env
  }
"""

_BETA_CAPABILITY = """\
module Engine.Core.Capability.Beta
  ( BetaCapability(..)
  , toBetaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldTwo)

data BetaCapability = BetaCapability
  { sharedRef ∷ IORef Text
  }

toBetaCapability ∷ EngineEnv → BetaCapability
toBetaCapability env = BetaCapability
  { sharedRef = fieldTwo env
  }
"""

# A capability module may import `Engine.Core.State` under an alias and
# project through the QUALIFIED accessor. Missing that spelling drops
# the record's accessors from the map entirely, and every write made
# through them with it.
_QUALIFIED_PROJECTION = """\
module Engine.Core.Capability.Gamma
  ( GammaCapability(..)
  , toGammaCapability
  ) where

import qualified Engine.Core.State as State

data GammaCapability = GammaCapability
  { gmFieldThree ∷ IORef Int
  }

toGammaCapability ∷ State.EngineEnv → GammaCapability
toGammaCapability env = GammaCapability
  { gmFieldThree = State.fieldThree env
  }
"""

# SS2.1's abstract-wrapper extension (issue #1896): a view field is
# `field = toReadOnlyRef (accessor env)`. It aliases the very same live
# handle, so it must canonicalize exactly as the bare form does.
_WRAPPED_PROJECTION = """\
module Engine.Core.Capability.DeltaView
  ( DeltaViewCapability(..)
  , toDeltaViewCapability
  ) where

import Engine.Core.ReadOnlyRef (ReadOnlyRef, toReadOnlyRef)
import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data DeltaViewCapability = DeltaViewCapability
  { dvFieldOne ∷ ReadOnlyRef Int
  , dvFieldTwo ∷ ReadOnlyRef Text
  }

toDeltaViewCapability ∷ EngineEnv → DeltaViewCapability
toDeltaViewCapability env = DeltaViewCapability
  { dvFieldOne = toReadOnlyRef (fieldOne env)
  , dvFieldTwo = snapshotOf (fieldTwo env)
  }
"""

# Issue #2059: the SAME two bindings, spelled with semantically inert
# grouping. Haskell reads `(fieldOne env)` and `(fieldTwo) env` exactly
# as their ungrouped forms; before #2059 the surface regexes read
# NEITHER, so both selectors were absent from the accessor map and the
# consumer write below was filed as `other` while the gate exited 0.
_GROUPED_PROJECTION = """\
module Engine.Core.Capability.Epsilon
  ( EpsilonCapability(..)
  , toEpsilonCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data EpsilonCapability = EpsilonCapability
  { epFieldOne ∷ IORef Int
  , epFieldTwo ∷ IORef Text
  }

toEpsilonCapability ∷ EngineEnv → EpsilonCapability
toEpsilonCapability env = EpsilonCapability
  { epFieldOne = (fieldOne env)
  , epFieldTwo = (fieldTwo) env
  }
"""

# The write that must be attributed through the grouped projection --
# the module is in no field's writing-module map, so it must fail.
_GROUPED_CONSUMER = """\
module Grouped.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Epsilon
  (EpsilonCapability(..), toEpsilonCapability)

sneak ∷ EngineEnv → IO ()
sneak env = writeIORef (epFieldOne (toEpsilonCapability env)) 1
"""

# SS2.1's wrapped form carries the same grouping freedom, and it is
# parsed by its own path -- inside the wrapper's argument and around
# the whole application.
_GROUPED_WRAPPED_PROJECTION = """\
module Engine.Core.Capability.ZetaView
  ( ZetaViewCapability(..)
  , toZetaViewCapability
  ) where

import Engine.Core.ReadOnlyRef (ReadOnlyRef, toReadOnlyRef)
import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data ZetaViewCapability = ZetaViewCapability
  { ztFieldOne ∷ ReadOnlyRef Int
  , ztFieldTwo ∷ ReadOnlyRef Text
  }

toZetaViewCapability ∷ EngineEnv → ZetaViewCapability
toZetaViewCapability env = ZetaViewCapability
  { ztFieldOne = toReadOnlyRef ((fieldOne env))
  , ztFieldTwo = (toReadOnlyRef (fieldTwo env))
  }
"""

# Two bindings the canonicalizer genuinely cannot read: an
# unrecognized wrapper (which might copy) and an operator expression.
# Widening the canonicalizer to guess at either is exactly what #2059
# forbids -- the requirement is that they FAIL, not that they parse.
_UNREADABLE_PROJECTION = """\
module Engine.Core.Capability.Eta
  ( EtaCapability(..)
  , toEtaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

data EtaCapability = EtaCapability
  { etFieldOne   ∷ IORef Int
  , etFieldTwo   ∷ IORef Text
  , etFieldThree ∷ Q.Queue Int
  }

toEtaCapability ∷ EngineEnv → EtaCapability
toEtaCapability env = EtaCapability
  { etFieldOne   = fieldOne env
  , etFieldTwo   = snapshotOf (fieldTwo env)
  , etFieldThree = chooseRef . pick $ env
  }
"""

# A record whose projection is not named `to<Name>Capability`, so no
# SS2.1 signature is discoverable: legal Haskell that loses EVERY
# selector of the record at once.
_UNPROJECTED_CAPABILITY = """\
module Engine.Core.Capability.Theta
  ( ThetaCapability(..)
  ) where

import Engine.Core.State (EngineEnv, fieldThree)

data ThetaCapability = ThetaCapability
  { thFieldThree ∷ Q.Queue Int
  }

thetaFrom ∷ EngineEnv → ThetaCapability
thetaFrom env = ThetaCapability
  { thFieldThree = fieldThree env
  }
"""

# A binding that canonicalizes onto a name that is not a live
# `EngineEnv` field. `capability_accessor_map` discards it at exactly
# the same cost as an unreadable one, so it must fail the same way.
_DEAD_ACCESSOR_PROJECTION = """\
module Engine.Core.Capability.Iota
  ( IotaCapability(..)
  , toIotaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldRenamed)

data IotaCapability = IotaCapability
  { ioFieldRenamed ∷ IORef Int
  }

toIotaCapability ∷ EngineEnv → IotaCapability
toIotaCapability env = IotaCapability
  { ioFieldRenamed = fieldRenamed env
  }
"""

# GHC2024 enables `GADTs`, so this declares exactly the record
# `data KappaCapability = KappaCapability { ... }` declares -- the same
# two selectors, in the same scope. Recognizing only the ordinary form
# left the whole record undiscovered, which is a strictly worse silent
# omission than an unreadable field: nothing about it reached the map
# or the completeness gate.
_GADT_PROJECTION = """\
module Engine.Core.Capability.Kappa
  ( KappaCapability(..)
  , toKappaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data KappaCapability where
  KappaCapability ∷ { kaFieldOne ∷ IORef Int
                    , kaFieldTwo ∷ IORef Text } → KappaCapability

toKappaCapability ∷ EngineEnv → KappaCapability
toKappaCapability env = KappaCapability
  { kaFieldOne = fieldOne env
  , kaFieldTwo = (fieldTwo env)
  }
"""

_GADT_CONSUMER = """\
module Kappa.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Kappa (KappaCapability(..), toKappaCapability)

sneak ∷ EngineEnv → IO ()
sneak env = writeIORef (kaFieldOne (toKappaCapability env)) 1
"""

# The third legal spelling: a one-field record may be a `newtype`.
_NEWTYPE_PROJECTION = """\
module Engine.Core.Capability.Lambda
  ( LambdaCapability(..)
  , toLambdaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldThree)

newtype LambdaCapability = LambdaCapability
  { laFieldThree ∷ Q.Queue Int
  }

toLambdaCapability ∷ EngineEnv → LambdaCapability
toLambdaCapability env = LambdaCapability
  { laFieldThree = fieldThree env
  }
"""

# A capability type with no record block at all, followed by an
# unrelated record that HAS one. Reading the declaration by name means
# the audit must also refuse to borrow the later declaration's braces
# and report `borrowed` as this record's field.
_BLOCKLESS_CAPABILITY = """\
module Engine.Core.Capability.Nu
  ( NuCapability(..)
  , toNuCapability
  ) where

import Engine.Core.State (EngineEnv)

data NuCapability = NuAlpha | NuBeta

toNuCapability ∷ EngineEnv → NuCapability
toNuCapability env = NuAlpha

data Unrelated = Unrelated
  { borrowed ∷ Int
  }
"""

# A SUM of record constructors. Every constructor's selectors live in
# ONE scope, so `omFieldTwo` is as reachable as `omFieldOne` -- reading
# only the first constructor's block left it unenumerated, and then the
# completeness gate had nothing to say about however it was bound.
_SUM_PROJECTION = """\
module Engine.Core.Capability.Omega
  ( OmegaCapability(..)
  , toOmegaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data OmegaCapability
  = OmegaFirst { omFieldOne ∷ IORef Int }
  | OmegaSecond { omFieldOne ∷ IORef Int
                , omFieldTwo ∷ IORef Text }

toOmegaCapability ∷ EngineEnv → OmegaCapability
toOmegaCapability env = OmegaSecond
  { omFieldOne = fieldOne env
  , omFieldTwo = fieldTwo env
  }
"""

# The same declaration, with the second constructor's field bound
# through a `where`-bound helper. The accessor map cannot see through
# that either, so this is the shape in which the unenumerated field
# went completely untracked: no binding, no map entry, no violation.
_SUM_HIDDEN_PROJECTION = _SUM_PROJECTION.replace(
    "  , omFieldTwo = fieldTwo env\n  }\n",
    "  , omFieldTwo = hidden\n  }\n  where hidden = fieldTwo env\n")

_SUM_CONSUMER = """\
module Omega.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Omega (OmegaCapability(..), toOmegaCapability)

sneak ∷ EngineEnv → IO ()
sneak env = writeIORef (omFieldTwo (toOmegaCapability env)) 1
"""

# A GADT declaring one record constructor per line -- the same sum,
# spelled the other legal way.
_GADT_SUM_PROJECTION = """\
module Engine.Core.Capability.Psi
  ( PsiCapability(..)
  , toPsiCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne)

data PsiCapability where
  PsiA ∷ { psFieldOne ∷ IORef Int } → PsiCapability
  PsiB ∷ { psFieldTwo ∷ IORef Text } → PsiCapability

toPsiCapability ∷ EngineEnv → PsiCapability
toPsiCapability env = PsiA
  { psFieldOne = fieldOne env
  }
"""

# A module whose body is uniformly indented. Legal Haskell -- the
# layout column is set by the first token after `where`, and nothing
# requires it to be zero -- and every top-level declaration then sits
# at that column. The trailing unrelated record is the trap: the
# declaration span must stop at the next declaration in the SAME
# column, not run to the end of an all-indented file.
_INDENTED_MODULE = """\
module Engine.Core.Capability.Rho
  ( RhoCapability(..)
  , toRhoCapability
  ) where

  import Engine.Core.State (EngineEnv, fieldOne)

  data RhoCapability = RhoCapability
    { rhFieldOne ∷ IORef Int
    }

  toRhoCapability ∷ EngineEnv → RhoCapability
  toRhoCapability env = RhoCapability
    { rhFieldOne = fieldOne env
    }

  data Unrelated = Unrelated
    { borrowed ∷ Int
    }
"""

_INDENTED_CONSUMER = """\
module Rho.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Rho (RhoCapability(..), toRhoCapability)

sneak ∷ EngineEnv → IO ()
sneak env = writeIORef (rhFieldOne (toRhoCapability env)) 1
"""

# A declaration form this audit deliberately does not model. The
# backstop must still see that a capability record was declared, so
# the record fails loudly instead of vanishing.
_UNMODELLED_DECLARATION = """\
module Engine.Core.Capability.Sigma
  ( SigmaCapability(..)
  ) where

import Engine.Core.State (EngineEnv, fieldOne)

data instance Envelope SigmaCapability = SigmaCapability
  { sgFieldOne ∷ IORef Int
  }
"""

# The backstop's false-positive trap: a field whose TYPE is a
# capability record is not a DECLARATION of one, and neither is a
# GADT constructor's record field.
_CAPABILITY_TYPED_FIELDS = """\
module Engine.Core.Capability.Tau
  ( TauCapability(..)
  , toTauCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne)

data TauCapability = TauCapability
  { tuFieldOne ∷ IORef Int
  }

data Context = Context
  { ctxRender ∷ RenderCapability
  , ctxInput  ∷ InputCapability
  }

data Envelope where
  Envelope ∷ { evRender ∷ RenderCapability } → Envelope

toTauCapability ∷ EngineEnv → TauCapability
toTauCapability env = TauCapability
  { tuFieldOne = fieldOne env
  }
"""

# The migrated reader: it CONSUMES the wrapped handle inline, exactly as
# a `readIORef` consumer does, so it must not be counted as a pass-on.
_WRAPPED_READER = """\
module WrappedReader.Mod where

import Engine.Core.ReadOnlyRef (readReadOnlyRef)

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.DeltaView
  (DeltaViewCapability(..), toDeltaViewCapability)

peek ∷ EngineEnv → IO Int
peek env = readReadOnlyRef (dvFieldOne (toDeltaViewCapability env))
"""

# The pass-on this whole arc exists to catch: the wrapped handle is
# stored in a context record instead of being read here.
_WRAPPED_PASS_ON = """\
module WrappedPassOn.Mod where

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.DeltaView
  (DeltaViewCapability(..), toDeltaViewCapability)

observer ∷ EngineEnv → Observer
observer env = Observer { obField = dvFieldOne (toDeltaViewCapability env) }
"""

# `readReadOnlyRef` is held to the same scope rule every primitive is:
# a module-local one of that name is a different function.
_LOCAL_READONLY_PRIMITIVE = """\
module LocalReadOnly.Mod where

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.DeltaView
  (DeltaViewCapability(..), toDeltaViewCapability)

readReadOnlyRef ∷ α → IO Int
readReadOnlyRef _ = pure 0

peek ∷ EngineEnv → IO Int
peek env = readReadOnlyRef (dvFieldOne (toDeltaViewCapability env))
"""

_GAMMA_CONSUMER = """\
module Gamma.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Gamma (GammaCapability(..), toGammaCapability)

bump ∷ EngineEnv → IO ()
bump env = writeIORef (gmFieldThree (toGammaCapability env)) 1
"""

_ALPHA_CONSUMER = """\
module CollideA.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import qualified Engine.Core.Capability.Alpha as A

bump ∷ EngineEnv → IO ()
bump env = writeIORef (A.sharedRef (A.toAlphaCapability env)) 1
"""

# A module may legally define its own `writeIORef` beside a `hiding`
# import -- the only TOP-LEVEL form of that which compiles, since an
# unqualified import plus a local definition is an ambiguous occurrence
# at every use site. The local one mutates nothing.
_SHADOWED_PRIMITIVE = """\
module ShadowPrim.Mod where

import Data.IORef hiding (writeIORef)

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

writeIORef ∷ (EngineEnv → IORef Int) → Int → IO ()
writeIORef _ _ = pure ()

localHelper ∷ EngineEnv → IO ()
localHelper env = writeIORef (fieldOne env) 1

genuine ∷ EngineEnv → IO ()
genuine env = modifyIORef' (fieldTwo env) (+ 1)
"""

# A comment marker inside a STRING is text. `src/Engine/Scripting/Lua/
# Thread/Dispatch.hs:257` carries a real one -- `<> " -- " <> reason` --
# and truncating there also removes the string's closing quote, which
# desynchronises everything after it.
_STRING_COMMENT_MARKER = """\
module StringMarker.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

marked ∷ EngineEnv → IO ()
marked env = let marker = "--" in writeIORef (fieldOne env) 1

afterwards ∷ EngineEnv → IO ()
afterwards env = writeIORef (fieldTwo env) 2

nested ∷ EngineEnv → IO ()
nested env = {- outer {- inner -} still a comment -}
    writeIORef (fieldThree env) 3
"""

# `T(..)` grants `T`'s selectors and nobody else's, so a wildcard on
# some OTHER type in the same module puts no `EngineEnv` field in scope.
_FOREIGN_WILDCARD = """\
module ForeignWildcard.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, WindowState(..))

fieldOne ∷ EngineEnv → IORef Int
fieldOne _ = error "this module's own helper, not the accessor"

use ∷ EngineEnv → IO ()
use env = writeIORef (fieldOne env) 1
"""

# The control: the wildcard that DOES own the field.
_OWNING_WILDCARD = """\
module OwningWildcard.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv(..))

use ∷ EngineEnv → IO ()
use env = writeIORef (fieldTwo env) 2
"""

_BETA_CONSUMER = """\
module CollideB.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import qualified Engine.Core.Capability.Beta as B

bump ∷ EngineEnv → IO ()
bump env = writeIORef (B.sharedRef (B.toBetaCapability env)) 2
"""


def _writer_sources(**modules: str) -> dict[str, str]:
    """Synthetic production tree: the fake capability record plus
    whichever consumer fixtures a case asks for, keyed by the relative
    path `module_identifier` maps back to the module name."""
    sources = {"src/Engine/Core/Capability/Fake.hs": _FAKE_CAPABILITY}
    paths = {
        "declared": "src/Consumer/Mod.hs",
        "undeclared": "src/Interloper/Mod.hs",
        "permanent": "src/Permanent/Mod.hs",
        "trap": "src/Trap/Mod.hs",
        "narrow": "src/Narrow/Mod.hs",
        "homonym": "src/Homonym/Mod.hs",
        "qualified": "src/Qualified/Mod.hs",
        "misqualified": "src/Misqualified/Mod.hs",
        "bare": "src/Bare/Mod.hs",
        "qualPrim": "src/QualPrim/Mod.hs",
        "qualOnly": "src/QualOnly/Mod.hs",
        "typeApp": "src/TypeApp/Mod.hs",
        "strict": "src/Strict/Mod.hs",
        "infix": "src/Infix/Mod.hs",
        "hiding": "src/Hiding/Mod.hs",
        "bareOperand": "src/BareOperand/Mod.hs",
        "allPrims": "src/AllPrims/Mod.hs",
        "bareImport": "src/BareImport/Mod.hs",
        "unreadable": "src/Unreadable/Mod.hs",
        "passedOn": "src/PassedOn/Mod.hs",
        "section": "src/Section/Mod.hs",
        "callee": "src/Callee/Mod.hs",
        "multiPassed": "src/MultiPassed/Mod.hs",
        "siblings": "src/Siblings/Mod.hs",
        "afterKeyword": "src/AfterKeyword/Mod.hs",
        "recordDot": "src/RecordDot/Mod.hs",
        "composed": "src/Composed/Mod.hs",
        "asValue": "src/AsValue/Mod.hs",
        "localPrim": "src/LocalPrim/Mod.hs",
        "alpha": "src/Engine/Core/Capability/Alpha.hs",
        "beta": "src/Engine/Core/Capability/Beta.hs",
        "shadowPrim": "src/ShadowPrim/Mod.hs",
        "stringMarker": "src/StringMarker/Mod.hs",
        "foreignWildcard": "src/ForeignWildcard/Mod.hs",
        "owningWildcard": "src/OwningWildcard/Mod.hs",
        "gamma": "src/Engine/Core/Capability/Gamma.hs",
        "gammaConsumer": "src/Gamma/Mod.hs",
        "collideA": "src/CollideA/Mod.hs",
        "collideB": "src/CollideB/Mod.hs",
        "qualHomonym": "src/QualHomonym/Mod.hs",
        "parens": "src/Parens/Mod.hs",
        "parenAccessor": "src/ParenAccessor/Mod.hs",
        "explicit": "src/Explicit/Mod.hs",
        "multiline": "src/Multi/Mod.hs",
        "deltaView": "src/Engine/Core/Capability/DeltaView.hs",
        "wrappedReader": "src/WrappedReader/Mod.hs",
        "wrappedPassOn": "src/WrappedPassOn/Mod.hs",
        "localReadOnly": "src/LocalReadOnly/Mod.hs",
        "epsilon": "src/Engine/Core/Capability/Epsilon.hs",
        "grouped": "src/Grouped/Mod.hs",
        "zetaView": "src/Engine/Core/Capability/ZetaView.hs",
        "eta": "src/Engine/Core/Capability/Eta.hs",
        "theta": "src/Engine/Core/Capability/Theta.hs",
        "iota": "src/Engine/Core/Capability/Iota.hs",
        "kappa": "src/Engine/Core/Capability/Kappa.hs",
        "kappaConsumer": "src/Kappa/Mod.hs",
        "lambda": "src/Engine/Core/Capability/Lambda.hs",
        "nu": "src/Engine/Core/Capability/Nu.hs",
        "omega": "src/Engine/Core/Capability/Omega.hs",
        "omegaConsumer": "src/Omega/Mod.hs",
        "psi": "src/Engine/Core/Capability/Psi.hs",
        "rho": "src/Engine/Core/Capability/Rho.hs",
        "rhoConsumer": "src/Rho/Mod.hs",
        "sigma": "src/Engine/Core/Capability/Sigma.hs",
        "tau": "src/Engine/Core/Capability/Tau.hs",
    }
    for key, body in modules.items():
        sources[paths[key]] = body
    return sources


def _full_scan(sources: dict[str, str], exemptions=None):
    return scan_capability_writes(
        sources, _WRITER_FIELDS, permanent=_WRITER_PERMANENT,
        definer="Engine.Core.State", exemptions=exemptions or {})


def _scan(sources: dict[str, str]):
    """`(writes, residue)` -- the two halves most cases assert on."""
    scan = _full_scan(sources)
    return scan.writes, scan.residue


def test_writer_map_canonicalizes_both_consumer_shapes():
    """A capability accessor and the raw `EngineEnv` accessor it
    projects canonicalize onto the SAME field, so one map covers both
    consumer shapes."""
    accessors = capability_accessor_map(
        _writer_sources(), _WRITER_FIELDS)
    expect(accessors == {
        "fkFieldOne": (("fieldOne", "Engine.Core.Capability.Fake",
                        "FakeCapability"),),
        "fkFieldTwo": (("fieldTwo", "Engine.Core.Capability.Fake",
                        "FakeCapability"),),
    }, f"capability_accessor_map must derive each accessor's field, owner "
       f"and record type from the LIVE projection, got: {accessors}")

    writes, _ = _scan(_writer_sources(declared=_DECLARED_WRITER))
    expect(writes["fieldOne"] == {"Consumer.Mod"},
           f"a `writeIORef (fkFieldOne ...)` must be attributed to "
           f"`fieldOne`, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Consumer.Mod"},
           f"a `writeIORef (fieldTwo env)` must be attributed to the same "
           f"field through the raw accessor, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == set(),
           f"an unwritten field must map to the empty set, got: "
           f"{sorted(writes['fieldThree'])}")


def test_writer_map_accepts_a_declared_write():
    """The permitted case: both writes are declared, so nothing fails."""
    writes, _ = _scan(_writer_sources(declared=_DECLARED_WRITER))
    declared = {"fieldOne": frozenset({"Consumer.Mod"}),
                "fieldTwo": frozenset({"Consumer.Mod"}),
                "fieldThree": frozenset()}
    violations = audit_writer_modules(
        writes, _WRITER_FIELDS, declared=declared)
    expect(violations == [],
           f"a fully declared write set must produce no violation, got: "
           f"{violations}")


def test_writer_map_rejects_an_undeclared_write():
    """Requirement 1, through BOTH consumer shapes: a write from a
    module the field's map does not list is a violation."""
    writes, _ = _scan(_writer_sources(declared=_DECLARED_WRITER,
                                      undeclared=_UNDECLARED_WRITER))
    expect(writes["fieldOne"] == {"Consumer.Mod", "Interloper.Mod"},
           f"the undeclared capability-accessor write must be detected, "
           f"got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Consumer.Mod", "Interloper.Mod"},
           f"the undeclared raw-accessor write must be detected, got: "
           f"{sorted(writes['fieldTwo'])}")

    declared = {"fieldOne": frozenset({"Consumer.Mod"}),
                "fieldTwo": frozenset({"Consumer.Mod"}),
                "fieldThree": frozenset()}
    violations = audit_writer_modules(
        writes, _WRITER_FIELDS, declared=declared)
    expect(len(violations) == 2 and all("Interloper.Mod" in v
                                        for v in violations),
           f"both undeclared writes must be reported, one per field, got: "
           f"{violations}")


def test_writer_map_rejects_a_stale_entry():
    """Requirement 2 -- the both-directions half
    `RENDER_MAIN_ONLY_MODULES` already has: a mapped module that no
    longer writes the field fails as loudly as an undeclared write."""
    writes, _ = _scan(_writer_sources(declared=_DECLARED_WRITER))
    declared = {"fieldOne": frozenset({"Consumer.Mod", "Departed.Mod"}),
                "fieldTwo": frozenset({"Consumer.Mod"}),
                "fieldThree": frozenset()}
    violations = audit_writer_modules(
        writes, _WRITER_FIELDS, declared=declared)
    expect(len(violations) == 1 and "Departed.Mod" in violations[0]
           and "no longer writes" in violations[0],
           f"a mapped module with no backing write must be reported, got: "
           f"{violations}")


def test_writer_map_keys_track_the_live_field_set():
    """The reviewer's key-set requirement, both ways: a live field with
    no map entry fails, and a map key that is no longer a field fails."""
    writes, _ = _scan(_writer_sources())
    missing = audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={"fieldOne": frozenset(), "fieldTwo": frozenset()})
    expect(len(missing) == 1 and "fieldThree" in missing[0]
           and "no entry in CAPABILITY_WRITER_MODULES" in missing[0],
           f"a live field with no map entry must be reported, got: {missing}")

    stale = audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={"fieldOne": frozenset(), "fieldTwo": frozenset(),
                  "fieldThree": frozenset(), "fieldGone": frozenset()})
    expect(len(stale) == 1 and "fieldGone" in stale[0]
           and "remove the stale key" in stale[0],
           f"a map key that is not a live field must be reported, got: "
           f"{stale}")


def test_permanent_module_writes_are_exempt():
    """Requirement 4 (design decision D-4): SS6.1's 24 permanent
    full-access modules hold whole-session orchestration authority by
    job description, so their writes are neither violations nor map
    entries."""
    writes, residue = _scan(_writer_sources(permanent=_PERMANENT_WRITER))
    expect(writes["fieldOne"] == set(),
           f"a write from an SS6.1 permanent module must not enter the "
           f"write map, got: {sorted(writes['fieldOne'])}")
    violations = audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={f: frozenset() for f in _WRITER_FIELDS})
    expect(violations == [],
           f"an SS6.1 permanent module's write must not be a violation, "
           f"got: {violations}")
    expect(residue == [],
           f"the permanent fixture uses raw accessors only, so it "
           f"contributes no capability-accessor residue, got: {residue}")


def test_passed_on_handle_is_residue_not_a_write():
    """Requirement 5 (D-5): a handle handed to a helper is counted and
    listed, never attributed and never a violation."""
    writes, residue = _scan(_writer_sources(trap=_TRAP_MODULE))
    expect(writes["fieldTwo"] == set(),
           f"`Trap.Mod` performs no attributable write -- its only "
           f"accessor use passes the handle onward, got: "
           f"{sorted(writes['fieldTwo'])}")
    passed = [item for item in residue if item.module == "Trap.Mod"]
    expect(len(passed) == 1 and passed[0].accessor == "fkFieldTwo"
           and passed[0].field == "fieldTwo"
           and passed[0].relpath == "src/Trap/Mod.hs",
           f"the passed-on handle must appear once in the residue with its "
           f"path, accessor and canonical field, got: {passed}")
    expect(audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={f: frozenset() for f in _WRITER_FIELDS}) == [],
           "residue must never be reported as a violation")


def test_out_of_scope_names_are_not_writes():
    """The import-scope gate, on its own. `Homonym.Mod` defines its own
    `fieldOne` helper and APPLIES it exactly the way a real write
    applies the accessor, so the write's shape says nothing; the only
    thing that distinguishes it from the field is that
    `Engine.Core.State` is imported for the `EngineEnv` TYPE alone. The
    two gates are independent on purpose: neither is asked to be
    complete by itself."""
    writes, residue = _scan(_writer_sources(homonym=_LOCAL_HOMONYM))
    expect(writes["fieldOne"] == set(),
           f"a name the module never imported cannot be the accessor, "
           f"got: {sorted(writes['fieldOne'])}")
    expect(residue == [],
           f"the fixture names no capability accessor, so it contributes "
           f"no residue, got: {residue}")


def test_comments_and_bare_arguments_are_not_writes():
    """The two remaining false-positive gates, in the same fixture as
    the residue case so one module proves all three: commentary that
    NAMES a write does not perform one, and a BARE first argument is
    never the accessor (`shadowed`'s parameter here) because an accessor
    projects out of a handle and so cannot itself be the `IORef`."""
    writes, residue = _scan(_writer_sources(trap=_TRAP_MODULE,
                                            narrow=_TYPE_ONLY_IMPORTER))
    expect(writes["fieldOne"] == set() and writes["fieldTwo"] == set(),
           f"neither a commented-out write, a bare local argument, nor a "
           f"type-only import may produce a write, got: fieldOne="
           f"{sorted(writes['fieldOne'])}, fieldTwo="
           f"{sorted(writes['fieldTwo'])}")
    expect(all(item.module != "Narrow.Mod" for item in residue),
           f"a module that never names a capability accessor contributes "
           f"no residue, got: {residue}")


def test_backticked_infix_mutations_are_writes():
    """Any two-argument function may be written infix, so
    ``(fieldOne env) `writeIORef` 1`` is the same direct write with its
    arguments swapped. A scan that only looked to the RIGHT of the
    primitive would miss it in silence."""
    writes, _ = _scan(_writer_sources(infix=_INFIX_WRITER))
    expect(writes["fieldOne"] == {"Infix.Mod"},
           f"a backticked raw-accessor write must be attributed, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Infix.Mod"},
           f"a backticked, qualified, capability-accessor write must be "
           f"attributed too, got: {sorted(writes['fieldTwo'])}")

    bare, _ = _scan(_writer_sources(bareOperand=_BARE_OPERAND))
    expect(bare["fieldThree"] == {"BareOperand.Mod"},
           f"an UNPARENTHESIZED left operand is the same write -- a "
           f"backtick binds looser than application, got: "
           f"{sorted(bare['fieldThree'])}")
    expect(bare["fieldOne"] == {"BareOperand.Mod"},
           f"and the same holds for a bare capability-accessor operand "
           f"under a qualified primitive, got: "
           f"{sorted(bare['fieldOne'])}")



def test_redundant_parentheses_change_nothing():
    """Parentheses around a primitive in a prefix application, and
    around an infix operand, are the same write. A primitive that
    something else is APPLYING to is not: it is being passed onward,
    which D-5 reports rather than attributes."""
    writes, _ = _scan(_writer_sources(parens=_PARENTHESIZED))
    expect(writes["fieldOne"] == {"Parens.Mod"},
           f"`(writeIORef) (fieldOne env) 1` is a write, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Parens.Mod"},
           f"a doubly parenthesized infix operand is a write, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == set(),
           f"a primitive handed to another function is not this module's "
           f"write, got: {sorted(writes['fieldThree'])}")


def test_parentheses_around_the_accessor_change_nothing():
    """`writeIORef ((fieldOne) env) 1` applies exactly what
    `writeIORef (fieldOne env) 1` does, prefix or infix. Only the
    closers balancing the openers stepped over are consumed, so a
    genuinely unapplied `(fieldThree)` still is not a write."""
    writes, _ = _scan(_writer_sources(parenAccessor=_PARENTHESIZED_ACCESSOR))
    expect(writes["fieldOne"] == {"ParenAccessor.Mod"},
           f"a parenthesized prefix accessor head is a write, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"ParenAccessor.Mod"},
           f"a parenthesized infix accessor head is a write, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == set(),
           f"an unapplied accessor is still not a write, got: "
           f"{sorted(writes['fieldThree'])}")


def test_visible_type_applications_are_skipped():
    """`writeIORef @Int (fieldOne env) 1` is a direct write. A scan that
    expects the accessor immediately after the primitive stops at the
    `@` and lets an undeclared writer through in silence."""
    writes, _ = _scan(_writer_sources(typeApp=_TYPE_APPLICATION))
    expect(writes["fieldOne"] == {"TypeApp.Mod"},
           f"a type application by name must be stepped over, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"TypeApp.Mod"},
           f"a parenthesized type application must be stepped over "
           f"whole, got: {sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == {"TypeApp.Mod"},
           f"a type application INSIDE parentheses around the primitive "
           f"must be stepped over before the closer, got: "
           f"{sorted(writes['fieldThree'])}")


def test_strict_application_groups_like_lazy():
    """`$!` is `$` with a `seq`, and groups its argument identically. The
    tokenizer splits it into two punctuation tokens, so its `!` has to
    be stepped over or the write disappears."""
    writes, _ = _scan(_writer_sources(strict=_STRICT_APPLICATION))
    expect(writes["fieldOne"] == {"Strict.Mod"},
           f"`writeIORef $! fieldOne env` is a write, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Strict.Mod"},
           f"and so is the lazy control, got: "
           f"{sorted(writes['fieldTwo'])}")


def test_a_first_argument_must_be_applied():
    """`_first_argument_head` directly, on the two halves of the rule:
    the argument must be GROUPED (parenthesized, or reached through
    `$`), and within that group the accessor must be APPLIED. A bare
    `prim ref v` and a parenthesized-but-unapplied `prim (ref) v` are
    both non-answers, and only the second distinguishes the halves."""
    def head_of(text):
        tokens = tokenize_haskell(text)
        return _first_argument_head(tokens, 0)

    expect(head_of("writeIORef (fieldOne env) 1") == 2,
           "an applied accessor inside parens is the first-argument head")
    expect(head_of("writeIORef $ fieldOne env") == 2,
           "`$` groups the first argument just as parentheses do")
    expect(head_of("writeIORef $! fieldOne env") == 3,
           "and `$!` groups it identically")
    expect(head_of("writeIORef (fieldOne) 1") is None,
           "a parenthesized but unapplied name is not an accessor "
           "application")
    expect(head_of("writeIORef ((fieldOne) env) 1") == 3,
           "parentheses around the accessor itself change nothing")
    expect(head_of("writeIORef fieldOne 1") is None,
           "a bare first argument is never an accessor application")
    expect(head_of("writeIORef @Int (fieldOne env) 1") == 4,
           "a visible type application is stepped over, not treated as "
           "the value argument")
    expect(head_of("writeIORef @Int fieldOne 1") is None,
           "stepping over a type application must not turn a bare "
           "argument into an application")

    def infix_head_of(text):
        tokens = tokenize_haskell(text)
        index = next(i for i, token in enumerate(tokens)
                     if token.text.endswith("writeIORef"))
        return _infix_left_operand_head(tokens, index)

    expect(infix_head_of("(fieldOne env) `writeIORef` 1") == 1,
           "a backticked primitive's left operand is its accessor")
    expect(infix_head_of("(fieldOne) `writeIORef` 1") is None,
           "an unapplied left operand is not an accessor application")
    expect(infix_head_of("writeIORef (fieldOne env) 1") is None,
           "a prefix application has no infix left operand")
    expect(infix_head_of("(fieldOne env) `writeIORef 1") is None,
           "an unterminated backtick is not an infix application")
    expect(infix_head_of("fieldOne env `writeIORef` 1") == 0,
           "an unparenthesized applied operand is read back to its head")
    expect(infix_head_of("fieldOne `writeIORef` 1") is None,
           "an unapplied bare operand is not an accessor application")
    expect(infix_head_of("x >> fieldOne env `writeIORef` 1") == 3,
           "the walk stops at the operator, not at the start of the line")
    expect(infix_head_of("fkFieldOne (cap env) `writeIORef` 1") == 0,
           "a trailing `)` closing an ARGUMENT is not the operand's own")
    expect(infix_head_of("((fieldOne) env) `writeIORef` 1") == 2,
           "and the same holds for an infix operand's head")
    expect(infix_head_of("(pick cfg) fieldOne `writeIORef` 1") == 1,
           "a group to the LEFT of an identifier is the application's "
           "head, so the identifier is its argument, not the accessor")

    # `_applied_head` consumes exactly the closers that balance the
    # openers written directly before the accessor. Reading past them
    # would let an unapplied accessor borrow whatever follows the group
    # it sits in.
    nested = tokenize_haskell("f ((fieldOne)) env")
    head = next(i for i, token in enumerate(nested)
                if token.text == "fieldOne")
    expect(_applied_head(nested, head) == head,
           "two openers before the accessor balance two closers, after "
           "which `env` applies it")
    trailing = tokenize_haskell("f (fieldOne)) env")
    head = next(i for i, token in enumerate(trailing)
                if token.text == "fieldOne")
    expect(_applied_head(trailing, head) is None,
           "one opener balances one closer, and the next token is "
           "another closer, not an argument")


def test_qualified_accessors_are_resolved():
    """A qualified spelling names the field exactly as the bare one
    does, through the module's own name or an `as` alias, so it must be
    attributed rather than silently missed -- otherwise
    `import qualified Engine.Core.State as State` is a hole in the
    gate."""
    writes, _ = _scan(_writer_sources(qualified=_QUALIFIED_WRITER))
    expect(writes["fieldTwo"] == {"Qualified.Mod"},
           f"`State.fieldTwo` must resolve to the raw field, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldOne"] == {"Qualified.Mod"},
           f"`Cap.fkFieldOne` must resolve through the capability "
           f"projection, got: {sorted(writes['fieldOne'])}")

    violations = audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={f: frozenset() for f in _WRITER_FIELDS})
    expect(len(violations) == 2 and all("Qualified.Mod" in v
                                        for v in violations),
           f"an undeclared qualified write must be a violation like any "
           f"other, got: {violations}")


def test_a_qualifier_must_name_the_owning_module():
    """The other half of qualified resolution: a prefix bound to a
    DIFFERENT module does not name this field, and an `as` alias
    REPLACES the module's own name as a qualifier rather than joining
    it. Neither line may be attributed."""
    writes, _ = _scan(_writer_sources(misqualified=_MISQUALIFIED))
    expect(writes["fieldTwo"] == set(),
           f"neither a foreign qualifier nor an alias-replaced module "
           f"name may resolve to the field, got: "
           f"{sorted(writes['fieldTwo'])}")


def test_qualified_mutation_primitives_are_recognized():
    """A mutation primitive under a qualifier -- `Ref.writeIORef`, from
    `import qualified Data.IORef as Ref` -- is the same write, and
    missing it would be a silent hole rather than a conservative
    miss."""
    writes, _ = _scan(_writer_sources(qualPrim=_QUALIFIED_PRIMITIVE))
    expect(writes["fieldOne"] == {"QualPrim.Mod"},
           f"`Ref.writeIORef` must be recognized as a mutation "
           f"primitive, got: {sorted(writes['fieldOne'])}")


def test_a_hiding_clause_removes_a_name_from_scope():
    """`hiding` brings in everything EXCEPT the listed names, so a
    module that hides `fieldOne` and defines its own is not writing the
    field -- while everything it did NOT hide stays in scope. Treating
    a `hiding` import as simply unrestricted loses that."""
    writes, _ = _scan(_writer_sources(hiding=_HIDING_IMPORTER))
    expect(writes["fieldOne"] == set(),
           f"a hidden name is out of scope, so the module's own helper "
           f"is not the accessor, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Hiding.Mod"},
           f"a `hiding` clause must not remove anything it did not "
           f"name, got: {sorted(writes['fieldTwo'])}")

    declarations = parse_imports(
        "import Engine.Core.State hiding (fieldOne)\n")
    expect(not imports_name(declarations, "Engine.Core.State",
                            "fieldOne", ""),
           "the hidden name is not in scope")
    expect(imports_name(declarations, "Engine.Core.State",
                        "fieldTwo", ""),
           "everything else still is")


def test_a_qualified_only_import_excludes_the_bare_spelling():
    """`qualified` removes the UNQUALIFIED spelling from scope entirely,
    so a module-local homonym is not the field even though the owner is
    imported -- while the qualified spelling in the same module still
    is. Merging every import of a module into one scope answer loses
    exactly this distinction."""
    writes, _ = _scan(_writer_sources(qualOnly=_QUALIFIED_ONLY))
    expect(writes["fieldOne"] == set(),
           f"a bare `fieldOne` is out of scope under a qualified-only "
           f"import, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"QualOnly.Mod"},
           f"`State.fieldTwo` in the same module must still be "
           f"attributed, got: {sorted(writes['fieldTwo'])}")


def test_import_declarations_record_qualification_and_alias():
    """`parse_imports` keeps each declaration separate, because one
    module is legitimately imported twice on different terms and each
    declaration carries its own answer."""
    declarations = parse_imports(
        "import Engine.Core.State (EngineEnv, fieldOne)\n"
        "import qualified Engine.Core.State as State\n"
        "import Data.IORef qualified as Ref\n"
        "import Data.Map hiding (lookup)\n"
        "import Engine.Core.Capability.Fake (FakeCapability(..))\n"
        "import Engine.Core.Defaults\n")
    shape = [(d.module, d.qualified, d.qualifier,
              None if d.names is None else sorted(d.names),
              sorted(d.wildcards))
             for d in declarations]
    expect(shape == [
        ("Engine.Core.State", False, "Engine.Core.State",
         ["EngineEnv", "fieldOne"], []),
        ("Engine.Core.State", True, "State", None, []),
        ("Data.IORef", True, "Ref", None, []),
        ("Data.Map", False, "Data.Map", None, []),
        ("Engine.Core.Capability.Fake", False,
         "Engine.Core.Capability.Fake", [], ["FakeCapability"]),
        ("Engine.Core.Defaults", False, "Engine.Core.Defaults", None, []),
    ], f"all six import shapes -- explicit list, qualified-with-alias, "
       f"`ImportQualifiedPost`, `hiding`, a `(..)` wildcard and a bare "
       f"import -- must each be recorded with its own qualification, "
       f"qualifier and name list, got: {shape}")

    expect(imports_name(declarations, "Engine.Core.State", "fieldOne", ""),
           "the unqualified declaration puts the bare name in scope")
    expect(imports_name(declarations, "Engine.Core.State", "fieldOne",
                        "State"),
           "the qualified declaration puts `State.fieldOne` in scope")
    expect(not imports_name(declarations[1:], "Engine.Core.State",
                            "fieldOne", ""),
           "a qualified-only import puts NO bare spelling in scope")
    expect(not imports_name(declarations, "Engine.Core.State", "fieldOne",
                            "Ref"),
           "a qualifier bound to another module resolves nothing here")
    expect(not imports_name(declarations[:1], "Engine.Core.State",
                            "fieldTwo", ""),
           "an explicit list brings in the names it enumerates and no "
           "others")


def test_a_bare_argument_surfaces_as_residue():
    """A bare accessor name in a mutation primitive's first argument is
    never attributed -- and when it is a CAPABILITY accessor (the record
    wildcard the rule's one blind spot needs) it is not silently
    dropped either: with no application to consume it inline, it lands
    in the pass-on residue where D-5 can count it."""
    writes, residue = _scan(_writer_sources(bare=_BARE_ARGUMENT))
    expect(writes["fieldOne"] == set() and writes["fieldTwo"] == set(),
           f"a bare first argument must never be attributed, got: "
           f"fieldOne={sorted(writes['fieldOne'])}, "
           f"fieldTwo={sorted(writes['fieldTwo'])}")
    bare = [item for item in residue if item.module == "Bare.Mod"]
    # `(fieldTwo)` is parenthesized but never applied -- the grouping
    # test alone would let it through, so both halves of the rule are
    # exercised here.
    expect(len(bare) == 1 and bare[0].accessor == "fkFieldOne"
           and bare[0].field == "fieldOne",
           f"the wildcard-bound capability accessor must be reported as "
           f"residue rather than dropped, got: {bare}")


def test_import_declarations_are_not_uses():
    """An import list names the accessor; naming one is not using one.
    `Explicit.Mod` imports `fkFieldOne` and `fkFieldTwo` by name across
    four lines and writes only the first, so the import declaration --
    the one place both tokens appear together -- must register as
    neither a write nor a residue use. It also drives
    `parse_imports`' explicit-name path, which `FakeCapability(..)`
    never reaches."""
    writes, residue = _scan(_writer_sources(explicit=_EXPLICIT_IMPORTER))
    expect(writes["fieldOne"] == {"Explicit.Mod"},
           f"an accessor imported by name must still be in scope at its "
           f"write site, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == set(),
           f"`fkFieldTwo` appears only in the import list, so nothing may "
           f"be attributed to `fieldTwo`, got: {sorted(writes['fieldTwo'])}")
    named = [item for item in residue if item.module == "Explicit.Mod"]
    expect(named == [],
           f"an import declaration must not register as a use, got: {named}")


def test_multiline_expressions_are_scanned():
    """The scan reads complete EXPRESSIONS: a mutation whose accessor
    argument sits three lines below the primitive is one token
    sequence, exactly like the real `rhStructureArtCatalogRef` and
    `rvFramebufferMinimizeGenRef` sites."""
    writes, _ = _scan(_writer_sources(multiline=_MULTILINE_WRITER))
    expect(writes["fieldOne"] == {"Multi.Mod"},
           f"a four-line `atomicModifyIORef'` must be detected, got: "
           f"{sorted(writes['fieldOne'])}")


def test_tokenizer_skips_literals_and_keeps_line_numbers():
    """String and character literals are consumed whole, so an accessor
    name inside one is not a token; identifier primes stay part of the
    identifier; and every token carries its own 1-based line, which is
    what makes the residue report citable."""
    tokens = tokenize_haskell(
        'a = "fieldOne"\nb = \'x\'\nmodifyIORef\' c\n')
    texts = [t.text for t in tokens if t.kind == "id"]
    expect("fieldOne" not in texts,
           f"an accessor name inside a string literal must not tokenize as "
           f"an identifier, got: {texts}")
    expect("modifyIORef'" in texts,
           f"a primed identifier must tokenize whole, got: {texts}")
    line = next(t.line for t in tokens if t.text == "modifyIORef'")
    expect(line == 3,
           f"`modifyIORef'` sits on line 3, got: {line}")


def test_every_recognized_primitive_is_read():
    """All six mutation primitives in the closed set, each on the same
    field, so a spelling that stopped being recognized shows up as a
    missing write SITE rather than a silently smaller map."""
    scan = _full_scan(_writer_sources(allPrims=_ALL_PRIMITIVES))
    expect(scan.writes["fieldOne"] == {"AllPrims.Mod"},
           f"every primitive must attribute to the field, got: "
           f"{sorted(scan.writes['fieldOne'])}")
    attributed = [site for site in scan.sites
                  if site.module == "AllPrims.Mod" and site.kind == "write"]
    expect(len(attributed) == 6,
           f"all six spellings must be read as writes, got "
           f"{len(attributed)}")


def test_a_bare_import_brings_the_accessor_into_scope():
    """The last import shape: a bare import grants everything the target
    exports, so the accessor is in scope without being named."""
    writes, _ = _scan(_writer_sources(bareImport=_BARE_IMPORTER))
    expect(writes["fieldOne"] == {"BareImport.Mod"},
           f"a bare import puts the accessor in scope, got: "
           f"{sorted(writes['fieldOne'])}")


def test_an_unreadable_mutation_site_blocks():
    """Requirement 6. An argument is plainly being formed and its head
    is not an identifier, so the scan says so and the audit fails --
    which is how a spelling outside the recognized set stops the gate
    instead of silently dropping a write."""
    scan = _full_scan(_writer_sources(unreadable=_UNREADABLE))
    kinds = [site.kind for site in scan.sites
             if site.module == "Unreadable.Mod"]
    expect(kinds == ["unclassifiable"],
           f"the site must be recorded as unclassifiable, got: {kinds}")
    violations = audit_mutation_sites(scan.sites)
    expect(len(violations) == 1 and "Unreadable" in violations[0],
           f"and that must be a blocking violation, got: {violations}")


def test_a_primitive_must_be_in_head_position():
    """`withLogging writeIORef (fieldOne env) 1` hands the primitive to
    `withLogging`. Reading the tokens after it as its own arguments
    invents a write — and, with a capability accessor, hides that
    accessor's pass-on residue entry behind a phantom inline use.

    A KEYWORD before the primitive applies to nothing, so `else
    writeIORef (...) ...` is head position; layout ends a statement
    with no token at all, so a preceding identifier or bracket only
    counts on the SAME line."""
    scan = _full_scan(_writer_sources(passedOn=_UNPARENTHESIZED_VALUE))
    expect(scan.writes["fieldOne"] == set()
           and scan.writes["fieldTwo"] == set(),
           f"a primitive being passed on writes nothing, got: "
           f"fieldOne={sorted(scan.writes['fieldOne'])}, "
           f"fieldTwo={sorted(scan.writes['fieldTwo'])}")
    expect([site.kind for site in scan.sites
            if site.module == "PassedOn.Mod"] == ["other", "other"],
           "both sites classify as ordinary non-writes")
    residue = [item for item in scan.residue
               if item.module == "PassedOn.Mod"]
    expect(len(residue) == 1 and residue[0].accessor == "fkFieldTwo",
           f"and the capability accessor keeps its residue entry, got: "
           f"{residue}")

    writes, _ = _scan(_writer_sources(afterKeyword=_AFTER_KEYWORD))
    expect(writes["fieldThree"] == {"AfterKeyword.Mod"},
           f"while a primitive after a keyword is in head position, "
           f"got: {sorted(writes['fieldThree'])}")

    # A newline does not end an application: the continuation is
    # indented past the line that opened it.
    scan = _full_scan(_writer_sources(multiPassed=_MULTILINE_VALUE))
    expect(scan.writes["fieldOne"] == set()
           and scan.writes["fieldTwo"] == set(),
           f"a multiline hand-off writes nothing either, got: "
           f"fieldOne={sorted(scan.writes['fieldOne'])}, "
           f"fieldTwo={sorted(scan.writes['fieldTwo'])}")
    expect(len([item for item in scan.residue
                if item.module == "MultiPassed.Mod"]) == 1,
           "and the capability accessor keeps its residue entry")

    # …while a sibling statement at the same column is a new statement,
    # however the previous one ended.
    writes, _ = _scan(_writer_sources(siblings=_SIBLING_STATEMENTS))
    expect(writes["fieldThree"] == {"Siblings.Mod"},
           f"a statement following `pure ()` is not its continuation, "
           f"got: {sorted(writes['fieldThree'])}")


def test_an_operator_section_applying_a_primitive_blocks():
    """`($) writeIORef (fieldOne env) 1` is a direct write and
    `(.) writeIORef f` is not, and nothing textual separates them. The
    site is therefore unreadable rather than silently `other` --
    recognizing each operator individually is the open-ended path this
    arc rejects.

    A parenthesized group holding a real expression is not a section,
    and stays the ordinary passed-on case."""
    scan = _full_scan(_writer_sources(section=_OPERATOR_SECTION))
    expect([site.kind for site in scan.sites
            if site.module == "Section.Mod"] == ["unclassifiable"],
           f"an applied operator section must block, got: "
           f"{[s.kind for s in scan.sites if s.module == 'Section.Mod']}")
    expect(len(audit_mutation_sites(scan.sites)) == 1, "and be reported")

    scan = _full_scan(_writer_sources(callee=_PARENTHESIZED_CALLEE))
    expect([site.kind for site in scan.sites
            if site.module == "Callee.Mod"] == ["other"],
           "a parenthesized expression callee is not a section")
    expect(scan.writes["fieldTwo"] == set()
           and audit_mutation_sites(scan.sites) == [],
           "so it is an ordinary hand-off: no write, and no block")


def test_record_dot_access_is_unclassifiable():
    """`modifyIORef' (env.fieldOne) id` is a direct mutation the scan
    cannot read. Taking `env` as the argument head would make it a
    silent non-write, which is exactly what requirement 6 exists to
    prevent, so the site blocks instead.

    Spaced composition tokenizes identically and is ordinary code —
    only the ABSENCE of a gap distinguishes them — so it must not be
    swept up with it."""
    scan = _full_scan(_writer_sources(recordDot=_RECORD_DOT))
    kinds = [site.kind for site in scan.sites
             if site.module == "RecordDot.Mod"]
    expect(kinds == ["unclassifiable"],
           f"record-dot access must block, got: {kinds}")
    expect(len(audit_mutation_sites(scan.sites)) == 1,
           "and be reported")

    scan = _full_scan(_writer_sources(composed=_COMPOSED_ARGUMENT))
    kinds = [site.kind for site in scan.sites
             if site.module == "Composed.Mod"]
    expect(kinds == ["other"],
           f"spaced composition is ordinary code, got: {kinds}")
    expect(audit_mutation_sites(scan.sites) == [],
           "and blocks nothing")


def test_a_primitive_used_as_a_value_is_not_unreadable():
    """The other side of requirement 6: a primitive that is not applied
    to anything HERE is being handed onward, which is an ordinary
    non-write, not an unreadable site. Confusing the two would make the
    guard fire on correct code."""
    scan = _full_scan(_writer_sources(asValue=_PRIMITIVE_AS_VALUE))
    kinds = [site.kind for site in scan.sites
             if site.module == "AsValue.Mod"]
    expect(kinds == ["other"],
           f"a primitive passed as a value classifies as `other`, got: "
           f"{kinds}")
    expect(audit_mutation_sites(scan.sites) == [],
           "and blocks nothing")


def test_a_comment_marker_inside_a_string_is_text():
    """`let marker = "--" in writeIORef (fieldOne env) 1` is a real
    write. Stripping at that `--` would drop it AND remove the string's
    closing quote, desynchronising every literal after it -- which is
    how three genuine mutation sites in
    `Engine.Scripting.Lua.Thread.Dispatch` were invisible until this
    was fixed. Block comments nest, too."""
    writes, _ = _scan(_writer_sources(stringMarker=_STRING_COMMENT_MARKER))
    expect(writes["fieldOne"] == {"StringMarker.Mod"},
           f"the write after a string containing `--` must survive, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"StringMarker.Mod"},
           f"and so must everything after it, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == {"StringMarker.Mod"},
           f"a NESTED block comment must close where Haskell closes it, "
           f"got: {sorted(writes['fieldThree'])}")

    stripped = _strip_haskell_comments(
        'a = "-- not a comment" -- but this is\n'
        'b = x --> y\n'
        'c = {- {- nested -} still -} kept\n')
    expect('"-- not a comment"' in stripped and "but this is" not in stripped,
           f"only the real comment is blanked, got: {stripped!r}")
    expect("x --> y" in stripped,
           f"a dash run continuing into a symbol is an operator, got: "
           f"{stripped!r}")
    expect("kept" in stripped and "nested" not in stripped
           and "still" not in stripped,
           f"a nested block comment closes at its own end, got: "
           f"{stripped!r}")
    expect(len(stripped.split("\n")) == 4,
           "and every line position is preserved")

    # A prime CONTINUES an identifier. Reading `x'` as opening a
    # character literal consumes `' '` and leaves the following quote
    # looking like a string opener, which swallows the rest of the file.
    primed = _strip_haskell_comments("f x' '\"' = 1 -- gone\ng = 2\n")
    expect("gone" not in primed and "g = 2" in primed,
           f"a primed identifier must not open a character literal, got: "
           f"{primed!r}")


def test_token_lines_survive_a_string_gap():
    """A Haskell string gap is a backslash, whitespace including
    NEWLINES, and another backslash. Skipping the escaped character
    without counting that newline reports every later token a line
    early -- and a residue entry or a blocking site then names the
    wrong source line, which is the one thing those reports exist to
    give."""
    tokens = tokenize_haskell('a = "start\\\n   \\end"\nb = 1\n')
    lines = {token.text: token.line for token in tokens if token.kind == "id"}
    expect(lines.get("a") == 1 and lines.get("b") == 3,
           f"the gap spans one newline, so `b` sits on line 3, got: "
           f"{lines}")

    plain = tokenize_haskell('a = "one\\ntwo"\nb = 1\n')
    expect({t.text: t.line for t in plain if t.kind == "id"}.get("b") == 2,
           "while an escaped `\\n` inside a literal spans no newline "
           "at all")


def test_a_wildcard_grants_only_its_own_type_s_selectors():
    """`import Engine.Core.State (WindowState(..))` brings in
    `WindowState`'s selectors, not `EngineEnv`'s -- so a module-local
    `fieldOne` used beside it is not the accessor. Treating every
    `(..)` as unrestricted access would make that a false writer, and
    then an undeclared-writer failure over code that touches no field."""
    writes, _ = _scan(_writer_sources(foreignWildcard=_FOREIGN_WILDCARD))
    expect(writes["fieldOne"] == set(),
           f"another type's wildcard puts no `EngineEnv` field in scope, "
           f"got: {sorted(writes['fieldOne'])}")

    writes, _ = _scan(_writer_sources(owningWildcard=_OWNING_WILDCARD))
    expect(writes["fieldTwo"] == {"OwningWildcard.Mod"},
           f"but the owning type's wildcard does, got: "
           f"{sorted(writes['fieldTwo'])}")

    declarations = parse_imports(
        "import Engine.Core.State (EngineEnv, WindowState(..))\n")
    expect(not imports_name(declarations, "Engine.Core.State", "fieldOne",
                            "", "EngineEnv"),
           "a foreign wildcard grants nothing here")
    expect(imports_name(parse_imports(
        "import Engine.Core.State (EngineEnv(..))\n"),
        "Engine.Core.State", "fieldOne", "", "EngineEnv"),
           "and the owning one grants everything it declares")


def test_a_projection_may_name_its_accessor_qualified():
    """A capability module may import `Engine.Core.State` under an
    alias and project `gmFieldThree = State.fieldThree env`. If that
    spelling is not parsed, the record's accessors never enter the map
    and every write through them is classified as somebody else's."""
    sources = _writer_sources(gamma=_QUALIFIED_PROJECTION,
                              gammaConsumer=_GAMMA_CONSUMER)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("gmFieldThree") == (
        ("fieldThree", "Engine.Core.Capability.Gamma", "GammaCapability"),),
           f"the qualified projection must canonicalize to the bare "
           f"field, got: {accessors.get('gmFieldThree')}")

    writes, _ = _scan(sources)
    expect(writes["fieldThree"] == {"Gamma.Mod"},
           f"and the write through it must be attributed, got: "
           f"{sorted(writes['fieldThree'])}")


def test_a_view_field_wrapped_by_a_named_alias_wrapper_canonicalizes():
    """SS2.1's abstract-wrapper extension (issue #1896). A reader-facing
    view projects `dvFieldOne = toReadOnlyRef (fieldOne env)` -- the same
    live handle, denied a write by its type. If that spelling is not
    parsed, the accessor never enters the map, and then EVERY use of it
    is invisible: the write scan cannot attribute one, and the pass-on
    residue CMA-3 weighs silently loses the context-record sites the
    wrapper was introduced to protect.

    The wrapper set is CLOSED, so an unrecognized function around the
    accessor does not canonicalize -- `snapshotOf` might copy, and
    inventing an alias for it would claim a guarantee nothing gives."""
    sources = _writer_sources(deltaView=_WRAPPED_PROJECTION)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("dvFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.DeltaView",
         "DeltaViewCapability"),),
           f"the wrapped projection must canonicalize to the bare field, "
           f"got: {accessors.get('dvFieldOne')}")
    expect("dvFieldTwo" not in accessors,
           f"but an unrecognized wrapper must NOT be treated as an alias, "
           f"got: {accessors.get('dvFieldTwo')}")


def test_a_redundantly_grouped_projection_canonicalizes_and_is_enforced():
    """Issue #2059's requirement 1, bare form. `(fieldOne env)` and
    `(fieldTwo) env` are the ungrouped bindings with semantically inert
    parentheses, so they must canonicalize identically -- and a direct
    write through the selector must fail the writing-module map exactly
    as the ungrouped spelling does.

    Before the fix both bindings were unreadable: the accessor map
    omitted both selectors, `writeIORef (epFieldOne ...)` resolved to no
    field and was recorded as `other`, and `audit_writer_modules` had
    nothing to reject while `audit_mutation_sites` saw nothing
    unclassifiable. The gate exited 0 on an unenforced write."""
    sources = _writer_sources(epsilon=_GROUPED_PROJECTION,
                              grouped=_GROUPED_CONSUMER)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("epFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.Epsilon",
         "EpsilonCapability"),),
           f"a binding grouped as `(accessor env)` must canonicalize to "
           f"the bare field, got: {accessors.get('epFieldOne')}")
    expect(accessors.get("epFieldTwo") == (
        ("fieldTwo", "Engine.Core.Capability.Epsilon",
         "EpsilonCapability"),),
           f"and so must one grouped as `(accessor) env`, got: "
           f"{accessors.get('epFieldTwo')}")

    expect(canonical_projection_accessor("((fieldOne env))") == "fieldOne",
           "nested inert grouping must canonicalize too")
    expect(canonical_projection_accessor("State.fieldOne env") == "fieldOne",
           "a qualified accessor must still report bare")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(violations == [],
           f"a fully readable grouped projection must raise no "
           f"completeness violation, got: {violations}")

    writes, _ = _scan(sources)
    expect(writes["fieldOne"] == {"Grouped.Mod"},
           f"the write through the grouped selector must be attributed, "
           f"got: {sorted(writes['fieldOne'])}")

    declared = {"fieldOne": frozenset(), "fieldTwo": frozenset(),
                "fieldThree": frozenset()}
    rejected = audit_writer_modules(writes, _WRITER_FIELDS, declared=declared)
    expect(len(rejected) == 1 and "Grouped.Mod" in rejected[0],
           f"and the undeclared write through it must be rejected, got: "
           f"{rejected}")


def test_a_redundantly_grouped_wrapped_projection_canonicalizes():
    """Requirement 1's other half. The wrapped form
    (`wrapper (accessor env)`) is read by its own path, so its grouping
    freedom needs its own case: parentheses INSIDE the wrapper's
    argument and parentheses AROUND the whole application both leave
    the same live handle, and both must reach the same field."""
    sources = _writer_sources(zetaView=_GROUPED_WRAPPED_PROJECTION)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("ztFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.ZetaView",
         "ZetaViewCapability"),),
           f"`toReadOnlyRef ((accessor env))` must canonicalize, got: "
           f"{accessors.get('ztFieldOne')}")
    expect(accessors.get("ztFieldTwo") == (
        ("fieldTwo", "Engine.Core.Capability.ZetaView",
         "ZetaViewCapability"),),
           f"and so must `(toReadOnlyRef (accessor env))`, got: "
           f"{accessors.get('ztFieldTwo')}")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(violations == [],
           f"a fully readable grouped WRAPPED projection must raise no "
           f"completeness violation, got: {violations}")


def test_an_unreadable_projection_binding_fails_closed():
    """Requirement 2, and the reason requirement 1 is not enough on its
    own: widening the canonicalizer can never be finished, so the
    spellings it does NOT read must fail loudly instead of vanishing.

    An unrecognized wrapper might copy and an operator expression might
    be anything, so neither canonicalizes -- and both are named, with
    their module, projection and field, rather than leaving the
    selector quietly out of the accessor map."""
    sources = _writer_sources(eta=_UNREADABLE_PROJECTION)

    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect("etFieldOne" in accessors
           and "etFieldTwo" not in accessors
           and "etFieldThree" not in accessors,
           f"the map must still refuse to INVENT an alias for an "
           f"unreadable binding, got: {sorted(accessors)}")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 2,
           f"exactly the two unreadable bindings must be reported, got: "
           f"{violations}")
    for field in ("etFieldTwo", "etFieldThree"):
        expect(any(field in v
                   and "Engine.Core.Capability.Eta" in v
                   and "toEtaCapability" in v
                   for v in violations),
               f"the violation for `{field}` must name the capability "
               f"module, projection and field, got: {violations}")
    expect(not any("etFieldOne" in v for v in violations),
           f"the readable binding beside them must not be reported, got: "
           f"{violations}")

    # The refusal itself, pinned directly. Dropping unrecognized
    # characters instead of refusing on them would leave the two
    # bindings above unreadable by accident -- their operators happen
    # to sit beside a THIRD identifier -- while quietly canonicalizing
    # a two-identifier operator expression that shares no handle at
    # all. Each of these applies SOMETHING to `env` that is not the
    # accessor, and none may reach a field.
    for expression in ("pickRef <$> env",
                       "toReadOnlyRef $ fieldOne env",
                       "fieldOne <$> pure env",
                       "either fieldOne fieldTwo env",
                       "fieldOne @Int env",
                       "(fieldOne env",
                       "fieldOne env (",
                       "fieldOne env)"):
        expect(canonical_projection_accessor(expression) is None,
               f"`{expression}` names no accessor this audit can read, so "
               f"it must not canonicalize; got: "
               f"{canonical_projection_accessor(expression)}")


def test_a_capability_record_with_no_discoverable_projection_fails_closed():
    """The same hole one level up. A record whose projection the audit
    cannot find loses EVERY selector at once, which is strictly worse
    than one unreadable field -- so an undiscoverable projection is a
    violation naming the record, never a module quietly skipped."""
    sources = _writer_sources(theta=_UNPROJECTED_CAPABILITY)
    records = {entry.record: entry.projection
               for entry in discover_capability_records(sources)}
    expect(records.get("ThetaCapability", "missing") is None,
           f"the record must be discovered WITHOUT a projection rather "
           f"than not discovered at all, got: {records}")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 1
           and "ThetaCapability" in violations[0]
           and "Engine.Core.Capability.Theta" in violations[0],
           f"an undiscoverable projection must be reported by record and "
           f"module, got: {violations}")


def test_a_projection_binding_onto_a_dead_accessor_fails_closed():
    """The reviewer's amendment to requirement 2.
    `capability_accessor_map` drops a parsed binding whose accessor is
    not a live `EngineEnv` field, at exactly the same cost as an
    unreadable one -- so a renamed or mistyped accessor must fail here
    too, naming the accessor it could not find."""
    sources = _writer_sources(iota=_DEAD_ACCESSOR_PROJECTION)
    expect("ioFieldRenamed" not in capability_accessor_map(
               sources, _WRITER_FIELDS),
           "the map must not carry a selector bound from a dead accessor")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 1
           and "ioFieldRenamed" in violations[0]
           and "fieldRenamed" in violations[0],
           f"a binding onto a name that is not a live EngineEnv field "
           f"must be reported by field and accessor, got: {violations}")


def test_projection_binding_expressions_keep_the_unreadable_ones():
    """The two parsers are deliberately different. The accessor map
    reads only what canonicalizes, but the completeness gate must see
    every binding the construction WRITES -- otherwise an unreadable
    one would be indistinguishable from an absent one and could not be
    quoted back in the failure message."""
    expressions = parse_projection_binding_expressions(
        _UNREADABLE_PROJECTION, "toEtaCapability")
    expect(expressions == {"etFieldOne": "fieldOne env",
                           "etFieldTwo": "snapshotOf (fieldTwo env)",
                           "etFieldThree": "chooseRef . pick $ env"},
           f"every binding must be returned verbatim, readable or not, "
           f"got: {expressions}")
    expect(parse_projection_binding_expressions(
               _UNREADABLE_PROJECTION, "toNothingCapability") == {},
           "and a projection with no equation returns nothing at all")


def test_projection_completeness_against_the_real_repo():
    """Requirement 3: the live tree passes. Every capability record has
    a discoverable projection, and every field it declares canonicalizes
    onto a live `EngineEnv` accessor -- so the gate added here is a
    ratchet on the real code, not a rule only fixtures satisfy."""
    sources = scan_production_sources(REPO_ROOT)
    live_fields = extract_record_fields(
        (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8"),
        ENGINE_ENV_PATTERN)
    records = discover_capability_records(sources)
    expect(len(records) >= 14,
           f"every capability module must contribute a record, got: "
           f"{len(records)}")
    expect([entry.module for entry in records
            if entry.projection is None] == [],
           "every live capability record must have a discoverable "
           "projection")
    violations = audit_capability_projection_completeness(
        sources, live_fields)
    expect(violations == [],
           f"the real repository must raise no capability projection "
           f"completeness violation, got: {violations}")


def test_a_capability_record_is_found_whatever_syntax_declares_it():
    """A capability type is recognized by its NAME and its
    `data`/`newtype` keyword, never by the shape of its body.

    GHC2024 enables `GADTs`, so
    `data X where X ∷ { ... } → X` declares the very same record --
    same selectors, same scope -- as `data X = X { ... }`, and a
    one-field record may be a `newtype`. Matching only the ordinary
    form left both records undiscovered, which is the same silent
    omission #2059 closes but one level up and strictly worse: the
    record reached neither the accessor map nor the completeness gate,
    so a direct write through its selector was filed as `other` and the
    audit exited 0 with nothing to report."""
    sources = _writer_sources(kappa=_GADT_PROJECTION,
                              kappaConsumer=_GADT_CONSUMER)
    expect(capability_record_fields(_GADT_PROJECTION, "KappaCapability")
           == ["kaFieldOne", "kaFieldTwo"],
           f"a GADT record's selectors must be enumerated, got: "
           f"{capability_record_fields(_GADT_PROJECTION, 'KappaCapability')}")

    records = {entry.record: entry.projection
               for entry in discover_capability_records(sources)}
    expect(records.get("KappaCapability") == "toKappaCapability",
           f"the GADT record and its projection must be discovered, got: "
           f"{records}")

    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("kaFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.Kappa", "KappaCapability"),)
           and accessors.get("kaFieldTwo") == (
        ("fieldTwo", "Engine.Core.Capability.Kappa", "KappaCapability"),),
           f"both GADT selectors must canonicalize, got: "
           f"{accessors.get('kaFieldOne')}, {accessors.get('kaFieldTwo')}")
    expect(audit_capability_projection_completeness(
               sources, _WRITER_FIELDS) == [],
           "a fully readable GADT projection must raise no completeness "
           "violation")

    writes, _ = _scan(sources)
    expect(writes["fieldOne"] == {"Kappa.Mod"},
           f"and the write through the GADT selector must be attributed, "
           f"got: {sorted(writes['fieldOne'])}")
    declared = {"fieldOne": frozenset(), "fieldTwo": frozenset(),
                "fieldThree": frozenset()}
    rejected = audit_writer_modules(writes, _WRITER_FIELDS, declared=declared)
    expect(len(rejected) == 1 and "Kappa.Mod" in rejected[0],
           f"and the undeclared write must be rejected, got: {rejected}")

    newtype_sources = _writer_sources(**{"lambda": _NEWTYPE_PROJECTION})
    expect(capability_accessor_map(newtype_sources, _WRITER_FIELDS).get(
               "laFieldThree") == (
        ("fieldThree", "Engine.Core.Capability.Lambda",
         "LambdaCapability"),),
           "a `newtype` capability record must canonicalize the same way")
    expect(audit_capability_projection_completeness(
               newtype_sources, _WRITER_FIELDS) == [],
           "and raise no completeness violation")


def test_an_indented_capability_module_is_fully_enforced():
    """A module's layout column is set by the first token after
    `where` and need not be zero, so every top-level declaration of a
    uniformly indented module sits at a non-zero column. Anchoring
    discovery at column zero made such a module invisible end to end --
    no record, no accessor map entry, no completeness violation, and a
    consumer's `writeIORef` through its selector filed as `other`.

    The declaration SPAN has to follow the same column, or the fix
    trades one silent failure for a false one: measured from column
    zero, the record's span would run to the end of an all-indented
    file and report the next declaration's fields as its own."""
    sources = _writer_sources(rho=_INDENTED_MODULE,
                              rhoConsumer=_INDENTED_CONSUMER)
    records = {entry.record: entry.projection
               for entry in discover_capability_records(sources)}
    expect(records.get("RhoCapability") == "toRhoCapability",
           f"an indented declaration and its indented projection must "
           f"both be found, got: {records}")
    expect(capability_record_fields(_INDENTED_MODULE, "RhoCapability")
           == ["rhFieldOne"],
           f"and the span must stop at the next declaration in the same "
           f"column, got: "
           f"{capability_record_fields(_INDENTED_MODULE, 'RhoCapability')}")
    expect(capability_accessor_map(sources, _WRITER_FIELDS).get(
               "rhFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.Rho", "RhoCapability"),),
           "the indented projection must canonicalize")
    expect(audit_capability_projection_completeness(
               sources, _WRITER_FIELDS) == [],
           "and raise no completeness violation")

    writes, _ = _scan(sources)
    expect(writes["fieldOne"] == {"Rho.Mod"},
           f"and the write through its selector must be attributed, got: "
           f"{sorted(writes['fieldOne'])}")
    declared = {"fieldOne": frozenset(), "fieldTwo": frozenset(),
                "fieldThree": frozenset()}
    rejected = audit_writer_modules(writes, _WRITER_FIELDS, declared=declared)
    expect(len(rejected) == 1 and "Rho.Mod" in rejected[0],
           f"and the undeclared write must be rejected, got: {rejected}")


def test_an_unmodelled_capability_declaration_fails_closed():
    """The backstop, and the reason this discovery is a CLOSED set
    rather than a list of spellings someone happened to think of.

    Every hole closed here had one shape: a legal declaration the
    pattern did not match, so the record reached neither the accessor
    map nor the completeness gate and a write through its selector was
    filed as `other` while the audit exited 0. Naming the
    `data`/`newtype` keyword and a `<Name>Capability` type is enough to
    know a capability record is THERE; whether this audit can read its
    fields is a separate question, and the honest answer to "no" is to
    fail. So the NEXT unmodelled spelling -- whatever it is -- stops the
    gate instead of quietly disarming it."""
    sources = _writer_sources(sigma=_UNMODELLED_DECLARATION)
    missed = undiscovered_capability_declarations(sources)
    expect([record for _, _, record in missed] == ["SigmaCapability"],
           f"a declaration the pattern cannot read must still be seen, "
           f"got: {missed}")
    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 1
           and "SigmaCapability" in violations[0]
           and "cannot read" in violations[0],
           f"and must be reported by module and record, got: {violations}")

    # The named forms SS2.1 does not describe, each reported rather
    # than modelled: this is the "detect and fail" half, and it is what
    # lets the strict pattern stay small without leaving a hole.
    header = "module Engine.Core.Capability.Sigma where\n\n"
    body = "  { sgFieldOne ∷ IORef Int\n  }\n"
    for head in ("data instance SigmaCapability Int = SigmaCapability\n",
                 "newtype instance SigmaCapability Int = SigmaCapability\n",
                 "data instance Envelope SigmaCapability = SigmaCapability\n"):
        reported = audit_capability_projection_completeness(
            _writer_sources(sigma=header + head + body), _WRITER_FIELDS)
        expect(len(reported) == 1 and "SigmaCapability" in reported[0],
               f"`{head.strip()}` must be reported, got: {reported}")
    family = audit_capability_projection_completeness(
        _writer_sources(
            sigma=header + "data family SigmaCapability ∷ Type → Type\n"),
        _WRITER_FIELDS)
    expect(len(family) == 1 and "SigmaCapability" in family[0],
           f"and so must a `data family` naming one, got: {family}")


def test_a_capability_typed_field_is_not_a_declaration():
    """The backstop's own false-positive trap. Naming a capability
    RECORD as a field's type -- a context record holding
    `RenderCapability`, which is exactly D-7's shipped pass-on shape --
    declares nothing, and reporting it would make the gate cry wolf on
    the very pattern the residue exists to measure."""
    sources = _writer_sources(tau=_CAPABILITY_TYPED_FIELDS)
    expect(undiscovered_capability_declarations(sources) == [],
           f"a capability-typed FIELD must not read as a declaration, "
           f"got: {undiscovered_capability_declarations(sources)}")
    expect(audit_capability_projection_completeness(
               sources, _WRITER_FIELDS) == [],
           "and must raise no violation")


def test_the_real_repo_declares_no_unreadable_capability_record():
    """The backstop against the live tree: every capability record it
    declares is one this audit actually reads, so the ratchet is on the
    real code rather than only on fixtures."""
    expect(undiscovered_capability_declarations(
               scan_production_sources(REPO_ROOT)) == [],
           "the real repository must declare no capability record this "
           "audit cannot read")


def test_every_record_constructor_s_selectors_are_enumerated():
    """A capability type may declare more than one record constructor,
    and every constructor's selectors live in ONE scope -- so reading
    only the first block left the rest unenumerated and therefore
    unchecked.

    That is #2059's own failure mode one level up: the completeness
    gate had nothing to say about a field it never knew existed, so a
    projection binding it through anything the canonicalizer cannot
    read took the selector out of the accessor map silently, and an
    undeclared write through it produced no violation at all. Both
    directions are pinned here: the field must be ENFORCED when its
    binding is readable, and must FAIL LOUDLY when it is not."""
    expect(capability_record_fields(_SUM_PROJECTION, "OmegaCapability")
           == ["omFieldOne", "omFieldTwo"],
           f"every constructor's selectors must be enumerated, once each "
           f"in first-declaration order, got: "
           f"{capability_record_fields(_SUM_PROJECTION, 'OmegaCapability')}")
    expect(capability_record_fields(_GADT_SUM_PROJECTION, "PsiCapability")
           == ["psFieldOne", "psFieldTwo"],
           f"and the same for a GADT declaring one record constructor "
           f"per line, got: "
           f"{capability_record_fields(_GADT_SUM_PROJECTION, 'PsiCapability')}")

    # Readable binding: the later constructor's selector is enforced.
    readable = _writer_sources(omega=_SUM_PROJECTION,
                               omegaConsumer=_SUM_CONSUMER)
    expect(capability_accessor_map(readable, _WRITER_FIELDS).get(
               "omFieldTwo") == (
        ("fieldTwo", "Engine.Core.Capability.Omega", "OmegaCapability"),),
           "a later constructor's selector must canonicalize")
    expect(audit_capability_projection_completeness(
               readable, _WRITER_FIELDS) == [],
           "and raise no completeness violation when its binding reads")
    writes, _ = _scan(readable)
    expect(writes["fieldTwo"] == {"Omega.Mod"},
           f"and the write through it must be attributed, got: "
           f"{sorted(writes['fieldTwo'])}")
    declared = {"fieldOne": frozenset(), "fieldTwo": frozenset(),
                "fieldThree": frozenset()}
    rejected = audit_writer_modules(writes, _WRITER_FIELDS, declared=declared)
    expect(len(rejected) == 1 and "Omega.Mod" in rejected[0],
           f"and the undeclared write must be rejected, got: {rejected}")

    # Unreadable binding: the selector leaves the map, so the gate must
    # be the thing that stops -- otherwise the write below is untracked.
    hidden = _writer_sources(omega=_SUM_HIDDEN_PROJECTION,
                             omegaConsumer=_SUM_CONSUMER)
    expect("omFieldTwo" not in capability_accessor_map(
               hidden, _WRITER_FIELDS),
           "a binding through a `where`-bound helper must not be guessed "
           "at")
    hidden_writes, _ = _scan(hidden)
    expect(hidden_writes["fieldTwo"] == set(),
           f"so the write through it is genuinely unattributed, got: "
           f"{sorted(hidden_writes['fieldTwo'])} -- which is exactly why "
           f"the completeness gate must fail")
    violations = audit_capability_projection_completeness(
        hidden, _WRITER_FIELDS)
    expect(len(violations) == 1 and "omFieldTwo" in violations[0],
           f"and the completeness gate must report `omFieldTwo` by name, "
           f"got: {violations}")

    # The unprojected constructor's field is reported the same way.
    gadt_violations = audit_capability_projection_completeness(
        _writer_sources(psi=_GADT_SUM_PROJECTION), _WRITER_FIELDS)
    expect(len(gadt_violations) == 1 and "psFieldTwo" in gadt_violations[0],
           f"a selector no binding covers must be reported by name, got: "
           f"{gadt_violations}")


def test_a_capability_type_with_no_record_block_fails_closed():
    """Recognizing a declaration by name is separated from reading its
    fields, so a `<Name>Capability` whose declaration carries no record
    block is a violation rather than a skip -- and the audit must not
    borrow the braces of a LATER declaration and report ITS field as
    this record's."""
    sources = _writer_sources(nu=_BLOCKLESS_CAPABILITY)
    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 1
           and "NuCapability" in violations[0]
           and "record block" in violations[0],
           f"a capability type with no readable record block must be "
           f"reported, got: {violations}")
    expect(not any("borrowed" in v for v in violations),
           f"and the unrelated record's field must not be read as its "
           f"own, got: {violations}")


def test_a_read_only_ref_read_is_an_inline_use_not_a_pass_on():
    """`readReadOnlyRef` consumes the handle exactly as `readIORef`
    does, so a migrated reader is an inline use. Without that, every
    reader moved onto a wrapped view would be recounted as a pass-on and
    the residue would inflate by the size of the migration -- reporting
    the OPPOSITE of what the migration did."""
    sources = _writer_sources(deltaView=_WRAPPED_PROJECTION,
                              wrappedReader=_WRAPPED_READER)
    _, residue = _scan(sources)
    expect([r for r in residue if r.module == "WrappedReader.Mod"] == [],
           f"an inline read of a wrapped field is not residue, got: "
           f"{[r for r in residue if r.module == 'WrappedReader.Mod']}")

    # ...and the pass-on it is contrasted with still IS residue, or the
    # rule above would have been achieved by simply going blind.
    sources = _writer_sources(deltaView=_WRAPPED_PROJECTION,
                              wrappedPassOn=_WRAPPED_PASS_ON)
    _, residue = _scan(sources)
    passed = [(r.accessor, r.field) for r in residue
              if r.module == "WrappedPassOn.Mod"]
    expect(passed == [("dvFieldOne", "fieldOne")],
           f"storing a wrapped handle in a context record must stay "
           f"residue, got: {passed}")

    # The primitive is held to the scope rule too: a module-local
    # `readReadOnlyRef` is a different function, so the accessor beside
    # it was not consumed here and stays residue.
    sources = _writer_sources(deltaView=_WRAPPED_PROJECTION,
                              localReadOnly=_LOCAL_READONLY_PRIMITIVE)
    _, residue = _scan(sources)
    expect([r.accessor for r in residue if r.module == "LocalReadOnly.Mod"]
           == ["dvFieldOne"],
           f"a module-local `readReadOnlyRef` is not the primitive, got: "
           f"{[r.accessor for r in residue if r.module == 'LocalReadOnly.Mod']}")

    expect(resolve_primitive(
        parse_imports("import Engine.Core.ReadOnlyRef (readReadOnlyRef)\n"),
        "readReadOnlyRef") == "readReadOnlyRef",
           "the read-only read resolves through its own defining module")
    expect(resolve_primitive(parse_imports("import Data.IORef\n"),
                             "readReadOnlyRef") is None,
           "and `Data.IORef` does not put it in scope")


def test_one_selector_may_belong_to_two_capabilities():
    """A selector name is only unique within its own record. Two
    capability modules may both export `sharedRef`, and the consumer's
    own imports say which one it means -- so every candidate owner is
    offered the scope test rather than one arbitrarily winning and the
    write being dropped as somebody else's."""
    sources = _writer_sources(alpha=_ALPHA_CAPABILITY,
                              beta=_BETA_CAPABILITY,
                              collideA=_ALPHA_CONSUMER,
                              collideB=_BETA_CONSUMER)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors["sharedRef"] == (
        ("fieldOne", "Engine.Core.Capability.Alpha", "AlphaCapability"),
        ("fieldTwo", "Engine.Core.Capability.Beta", "BetaCapability"),
    ), f"both owners must survive, sorted, got: "
       f"{accessors.get('sharedRef')}")

    # One consumer per record, so neither candidate order can be right
    # by luck: each write must land on the field of the capability that
    # consumer actually imported.
    writes, _ = _scan(sources)
    expect(writes["fieldOne"] == {"CollideA.Mod"},
           f"the `Alpha` consumer writes `Alpha`'s field, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"CollideB.Mod"},
           f"and the `Beta` consumer writes `Beta`'s, got: "
           f"{sorted(writes['fieldTwo'])}")


def test_a_primitive_must_be_the_one_from_data_ioref():
    """The primitive is held to the same scope rule as the accessor. A
    module-local `writeIORef`, or an unrelated module's qualified
    homonym, is a different function whose argument mutates no `IORef`
    -- attributing it would fabricate a write, and then an undeclared
    writer or a stale map entry, out of code that performs none."""
    writes, _ = _scan(_writer_sources(localPrim=_LOCAL_PRIMITIVE))
    expect(writes["fieldOne"] == set(),
           f"a module-local `writeIORef` is not the primitive, got: "
           f"{sorted(writes['fieldOne'])}")

    writes, _ = _scan(_writer_sources(qualHomonym=_QUALIFIED_HOMONYM))
    expect(writes["fieldOne"] == set(),
           f"`Other.writeIORef` is whatever `Other` exports, not "
           f"`Data.IORef`'s, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"QualHomonym.Mod"},
           f"but the genuine qualified primitive in the same module must "
           f"still be read, got: {sorted(writes['fieldTwo'])}")

    declarations = parse_imports(
        "import qualified Data.IORef as Ref\n")
    expect(resolve_primitive(declarations, "Ref.writeIORef") == "writeIORef",
           "a qualified primitive resolves through its alias")
    expect(resolve_primitive(declarations, "writeIORef") is None,
           "and a qualified-only import does not put the bare spelling "
           "in scope")
    expect(resolve_primitive(parse_imports("import Data.IORef\n"),
                             "writeIORef") == "writeIORef",
           "a bare import does")

    # The only TOP-LEVEL homonym that compiles: `hiding` the primitive
    # and defining one. An unqualified import beside a local definition
    # is an ambiguous occurrence at every use site, so it cannot reach
    # this scan at all. A LOCAL shadow -- a `let`, a `where`, a lambda
    # parameter -- is the mirror of an accessor shadowed the same way,
    # and requirement 7 sends both to SHADOW_EXEMPTIONS rather than to
    # a scope analysis.
    writes, _ = _scan(_writer_sources(shadowPrim=_SHADOWED_PRIMITIVE))
    expect(writes["fieldOne"] == set(),
           f"a hidden primitive leaves the module's own helper standing "
           f"alone, and it mutates nothing, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"ShadowPrim.Mod"},
           f"while the primitives it did NOT hide still read, got: "
           f"{sorted(writes['fieldTwo'])}")


def test_a_shadow_exemption_suppresses_only_its_own_pair():
    """Requirement 7's mechanism. An exemption suppresses exactly the
    module/field pair it names -- the same module's other writes are
    untouched -- and is itself checked, so it cannot quietly outlive
    what it was suppressing."""
    sources = _writer_sources(declared=_DECLARED_WRITER)
    admitted = {("Consumer.Mod", "fieldOne"):
                "`fieldOne` here is the equation's own parameter"}
    scan = _full_scan(sources, exemptions=admitted)
    expect(scan.writes["fieldOne"] == set(),
           f"the exempted pair must not be attributed, got: "
           f"{sorted(scan.writes['fieldOne'])}")
    expect(scan.writes["fieldTwo"] == {"Consumer.Mod"},
           f"but the same module's OTHER write must survive, got: "
           f"{sorted(scan.writes['fieldTwo'])}")
    expect(scan.suppressed == frozenset({("Consumer.Mod", "fieldOne")}),
           f"and the suppression must be recorded, got: {scan.suppressed}")
    expect(audit_shadow_exemptions(scan.suppressed, _WRITER_FIELDS,
                                   exemptions=admitted) == [],
           "a live, reasoned exemption is valid")


def test_shadow_exemptions_are_validated():
    """Its three failure modes: a field that is not live, a reason that
    says nothing, and an entry that no longer suppresses anything."""
    sources = _writer_sources(declared=_DECLARED_WRITER)
    suppressed = _full_scan(sources).suppressed

    unknown = {("Consumer.Mod", "fieldGone"): "a real reason"}
    violations = audit_shadow_exemptions(
        _full_scan(sources, exemptions=unknown).suppressed,
        _WRITER_FIELDS, exemptions=unknown)
    expect(len(violations) == 1 and "fieldGone" in violations[0]
           and "not a live `EngineEnv` field" in violations[0],
           f"an exemption naming a dead field must fail as such, not "
           f"merely as a stale one, got: {violations}")

    for reason in ("", "   ", "TBD"):
        blank = {("Consumer.Mod", "fieldOne"): reason}
        violations = audit_shadow_exemptions(
            _full_scan(sources, exemptions=blank).suppressed,
            _WRITER_FIELDS, exemptions=blank)
        expect(len(violations) == 1 and "no real reason" in violations[0],
               f"reason {reason!r} must fail, got: {violations}")

    stale = {("Nobody.Mod", "fieldOne"): "a real reason"}
    violations = audit_shadow_exemptions(
        suppressed, _WRITER_FIELDS, exemptions=stale)
    expect(len(violations) == 1 and "no such write is detected"
           in violations[0],
           f"an exemption that suppresses nothing must fail, got: "
           f"{violations}")


def test_writer_map_against_the_real_repo():
    """The live gate, asserted against the REAL tree and the REAL
    checked-in map: every field is mapped, no undeclared write, no
    stale entry, and the residue is a deterministic, non-empty
    measurement."""
    engine_env_source = (REPO_ROOT / ENGINE_ENV_FILE).read_text(
        encoding="utf-8")
    live_fields = extract_record_fields(engine_env_source,
                                        ENGINE_ENV_PATTERN)
    sources = scan_production_sources(REPO_ROOT)
    scan = scan_capability_writes(sources, live_fields)
    writes, residue = scan.writes, scan.residue

    expect(set(CAPABILITY_WRITER_MODULES) == set(live_fields),
           f"CAPABILITY_WRITER_MODULES' keys must equal the live EngineEnv "
           f"field set; extra: "
           f"{sorted(set(CAPABILITY_WRITER_MODULES) - set(live_fields))}, "
           f"missing: "
           f"{sorted(set(live_fields) - set(CAPABILITY_WRITER_MODULES))}")

    violations = audit_writer_modules(writes, live_fields)
    expect(violations == [],
           f"the real tree must have no undeclared or stale writing-module "
           f"entry, got: {violations}")

    exempt = set(PERMANENT_IMPORTERS) | {PERMANENT_DEFINER}
    leaked = sorted({module
                     for modules in CAPABILITY_WRITER_MODULES.values()
                     for module in modules} & exempt)
    expect(leaked == [],
           f"no SS6.1 permanent module may appear in the map -- D-4 puts "
           f"them outside this boundary entirely, got: {leaked}")

    expect(residue and residue == sorted(residue),
           "the residue must be non-empty and deterministically ordered")
    expect(all(item.field in set(live_fields) for item in residue),
           "every residue entry must canonicalize to a live EngineEnv field")

    expect(scan_capability_writes(sources, live_fields).writes == writes,
           "the scan must be deterministic across runs")

    unclassified = audit_mutation_sites(scan.sites)
    expect(unclassified == [],
           f"every mutation-primitive occurrence in the real tree must "
           f"classify -- requirement 6's whole point is that an "
           f"unreadable site fails instead of vanishing; got: "
           f"{unclassified[:3]}")
    expect(all(site.kind in ("write", "other") for site in scan.sites)
           and len(scan.sites) > len(
               [s for s in scan.sites if s.kind == "write"]),
           "the site census covers both attributed and ignored sites")
    expect(SHADOW_EXEMPTIONS == {},
           f"SHADOW_EXEMPTIONS is empty in this tree -- the two shape "
           f"rules separate every real case; got: {SHADOW_EXEMPTIONS}")
    expect(audit_shadow_exemptions(scan.suppressed, live_fields) == [],
           "and an empty exemption list has nothing to be stale about")


def main() -> int:
    tests = [
        test_complete_inventory_has_no_violations,
        test_missing_row_detected,
        test_duplicate_row_detected,
        test_stale_row_detected,
        test_unknown_capability_heading_detected,
        test_row_with_no_enclosing_heading_detected,
        test_malformed_capability_heading_resets_scope,
        test_unknown_lifecycle_detected,
        test_unknown_thread_role_detected,
        test_mixed_valid_and_unknown_role_detected,
        test_bare_unquoted_unknown_role_detected,
        test_lower_camel_unknown_role_detected,
        test_conjunction_joined_unknown_role_detected,
        test_wrong_shaped_quoted_role_detected,
        test_arbitrary_joiner_unknown_role_detected,
        test_blank_reader_decision_detected,
        test_unjustified_none_writer_detected,
        test_whitespace_only_none_justification_detected,
        test_justified_none_writer_accepted,
        test_missing_sync_contract_detected,
        test_blank_init_shutdown_notes_detected,
        test_em_dash_notes_accepted,
        test_missing_grounding_evidence_detected,
        test_valid_multi_reader_multi_writer_field_passes,
        test_parse_inventory_only_scans_section_5,
        test_audit_against_the_real_repo,
        test_bare_import_detected,
        test_bare_import_qualified_and_aliased_detected,
        test_bare_import_multiline_detected,
        test_bare_import_as_last_import_not_swallowed_by_later_code,
        test_explicit_engineenv_ordinary_qualified_and_multiline_detected,
        test_narrow_import_not_classified_unrestricted,
        test_no_state_import_returns_none,
        test_section_6_2_parser_extracts_real_assignments,
        test_section_6_2_parser_ignores_explanatory_backtick_references,
        test_new_full_access_module_rejected,
        test_shrinking_migration_accepted,
        test_documented_but_ungoverned_addition_still_rejected,
        test_stale_ceiling_entry_rejected,
        test_stale_permanent_importer_rejected,
        test_ceiling_and_doc_mismatch_detected,
        test_real_repo_ratchet_consistency,
        test_section_6_1_parser_reads_first_column_only,
        test_section_6_1_matching_constants_accepted,
        test_section_6_1_undocumented_permanent_importer_rejected,
        test_section_6_1_documented_but_ungoverned_module_rejected,
        test_section_6_1_placeholder_justification_rejected,
        test_section_6_1_missing_table_rejected,
        test_save_load_projection_clean_case_passes,
        test_save_load_projection_transposed_binding_rejected,
        test_save_load_projection_missing_field_rejected,
        test_save_load_projection_extra_field_rejected,
        test_save_load_projection_unlisted_in_cabal_rejected,
        test_save_load_projection_missing_module_rejected,
        test_save_load_projection_ignores_haddock_bindings,
        test_boundary_clean_tree_has_no_violations,
        test_boundary_worker_importing_full_capability_rejected,
        test_boundary_non_owner_naming_engine_state_ref_rejected,
        test_boundary_engine_state_ref_in_a_comment_is_not_a_violation,
        test_boundary_view_carrying_engine_state_ref_rejected,
        test_boundary_missing_view_module_rejected,
        test_boundary_stale_main_only_entry_rejected,
        test_boundary_stale_state_ref_owner_rejected,
        test_real_repo_render_boundary_holds,
        test_input_boundary_clean_tree_has_no_violations,
        test_input_boundary_worker_importing_full_capability_rejected,
        test_input_boundary_non_owner_naming_allocator_rejected,
        test_input_boundary_non_owner_naming_current_key_rejected,
        test_input_boundary_watermark_is_not_confused_with_allocator,
        test_input_boundary_private_field_in_a_comment_is_not_a_violation,
        test_input_boundary_view_carrying_private_field_rejected,
        test_input_boundary_missing_view_module_rejected,
        test_input_boundary_stale_lua_only_entry_rejected,
        test_input_boundary_stale_field_owner_rejected,
        test_real_repo_input_boundary_holds,
        test_field_total_clean_fixture_accepted,
        test_field_total_stale_count_rejected_while_rows_stay_synchronized,
        test_field_total_synchronized_rows_alone_do_not_save_a_stale_block,
        test_field_total_missing_block_rejected,
        test_field_total_duplicate_block_rejected,
        test_field_total_unclosed_block_rejected,
        test_field_total_reintroduced_line_anchor_rejected,
        test_field_total_absent_number_rejected,
        test_field_total_wrong_span_field_rejected,
        test_field_total_reversed_span_rejected,
        test_field_total_missing_one_row_contract_rejected,
        test_field_total_section_references_are_not_counts,
        test_procedure_item_reintroduced_total_rejected,
        test_procedure_item_agreeing_total_still_rejected,
        test_procedure_item_total_in_its_tail_rejected,
        test_procedure_item_reworded_away_rejected,
        test_procedure_item_displaced_by_a_new_first_item_rejected,
        test_procedure_item_missing_section_rejected,
        test_procedure_item_later_items_may_count_legitimately,
        test_scope_block_must_be_section_ones_first_content,
        test_scope_section_may_state_no_other_number,
        test_scope_section_allows_code_spans_and_references,
        test_scope_section_code_span_total_rejected,
        test_procedure_item_code_span_total_rejected,
        test_source_location_spans_stay_exempt,
        test_stray_engineenv_total_anywhere_rejected,
        test_bare_field_counts_elsewhere_are_not_flagged,
        test_field_total_block_outside_section_one_rejected,
        test_field_total_renamed_section_heading_rejected,
        test_fenced_heading_does_not_end_the_scope_section,
        test_fenced_heading_does_not_end_the_procedure_section,
        test_fenced_scope_heading_does_not_start_the_section,
        test_tilde_fences_and_longer_closers_are_handled,
        test_real_inventory_fenced_heading_escape_rejected,
        test_section_bounds_stops_at_the_next_peer_heading,
        test_section_bounds_keeps_subsections_inside_a_top_level_section,
        test_field_total_against_the_real_repo,
        test_real_repo_end_state,
        test_writer_map_canonicalizes_both_consumer_shapes,
        test_writer_map_accepts_a_declared_write,
        test_writer_map_rejects_an_undeclared_write,
        test_writer_map_rejects_a_stale_entry,
        test_writer_map_keys_track_the_live_field_set,
        test_permanent_module_writes_are_exempt,
        test_passed_on_handle_is_residue_not_a_write,
        test_out_of_scope_names_are_not_writes,
        test_comments_and_bare_arguments_are_not_writes,
        test_backticked_infix_mutations_are_writes,
        test_redundant_parentheses_change_nothing,
        test_parentheses_around_the_accessor_change_nothing,
        test_visible_type_applications_are_skipped,
        test_strict_application_groups_like_lazy,
        test_a_first_argument_must_be_applied,
        test_qualified_accessors_are_resolved,
        test_a_qualifier_must_name_the_owning_module,
        test_qualified_mutation_primitives_are_recognized,
        test_a_hiding_clause_removes_a_name_from_scope,
        test_a_qualified_only_import_excludes_the_bare_spelling,
        test_import_declarations_record_qualification_and_alias,
        test_a_bare_argument_surfaces_as_residue,
        test_import_declarations_are_not_uses,
        test_multiline_expressions_are_scanned,
        test_tokenizer_skips_literals_and_keeps_line_numbers,
        test_every_recognized_primitive_is_read,
        test_a_bare_import_brings_the_accessor_into_scope,
        test_an_unreadable_mutation_site_blocks,
        test_a_primitive_must_be_in_head_position,
        test_an_operator_section_applying_a_primitive_blocks,
        test_record_dot_access_is_unclassifiable,
        test_a_primitive_used_as_a_value_is_not_unreadable,
        test_a_comment_marker_inside_a_string_is_text,
        test_token_lines_survive_a_string_gap,
        test_a_wildcard_grants_only_its_own_type_s_selectors,
        test_a_projection_may_name_its_accessor_qualified,
        test_a_view_field_wrapped_by_a_named_alias_wrapper_canonicalizes,
        test_a_redundantly_grouped_projection_canonicalizes_and_is_enforced,
        test_a_redundantly_grouped_wrapped_projection_canonicalizes,
        test_an_unreadable_projection_binding_fails_closed,
        test_a_capability_record_with_no_discoverable_projection_fails_closed,
        test_a_projection_binding_onto_a_dead_accessor_fails_closed,
        test_projection_binding_expressions_keep_the_unreadable_ones,
        test_projection_completeness_against_the_real_repo,
        test_a_capability_record_is_found_whatever_syntax_declares_it,
        test_an_indented_capability_module_is_fully_enforced,
        test_an_unmodelled_capability_declaration_fails_closed,
        test_a_capability_typed_field_is_not_a_declaration,
        test_the_real_repo_declares_no_unreadable_capability_record,
        test_every_record_constructor_s_selectors_are_enumerated,
        test_a_capability_type_with_no_record_block_fails_closed,
        test_a_read_only_ref_read_is_an_inline_use_not_a_pass_on,
        test_one_selector_may_belong_to_two_capabilities,
        test_a_primitive_must_be_the_one_from_data_ioref,
        test_a_shadow_exemption_suppresses_only_its_own_pair,
        test_shadow_exemptions_are_validated,
        test_writer_map_against_the_real_repo,
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
