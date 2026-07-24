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
        test_boundary_clean_tree_has_no_violations,
        test_boundary_worker_importing_full_capability_rejected,
        test_boundary_non_owner_naming_engine_state_ref_rejected,
        test_boundary_engine_state_ref_in_a_comment_is_not_a_violation,
        test_boundary_view_carrying_engine_state_ref_rejected,
        test_boundary_missing_view_module_rejected,
        test_boundary_stale_main_only_entry_rejected,
        test_boundary_stale_state_ref_owner_rejected,
        test_real_repo_render_boundary_holds,
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
