#!/usr/bin/env python3
"""Unit tests for engine_env_capability_audit.py (issue #876 acceptance:
the audit detects an intentionally introduced capability-inventory gap
using synthetic fixtures, never by editing the real EngineEnv or the
real inventory doc).

Mirrors tools/test_persistence_inventory_audit.py's own approach: feed
the audit's pure functions synthetic Haskell record text and a
synthetic inventory doc, so these tests stay stable regardless of how
EngineEnv or the real inventory doc grow.

This is the AGGREGATE self-test -- the one command CI and
tools/ci-local.sh run for the whole engine_env_capability_audit.py gate.
The cases defined here cover the older half of that gate (the inventory
rows, the SS1 field total, the SS6 ratchet and SS6.1 boundary, the SS3/
SS7.3 thread boundaries, the E8 save-load projection, the end state).
The SS5 writing-module scanner's contract (issue #1892) lives in
tools/test_engine_env_capability_writers.py since issue #2036, and is
run FROM HERE: `main` appends that module's `TESTS` registry to its
own list and runs every case in this process, so the two halves share
one `selftestlib.FAILURES` collector and one exit status. A failing
writer-scanner assertion therefore still fails this command and is
named in its output, exactly as before the split -- delegation that
ran the focused module but did not propagate its failures would have
silently disabled the CI-visible half of the gate.

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
    CAPABILITIES, PERMANENT_IMPORTERS, TEMPORARY_CEILING,
    parse_permanent_boundary, audit_permanent_boundary,
    audit_field_total, extract_marked_spans, section_bounds,
    FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE, ONE_ROW_PHRASE,
    SECTION_1_HEADING, SECTION_6_2_HEADING, PROCEDURE_ITEM_ANCHOR,
    audit_save_load_projection, parse_projection_bindings,
    SAVE_LOAD_CAPABILITY_MODULE, SAVE_LOAD_CAPABILITY_FILE,
    SAVE_LOAD_FIELD_MAP, SAVE_LOAD_PROJECTION,
    _import_chunks, _strip_haskell_comments,
)
from persistence_inventory_audit_haskell import extract_record_fields  # type: ignore
# The writer-scanner half of this gate (#1892), owned by its own module
# since #2036 and run from here so CI keeps exactly one self-test
# command; see that module's docstring for the delegation contract.
from test_engine_env_capability_writers import TESTS as WRITER_TESTS  # type: ignore

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402


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


def main() -> int:
    selftestlib.parse_verbose()
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
    ] + list(WRITER_TESTS)

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
