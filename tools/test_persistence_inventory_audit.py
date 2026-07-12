#!/usr/bin/env python3
"""Unit tests for persistence_inventory_audit.py (issue #756 acceptance:
"the audit detects an intentionally introduced unclassified root state
owner or Lua persistence module in its own automated test").

Feeds the audit's pure functions synthetic Haskell record text, synthetic
Lua source, and a synthetic inventory doc -- never touches the real repo
files -- so these tests stay stable regardless of how EngineEnv or the
inventory grow.

Usage:
  python3 tools/test_persistence_inventory_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from persistence_inventory_audit import (  # type: ignore
    extract_record_fields, extract_lua_registered_modules,
    parse_classified_names, audit,
)

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


# ----- Fixtures --------------------------------------------------------

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

# Section-scoped inventory: section 1 classifies the EngineEnv fixture
# fields, section 7 classifies the Lua registry fixture.
SYNTHETIC_INVENTORY_COMPLETE = """\
# Fake inventory

## 1. `EngineEnv` fields

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | Exclude |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""

SYNTHETIC_INVENTORY_MISSING_ONE = """\
# Fake inventory

## 1. `EngineEnv` fields

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""

# fieldTwo is only classified under section 7 (as if it were a Lua
# module name), NOT under section 1 where the real EngineEnv.fieldTwo
# lives -- this must NOT satisfy EngineEnv.fieldTwo's requirement.
SYNTHETIC_INVENTORY_CROSS_SECTION_COLLISION = """\
# Fake inventory

## 1. `EngineEnv` fields

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

| Field | Classification |
|---|---|
| `fieldTwo` | Persist exactly (opaque blob) |
| `unit_ai` | Persist exactly (opaque blob) |
"""

SYNTHETIC_LUA_REGISTER = """\
local saveMods = require("scripts.lib.save_modules")

local function init()
    saveMods.register("unit_ai",
        function() return "blob" end,
        function(b) end)
end

-- saveMods.register("commented_out", nil, nil)
"""

# A record with an UNBALANCED brace inside a haddock comment (a lone
# `}`) -- if comments aren't stripped before brace-depth tracking, this
# closes the record block right after fieldOne and fieldTwo/fieldThree
# are never seen. This is the exact false-negative the audit must not
# have.
SYNTHETIC_ENGINE_ENV_UNBALANCED_COMMENT = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne   ∷ IORef Int
    -- ^ refers to an unrelated closing brace from other prose: cheese}
  , fieldTwo   ∷ IORef Text
  , fieldThree ∷ Q.Queue Int
  } deriving (Eq)
"""

# A record whose haddock comment contains a legally NESTED Haskell block
# comment (`{- outer {- inner -} still outer -}`) with an unmatched `}`
# left over after the inner comment's own close. A non-nesting stripper
# removes only up to the FIRST `-}` (the inner one), leaving " with a
# stray } here -}" in the text -- and that stray `}` would close the
# record block early, exactly like the unbalanced-comment case above,
# but only reachable via a legally nested comment.
SYNTHETIC_ENGINE_ENV_NESTED_COMMENT = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne   ∷ IORef Int
    {- outer comment {- inner -} with a stray } here -}
  , fieldTwo   ∷ IORef Text
  , fieldThree ∷ Q.Queue Int
  } deriving (Eq)
"""

# A register() call whose arguments span multiple lines.
SYNTHETIC_LUA_REGISTER_MULTILINE = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register(
    "multiline_module",
    function() return "blob" end,
    function(b) end)
"""

# A register() call written with whitespace around the dot (legal Lua).
SYNTHETIC_LUA_REGISTER_SPACED_DOT = """\
local saveMods = require("scripts.lib.save_modules")

saveMods . register("spaced_dot_module", nil, nil)
"""

# A string literal earlier on the SAME line contains `--`. A stripper
# that isn't string-aware treats that embedded `--` as a comment start
# and discards the real register() call that follows it.
SYNTHETIC_LUA_REGISTER_AFTER_DASH_STRING = """\
local saveMods = require("scripts.lib.save_modules")

local dash = "--"; saveMods.register("string_dash_module", nil, nil)
"""


# ----- Tests -------------------------------------------------------------

def test_extract_fields_from_brace_block():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV, r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "fieldTwo", "fieldThree"],
           f"extracts exactly the three EngineEnv fields, got {fields}")


def test_extract_fields_stray_brace_in_comment_is_harmless():
    # The haddock comment under fieldOne contains a literal `{...}` — the
    # depth tracker must not let prose braces close the block early.
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV, r"^data EngineEnv = EngineEnv\b")
    expect("fieldTwo" in fields and "fieldThree" in fields,
           "a brace inside a haddock comment doesn't truncate field extraction")


def test_extract_fields_unbalanced_brace_in_comment_does_not_truncate():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_UNBALANCED_COMMENT,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "fieldTwo", "fieldThree"],
           f"an UNBALANCED brace inside a haddock comment (a lone '}}') does not "
           f"prematurely close the record and drop later fields, got {fields}")


def test_extract_fields_nested_block_comment_does_not_truncate():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_NESTED_COMMENT,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "fieldTwo", "fieldThree"],
           f"a legally NESTED {{- -}} block comment (with a stray '}}' left "
           f"over from a non-nesting strip) does not truncate extraction, "
           f"got {fields}")


def test_extract_fields_ignores_other_records():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV, r"^data EngineEnv = EngineEnv\b")
    expect("unrelated" not in fields,
           "fields from a different record in the same file are not picked up")


def test_extract_fields_missing_record_raises():
    raised = False
    try:
        extract_record_fields(SYNTHETIC_ENGINE_ENV, r"^data NoSuchRecord = NoSuchRecord\b")
    except ValueError:
        raised = True
    expect(raised, "a record-start pattern that matches nothing raises ValueError")


def test_extract_lua_registered_modules():
    found = extract_lua_registered_modules({"scripts/fake.lua": SYNTHETIC_LUA_REGISTER})
    names = [n for n, _ in found]
    expect(names == ["unit_ai"],
           f"finds the live register() call and skips the commented-out one, got {names}")


def test_extract_lua_registered_modules_multiline_call():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_MULTILINE})
    names = [n for n, _ in found]
    expect(names == ["multiline_module"],
           f"finds a register() call whose arguments span multiple lines, got {names}")


def test_extract_lua_registered_modules_spaced_dot_call():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_SPACED_DOT})
    names = [n for n, _ in found]
    expect(names == ["spaced_dot_module"],
           f"finds a register() call written with whitespace around the dot, got {names}")


def test_extract_lua_registered_modules_block_commented_out():
    lua = SYNTHETIC_LUA_REGISTER + '\n--[[\nsaveMods.register("block_commented_out", nil, nil)\n]]\n'
    found = extract_lua_registered_modules({"scripts/fake.lua": lua})
    names = [n for n, _ in found]
    expect(names == ["unit_ai"],
           f"a register() call inside a --[[ ]] block comment is not matched, got {names}")


def test_extract_lua_registered_modules_survives_dash_in_string():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_AFTER_DASH_STRING})
    names = [n for n, _ in found]
    expect(names == ["string_dash_module"],
           f"a `--` embedded in an earlier string literal on the same line "
           f"does not swallow a real register() call after it, got {names}")


def test_parse_classified_names_scoped_by_section():
    by_section = parse_classified_names(SYNTHETIC_INVENTORY_COMPLETE)
    expect(by_section.get("1") == {"fieldOne", "fieldTwo", "fieldThree"},
           f"section 1 gets exactly its own backtick-quoted first-column "
           f"names, got {by_section.get('1')}")
    expect(by_section.get("7") == {"unit_ai"},
           f"section 7 gets exactly its own names, got {by_section.get('7')}")


def test_parse_classified_names_ignores_other_columns():
    # A name that only appears in a NON-first column (e.g. a cross-
    # reference in "Restoration dependency") must not count as classified.
    text = "## 1. fake\n\n| `realField` | depends on `otherField` |\n"
    by_section = parse_classified_names(text)
    expect(by_section.get("1") == {"realField"},
           f"only the first column counts as a classification, got {by_section.get('1')}")


def test_parse_classified_names_does_not_merge_across_sections():
    text = ("## 1. fake\n\n| `shared` | x |\n\n"
            "## 7. fake\n\n| `shared` | y |\n| `only_in_seven` | z |\n")
    by_section = parse_classified_names(text)
    expect(by_section.get("1") == {"shared"},
           f"section 1 keeps only its own copy of a shared name, got {by_section.get('1')}")
    expect(by_section.get("7") == {"shared", "only_in_seven"},
           f"section 7 keeps its own names independently, got {by_section.get('7')}")


FAKE_ROOT_RECORDS = [("EngineEnv", "Fake.hs", r"^data EngineEnv = EngineEnv\b", "1")]


def test_audit_clean_repo_state_has_no_violations():
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not violations,
           f"a fully classified fixture reports no violations at all, got {violations}")


def test_audit_detects_field_hidden_behind_unbalanced_comment_brace():
    """Regression for the false-negative the naive brace counter had: a
    lone unbalanced `}` in a haddock comment used to close the record
    block early, so fieldThree was never extracted and its absence from
    the inventory went unreported. It must be reported now."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_UNBALANCED_COMMENT},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo, not fieldThree
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("fieldTwo" in v for v in violations),
           f"fieldTwo (dropped from the fixture inventory) is still reported "
           f"even with an unbalanced brace earlier in the same record, got {violations}")


def test_audit_detects_field_hidden_behind_nested_comment():
    """Regression for the nesting-unaware stripper: a legally nested
    {- -} comment used to leave a stray `}` behind that closed the
    record early, hiding fieldTwo's absence from the inventory."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_NESTED_COMMENT},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("fieldTwo" in v for v in violations),
           f"fieldTwo is still reported even with a nested block comment "
           f"earlier in the same record, got {violations}")


def test_audit_detects_module_registered_across_multiple_lines():
    """Regression for the Lua false-negative: a register() call split
    across lines used to never match, so an unclassified module
    registered that way went unreported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_MULTILINE},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for multiline_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("multiline_module" in v for v in violations),
           f"a module registered via a multi-line call is reported when "
           f"unclassified, got {violations}")


def test_audit_detects_module_registered_after_dash_string():
    """Regression for the Lua string-awareness gap: a `--` inside an
    earlier string literal used to truncate the line and hide a real
    register() call that followed it on the same line."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_AFTER_DASH_STRING},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for string_dash_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("string_dash_module" in v for v in violations),
           f"a module registered after a same-line string containing '--' "
           f"is reported when unclassified, got {violations}")


def test_audit_does_not_let_a_same_named_entry_in_another_section_count():
    """Regression for the owner-scoping gap: a Lua-module row (section 7)
    happening to be named `fieldTwo` must not satisfy the classification
    requirement for the UNRELATED EngineEnv.fieldTwo (section 1) --
    they are different owners and need independent decisions."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_CROSS_SECTION_COLLISION,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("EngineEnv.fieldTwo" in v for v in violations),
           f"EngineEnv.fieldTwo is still reported unclassified even though "
           f"a same-named row exists in the Lua section, got {violations}")


def test_audit_detects_intentionally_unclassified_field():
    """The req-10 acceptance test: introduce an unclassified root-owner
    field (by using the inventory with one entry deleted) and confirm the
    audit reports it by name."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("fieldTwo" in v for v in violations),
           f"an unclassified field (fieldTwo, deliberately dropped from the "
           f"fixture inventory) is reported, got {violations}")
    expect(not any("fieldOne" in v or "fieldThree" in v for v in violations),
           f"fields that ARE classified are not falsely reported, got {violations}")


def test_audit_detects_intentionally_unclassified_lua_module():
    """The req-10 acceptance test, Lua half: a registered save module
    absent from the inventory is reported by name."""
    lua_source = SYNTHETIC_LUA_REGISTER + '\nsaveMods.register("new_untracked_module", nil, nil)\n'
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": lua_source},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("new_untracked_module" in v for v in violations),
           f"a newly registered, unclassified Lua module is reported, got {violations}")
    expect(not any('"unit_ai"' in v for v in violations),
           f"the already-classified unit_ai module is not falsely reported, got {violations}")


def test_audit_against_the_real_repo():
    """End-to-end smoke test against the actual checked-out inventory and
    source files -- this is what CI/make ci actually runs via main()."""
    from persistence_inventory_audit import _load_repo_state  # type: ignore
    record_sources, scripts_text_by_file, inventory_text = _load_repo_state()
    violations = audit(record_sources, scripts_text_by_file, inventory_text)
    expect(not violations,
           f"the real repo's inventory has no unclassified root-owner fields "
           f"or Lua save modules, got {violations}")


# ----- Runner --------------------------------------------------------------

def main() -> int:
    tests = [
        test_extract_fields_from_brace_block,
        test_extract_fields_stray_brace_in_comment_is_harmless,
        test_extract_fields_unbalanced_brace_in_comment_does_not_truncate,
        test_extract_fields_nested_block_comment_does_not_truncate,
        test_extract_fields_ignores_other_records,
        test_extract_fields_missing_record_raises,
        test_extract_lua_registered_modules,
        test_extract_lua_registered_modules_multiline_call,
        test_extract_lua_registered_modules_spaced_dot_call,
        test_extract_lua_registered_modules_block_commented_out,
        test_extract_lua_registered_modules_survives_dash_in_string,
        test_parse_classified_names_scoped_by_section,
        test_parse_classified_names_ignores_other_columns,
        test_parse_classified_names_does_not_merge_across_sections,
        test_audit_clean_repo_state_has_no_violations,
        test_audit_detects_field_hidden_behind_unbalanced_comment_brace,
        test_audit_detects_field_hidden_behind_nested_comment,
        test_audit_detects_module_registered_across_multiple_lines,
        test_audit_detects_module_registered_after_dash_string,
        test_audit_does_not_let_a_same_named_entry_in_another_section_count,
        test_audit_detects_intentionally_unclassified_field,
        test_audit_detects_intentionally_unclassified_lua_module,
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
