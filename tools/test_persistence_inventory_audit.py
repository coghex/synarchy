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
    find_lua_register_aliases, parse_classified_names, audit,
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

# Owner-scoped inventory: the `### EngineEnv` heading classifies the
# EngineEnv fixture fields, `### Lua persistence registry` classifies
# the Lua registry fixture -- matching the real inventory doc's scheme
# (docs/persistence_state_inventory.md), NOT the coarser `## N.`
# section number (several distinct owners can share one numbered
# section there, e.g. WorldManager/WorldState both under "## 3.").
SYNTHETIC_INVENTORY_COMPLETE = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | Exclude |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""

SYNTHETIC_INVENTORY_MISSING_ONE = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""

# fieldTwo HAS a row and is present by name, but its classification cell
# is a bare em-dash placeholder -- not one of the five taxonomy values.
# This is the "accepts no classification as a classification" gap:
# name-presence alone must not be enough.
SYNTHETIC_INVENTORY_INVALID_CLASSIFICATION = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | — |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""

# fieldTwo's classification uses a valid taxonomy label wrapped in bold
# markup with a parenthetical suffix -- must still count as valid.
SYNTHETIC_INVENTORY_DECORATED_VALID_CLASSIFICATION = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | **Exclude (new-format target differs)** |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""

# fieldTwo is only classified under the Lua registry heading (as if it
# were a Lua module name), NOT under `### EngineEnv` where the real
# EngineEnv.fieldTwo lives -- this must NOT satisfy EngineEnv.fieldTwo's
# requirement, even though both owners share `## 1.`/`## 7.`'s sibling
# status under the same document.
SYNTHETIC_INVENTORY_OWNER_COLLISION = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `fieldTwo` | Persist exactly (opaque blob) |
| `unit_ai` | Persist exactly (opaque blob) |
"""

# Two DIFFERENT Haskell records sharing a field name, mirroring the
# real inventory's WorldManager/WorldState pair (both live under the
# same "## 3." numbered section but must still be scoped as separate
# owners). Only OwnerB's `shared` is classified.
SYNTHETIC_TWO_OWNERS_SHARED_FIELD_NAME = """\
module Fake where

data OwnerA = OwnerA
  { shared ∷ Int
  }

data OwnerB = OwnerB
  { shared ∷ Int
  }
"""

TWO_OWNER_ROOT_RECORDS = [
    ("OwnerA", "Fake.hs", r"^data OwnerA = OwnerA\b"),
    ("OwnerB", "Fake.hs", r"^data OwnerB = OwnerB\b"),
]

SYNTHETIC_INVENTORY_ONLY_OWNER_B_CLASSIFIED = """\
# Fake inventory

## 3. OwnerA / OwnerB (share one numbered section, like WorldManager/WorldState)

### OwnerB

| Field | Classification |
|---|---|
| `shared` | Persist exactly |
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

# fieldTwo's name and its `∷`/type are on DIFFERENT physical lines --
# legal Haskell layout. A field-name matcher anchored to "same line as
# the arrow" never sees it.
SYNTHETIC_ENGINE_ENV_MULTILINE_FIELD = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne   ∷ IORef Int
  , fieldTwo
      ∷ IORef Text
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

# A register() call using single-quoted Lua strings (legal Lua, an
# alternative to double quotes).
SYNTHETIC_LUA_REGISTER_SINGLE_QUOTED = """\
local saveMods = require('scripts.lib.save_modules')

saveMods.register('single_quoted_module', nil, nil)
"""

# A register() call using Lua LONG-BRACKET strings (`[[...]]`, or
# `[=[...]=]` etc. for a name that itself contains `]]`) -- a third,
# less common but fully legal Lua string-literal form.
SYNTHETIC_LUA_REGISTER_LONGBRACKET = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register([[longbracket_module]], nil, nil)
"""

SYNTHETIC_LUA_REGISTER_LONGBRACKET_LEVELED = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register([==[leveled_longbracket_module]==], nil, nil)
"""

# A GROUPED field declaration -- several names sharing one trailing
# type signature (`name1, name2 :: Type`), legal Haskell. `unclassified`
# has no arrow of its own; it borrows `classified`'s.
SYNTHETIC_ENGINE_ENV_GROUPED_FIELD = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , unclassified, fieldTwo ∷ IORef Text
  , fieldThree ∷ Q.Queue Int
  } deriving (Eq)
"""

# A long-bracket Lua STRING (not a comment) whose CONTENT contains `--`.
# A comment stripper that isn't long-bracket-aware treats that embedded
# `--` as a real comment start and discards the real register() call
# that follows it on the same line.
SYNTHETIC_LUA_REGISTER_AFTER_LONGBRACKET_DASH_STRING = """\
local saveMods = require("scripts.lib.save_modules")

local dash = [[--]]; saveMods.register([[longbracket_dash_module]], nil, nil)
"""

# saveMods.register is stored in a local and called THROUGH the alias --
# a live, unclassified registration the direct-call matcher can't trace.
SYNTHETIC_LUA_REGISTER_ALIASED = """\
local saveMods = require("scripts.lib.save_modules")

local register = saveMods.register
register("aliased_module", nil, nil)
"""

# Mirrors the REAL save_modules.lua registry's own validation message --
# the literal text "saveModules.register" appears inside a string
# literal here, not as a reference to the function. Must NOT be flagged
# as an alias.
SYNTHETIC_LUA_REGISTER_DEFINITION_WITH_ERROR_STRING = """\
local saveModules = package.loaded["scripts.lib.save_modules"] or {}

function saveModules.register(name, serializeFn, deserializeFn)
    if type(name) ~= "string" then
        error("saveModules.register: name must be a string")
    end
    saveModules.registry[name] = { serialize = serializeFn, deserialize = deserializeFn }
end
"""

# A direct call reached via BRACKET indexing instead of dot access --
# ordinary, fully traceable Lua, not an alias.
SYNTHETIC_LUA_REGISTER_BRACKET_CALL = """\
local saveMods = require("scripts.lib.save_modules")

saveMods["register"]("bracket_module", nil, nil)
"""

# saveMods["register"] is stored in a local and called THROUGH the
# alias -- the bracket-indexed sibling of SYNTHETIC_LUA_REGISTER_ALIASED.
SYNTHETIC_LUA_REGISTER_BRACKET_ALIASED = """\
local saveMods = require("scripts.lib.save_modules")

local register = saveMods["register"]
register("aliased_bracket_module", nil, nil)
"""

# A record field typed with a DataKinds/GHC.TypeLits promoted string
# literal containing a `}` -- legal Haskell. A brace-counter that isn't
# string-aware treats this as the record's OWN closing brace, hiding
# `unclassified` (and every field after it) from extraction entirely.
SYNTHETIC_ENGINE_ENV_STRING_LITERAL_BRACE = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , classified ∷ Proxy "}"
  , unclassified ∷ Int
  } deriving (Eq)
"""

# Same hazard, but with `--` inside the promoted string literal instead
# of `}` -- must not be mistaken for a line-comment start either.
SYNTHETIC_ENGINE_ENV_STRING_LITERAL_DASH = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , classified ∷ Proxy "--"
  , unclassified ∷ Int
  } deriving (Eq)
"""

# A promoted Char literal (DataKinds `'}'`) containing a `}` in a
# field's own type -- the char-literal sibling of the string-literal
# brace hazard above.
SYNTHETIC_ENGINE_ENV_CHAR_LITERAL_BRACE = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , classified ∷ Proxy '}'
  , unclassified ∷ Int
  } deriving (Eq)
"""

# Ordinary Haskell identifiers ending in one or more trailing "primes"
# (`foo'`, `bar''`) -- must NOT be mistaken for char-literal openers.
SYNTHETIC_ENGINE_ENV_TRAILING_PRIMES = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne' ∷ IORef Int
  , fieldTwo'' ∷ Int
  } deriving (Eq)
"""

# A direct call reached off require(...)'s return value with no local
# binding at all -- fully traceable (the module path is a literal
# string), not an alias.
SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_CALL = """\
require("scripts.lib.save_modules").register("require_chained_module", nil, nil)
"""

# require(...).register is stored in a local and called THROUGH the
# alias -- the require-chained sibling of SYNTHETIC_LUA_REGISTER_ALIASED.
SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_ALIASED = """\
local register = require("scripts.lib.save_modules").register
register("aliased_require_module", nil, nil)
"""

# A long-bracket STRING (not a comment) whose content happens to
# mention "saveMods.register" -- prose, not a live reference. Mirrors
# SYNTHETIC_LUA_REGISTER_DEFINITION_WITH_ERROR_STRING but for the
# long-bracket string form instead of a quoted one.
SYNTHETIC_LUA_LONGBRACKET_STRING_MENTIONING_REGISTER = """\
local saveMods = require("scripts.lib.save_modules")

local doc = [[saveMods.register]]
saveMods.register("real_module", nil, nil)
"""

# Two names sharing one classification, joined with "+" -- looks
# plausible but is not a single taxonomy label.
SYNTHETIC_INVENTORY_COMPOUND_CLASSIFICATION = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | Rebuild + Persist (mixed) |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
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


def test_extract_fields_name_and_arrow_on_different_lines():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_MULTILINE_FIELD,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "fieldTwo", "fieldThree"],
           f"a field whose name and `∷`/type are on DIFFERENT physical "
           f"lines is still extracted, got {fields}")


def test_extract_fields_grouped_declaration():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_GROUPED_FIELD,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "unclassified", "fieldTwo", "fieldThree"],
           f"a grouped declaration (`unclassified, fieldTwo ∷ IORef Text`) "
           f"extracts BOTH names sharing the trailing type, got {fields}")


def test_extract_fields_survives_brace_in_string_literal_type():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_STRING_LITERAL_BRACE,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "classified", "unclassified"],
           f"a DataKinds promoted string literal type containing '}}' "
           f"(`Proxy \"}}\"`) does not prematurely close the record and "
           f"drop later fields, got {fields}")


def test_extract_fields_survives_dash_in_string_literal_type():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_STRING_LITERAL_DASH,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "classified", "unclassified"],
           f"a DataKinds promoted string literal type containing '--' "
           f"(`Proxy \"--\"`) is not mistaken for a line comment, "
           f"got {fields}")


def test_extract_fields_survives_brace_in_char_literal_type():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_CHAR_LITERAL_BRACE,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "classified", "unclassified"],
           f"a DataKinds promoted CHAR literal type containing '}}' "
           f"(`Proxy '}}'`) does not prematurely close the record and "
           f"drop later fields, got {fields}")


def test_extract_fields_trailing_primes_are_not_char_literals():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_TRAILING_PRIMES,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne'", "fieldTwo''"],
           f"ordinary identifiers ending in trailing primes are not "
           f"mistaken for char-literal openers, got {fields}")


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


def test_extract_lua_registered_modules_single_quoted():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_SINGLE_QUOTED})
    names = [n for n, _ in found]
    expect(names == ["single_quoted_module"],
           f"finds a register() call using single-quoted Lua strings, got {names}")


def test_extract_lua_registered_modules_longbracket():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET})
    names = [n for n, _ in found]
    expect(names == ["longbracket_module"],
           f"finds a register() call using [[ ]] long-bracket Lua strings, got {names}")


def test_extract_lua_registered_modules_longbracket_leveled():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_LEVELED})
    names = [n for n, _ in found]
    expect(names == ["leveled_longbracket_module"],
           f"finds a register() call using a leveled [==[ ]==] long-bracket "
           f"string, got {names}")


def test_extract_lua_registered_modules_survives_dash_in_longbracket_string():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_AFTER_LONGBRACKET_DASH_STRING})
    names = [n for n, _ in found]
    expect(names == ["longbracket_dash_module"],
           f"a `--` embedded in an earlier LONG-BRACKET string literal on "
           f"the same line does not swallow a real register() call after "
           f"it, got {names}")


def test_extract_lua_registered_modules_does_not_see_through_alias():
    # extract_lua_registered_modules only recognizes DIRECT calls -- an
    # aliased call is invisible to it BY DESIGN; find_lua_register_aliases
    # (tested below) is what catches this case instead.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_ALIASED})
    names = [n for n, _ in found]
    expect(names == [],
           f"a call routed through an alias is not seen as a direct "
           f"registration (that's find_lua_register_aliases's job), got {names}")


def test_find_lua_register_aliases_detects_stored_reference():
    offenders = find_lua_register_aliases({"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_ALIASED})
    expect(offenders == ["scripts/fake.lua"],
           f"a saveMods.register reference stored in a local (not called "
           f"directly) is flagged, got {offenders}")


def test_find_lua_register_aliases_ignores_direct_calls():
    offenders = find_lua_register_aliases({"scripts/fake.lua": SYNTHETIC_LUA_REGISTER})
    expect(offenders == [],
           f"ordinary direct register() calls are not flagged as aliases, "
           f"got {offenders}")


def test_find_lua_register_aliases_ignores_the_definition_and_its_error_string():
    # The real save_modules.lua's OWN function definition
    # (`function saveModules.register(...)`) is a direct-call-shaped
    # signature, not an alias -- and its validation error string
    # literally contains the text "saveModules.register", which must
    # not be mistaken for a reference to the function either.
    offenders = find_lua_register_aliases(
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_DEFINITION_WITH_ERROR_STRING})
    expect(offenders == [],
           f"the registry's own definition + error message are not "
           f"flagged as an alias, got {offenders}")


def test_extract_lua_registered_modules_bracket_form_call():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_CALL})
    names = [n for n, _ in found]
    expect(names == ["bracket_module"],
           f"a saveMods[\"register\"](...) direct call (bracket indexing "
           f"instead of dot access) is extracted, got {names}")


def test_find_lua_register_aliases_ignores_bracket_form_direct_call():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_CALL})
    expect(offenders == [],
           f"a bracket-form DIRECT call is not flagged as an alias, "
           f"got {offenders}")


def test_find_lua_register_aliases_detects_bracket_form_stored_reference():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_ALIASED})
    expect(offenders == ["scripts/fake.lua"],
           f"a saveMods[\"register\"] reference stored in a local (not "
           f"called directly) is flagged, got {offenders}")


def test_extract_lua_registered_modules_require_chained_call():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_CALL})
    names = [n for n, _ in found]
    expect(names == ["require_chained_module"],
           f"a require(...).register(...) direct call with no local "
           f"binding is extracted, got {names}")


def test_find_lua_register_aliases_ignores_require_chained_direct_call():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_CALL})
    expect(offenders == [],
           f"a require(...).register(...) DIRECT call is not flagged as "
           f"an alias, got {offenders}")


def test_find_lua_register_aliases_detects_require_chained_stored_reference():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_ALIASED})
    expect(offenders == ["scripts/fake.lua"],
           f"require(...).register stored in a local (not called "
           f"directly) is flagged, got {offenders}")


def test_find_lua_register_aliases_ignores_longbracket_string_prose():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_LONGBRACKET_STRING_MENTIONING_REGISTER})
    expect(offenders == [],
           f"a long-bracket STRING literal mentioning \"saveMods.register\" "
           f"in its text is not mistaken for a live reference, "
           f"got {offenders}")


def test_parse_classified_names_scoped_by_owner_heading():
    by_owner = parse_classified_names(SYNTHETIC_INVENTORY_COMPLETE)
    expect(set(by_owner.get("EngineEnv", {})) == {"fieldOne", "fieldTwo", "fieldThree"},
           f"the '### EngineEnv' heading gets exactly its own backtick-quoted "
           f"first-column names, got {by_owner.get('EngineEnv')}")
    expect(by_owner.get("EngineEnv", {}).get("fieldOne") == "Persist exactly",
           f"captures each name's own classification cell text, got "
           f"{by_owner.get('EngineEnv')}")
    expect(set(by_owner.get("Lua persistence registry", {})) == {"unit_ai"},
           f"the Lua registry heading gets exactly its own names, got "
           f"{by_owner.get('Lua persistence registry')}")


def test_parse_classified_names_ignores_other_columns():
    # A name that only appears in a NON-first column (e.g. a cross-
    # reference in "Restoration dependency") must not count as classified,
    # and it's the CLASSIFICATION column's own text that's captured, not
    # some other column's.
    text = ("### Fake\n\n"
            "| Field | Classification | Restoration dependency |\n"
            "|---|---|---|\n"
            "| `realField` | Exclude | depends on `otherField` |\n")
    by_owner = parse_classified_names(text)
    expect(set(by_owner.get("Fake", {})) == {"realField"},
           f"only the first column's names count as classified, got {by_owner.get('Fake')}")
    expect(by_owner.get("Fake", {}).get("realField") == "Exclude",
           f"the Classification column's text is captured, not a later "
           f"column's, got {by_owner.get('Fake')}")


def test_parse_classified_names_does_not_merge_across_owners():
    text = ("### OwnerX\n\n"
            "| Field | Classification |\n|---|---|\n"
            "| `shared` | Persist exactly |\n\n"
            "### OwnerY\n\n"
            "| Field | Classification |\n|---|---|\n"
            "| `shared` | Exclude |\n"
            "| `only_in_y` | Rebuild |\n")
    by_owner = parse_classified_names(text)
    expect(by_owner.get("OwnerX") == {"shared": "Persist exactly"},
           f"OwnerX keeps only its own copy of a shared name (with its own "
           f"classification text), got {by_owner.get('OwnerX')}")
    expect(by_owner.get("OwnerY") == {"shared": "Exclude", "only_in_y": "Rebuild"},
           f"OwnerY keeps its own names and classifications independently, "
           f"got {by_owner.get('OwnerY')}")


def test_parse_classified_names_finds_classification_column_at_any_index():
    # The Lua registry table's real header puts Classification 4th
    # (Module | Owner | Scope | Classification | ...), not 2nd -- the
    # parser must locate it by name, not assume a fixed position.
    text = ("### Fake\n\n"
            "| Module | Owner | Scope | Classification | Test oracle |\n"
            "|---|---|---|---|---|\n"
            "| `mod_a` | some/file.lua | global | Persist exactly | none yet |\n")
    by_owner = parse_classified_names(text)
    expect(by_owner.get("Fake") == {"mod_a": "Persist exactly"},
           f"finds Classification wherever it sits in the header, got {by_owner.get('Fake')}")


FAKE_ROOT_RECORDS = [("EngineEnv", "Fake.hs", r"^data EngineEnv = EngineEnv\b")]


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


def test_audit_detects_field_with_name_and_arrow_on_different_lines():
    """Regression for the multiline-field false-negative: a field
    whose name and `∷`/type are on different physical lines used to
    never be extracted, so its absence from the inventory went
    unreported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_MULTILINE_FIELD},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("fieldTwo" in v for v in violations),
           f"fieldTwo (whose name and arrow are on different lines in this "
           f"fixture) is still reported when unclassified, got {violations}")


def test_audit_detects_grouped_field_declaration():
    """Regression for the grouped-declaration false-negative: only the
    LAST name in `name1, name2 :: Type` used to be extracted, so an
    unclassified name earlier in the group went unreported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_GROUPED_FIELD},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo; unclassified is never classified either
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("unclassified" in v for v in violations),
           f"the bare-named first field in a grouped declaration is "
           f"reported when unclassified, got {violations}")
    expect(any("EngineEnv.fieldTwo" in v for v in violations),
           f"the arrow-bearing second field in the group is also reported "
           f"when unclassified, got {violations}")


def test_audit_detects_field_hidden_behind_brace_in_string_literal():
    """Regression: a DataKinds promoted string literal containing '}'
    in a field's own type used to prematurely close the record,
    hiding every field after it from the audit entirely."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_STRING_LITERAL_BRACE},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # classifies fieldOne/fieldThree; classified/unclassified aren't real names here
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("unclassified" in v for v in violations),
           f"the field after the brace-containing string literal type is "
           f"still extracted and reported when unclassified, got {violations}")


def test_audit_detects_field_hidden_behind_brace_in_char_literal():
    """Regression: a DataKinds promoted CHAR literal containing '}'
    used to prematurely close the record the same way a string literal
    did (test above) -- the char-literal-specific code path."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_CHAR_LITERAL_BRACE},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("unclassified" in v for v in violations),
           f"the field after the brace-containing char literal type is "
           f"still extracted and reported when unclassified, got {violations}")


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


def test_audit_detects_single_quoted_module_registration():
    """Regression for the single-quote gap: a register() call using
    Lua's single-quote string syntax used to never match."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_SINGLE_QUOTED},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for single_quoted_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("single_quoted_module" in v for v in violations),
           f"a module registered with single-quoted Lua strings is reported "
           f"when unclassified, got {violations}")


def test_audit_detects_longbracket_module_registration():
    """Regression for the long-bracket gap: a register() call using
    Lua's [[ ]] / [=[ ]=] long-bracket string syntax used to never
    match, so a live, unclassified registration went unreported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": (SYNTHETIC_LUA_REGISTER_LONGBRACKET
                               + SYNTHETIC_LUA_REGISTER_LONGBRACKET_LEVELED)},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for either module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("longbracket_module" in v and "leveled" not in v for v in violations),
           f"a module registered with [[ ]] long brackets is reported when "
           f"unclassified, got {violations}")
    expect(any("leveled_longbracket_module" in v for v in violations),
           f"a module registered with a leveled [==[ ]==] long bracket is "
           f"reported when unclassified, got {violations}")


def test_audit_detects_module_registered_after_longbracket_dash_string():
    """Regression for the long-bracket string-awareness gap: a `--`
    inside an earlier LONG-BRACKET string literal used to truncate the
    line and hide a real register() call that followed it on the same
    line."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_AFTER_LONGBRACKET_DASH_STRING},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for longbracket_dash_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("longbracket_dash_module" in v for v in violations),
           f"a module registered after a same-line long-bracket string "
           f"containing '--' is reported when unclassified, got {violations}")


def test_audit_does_not_let_a_same_named_entry_under_another_owner_count():
    """Regression for the owner-scoping gap: a Lua-module row happening
    to be named `fieldTwo` must not satisfy the classification
    requirement for the UNRELATED EngineEnv.fieldTwo -- they are
    different owners and need independent decisions, even though both
    live in the same document."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_OWNER_COLLISION,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("EngineEnv.fieldTwo" in v for v in violations),
           f"EngineEnv.fieldTwo is still reported unclassified even though "
           f"a same-named row exists under the Lua registry heading, got {violations}")


def test_audit_does_not_let_a_sibling_record_in_the_same_numbered_section_count():
    """Regression for the reviewer's exact WorldManager/WorldState
    scenario: two DIFFERENT records sharing a `## N.` numbered section
    (but each with its own `### OwnerName` heading) must not let one's
    classified field satisfy the other's requirement for a same-named
    field."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_TWO_OWNERS_SHARED_FIELD_NAME},
        {},
        SYNTHETIC_INVENTORY_ONLY_OWNER_B_CLASSIFIED,
        root_records=TWO_OWNER_ROOT_RECORDS,
    )
    expect(any("OwnerA.shared" in v for v in violations),
           f"OwnerA.shared is reported unclassified even though OwnerB.shared "
           f"(a sibling record under the same numbered section, same field "
           f"name) IS classified, got {violations}")
    expect(not any("OwnerB.shared" in v for v in violations),
           f"OwnerB.shared, which IS classified under its own heading, is "
           f"not falsely reported, got {violations}")


def test_audit_rejects_a_blank_placeholder_as_a_classification():
    """Regression for the "no classification counts as a classification"
    gap: a row whose NAME is present but whose classification cell is a
    bare '—' placeholder (none of the five taxonomy values) must still
    be reported -- name-presence alone is not enough."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_INVALID_CLASSIFICATION,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("EngineEnv.fieldTwo" in v and "not one of" in v for v in violations),
           f"fieldTwo's blank '—' classification is reported as invalid, "
           f"got {violations}")
    expect(not any("EngineEnv.fieldOne" in v or "EngineEnv.fieldThree" in v
                    for v in violations),
           f"fields with a real taxonomy classification are not falsely "
           f"reported, got {violations}")


def test_audit_accepts_a_decorated_valid_classification():
    """A valid taxonomy label wrapped in bold markup with a trailing
    parenthetical aside (e.g. the real inventory's '**Exclude
    (new-format target differs)**' rows) must still count as valid --
    the check strips that decoration down to a CORE value and requires
    the core to equal one of the five canonical labels exactly."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_DECORATED_VALID_CLASSIFICATION,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("EngineEnv.fieldTwo" in v for v in violations),
           f"a bold-wrapped, parenthetical-suffixed but still-valid "
           f"classification is accepted, got {violations}")


def test_audit_rejects_a_compound_classification():
    """Regression for the "more than one label at once" gap: a value
    like 'Rebuild + Persist (mixed)' looks plausible but its CORE
    (after stripping the trailing parenthetical) is 'Rebuild + Persist'
    -- not a single canonical label -- so it must be rejected. A plain
    substring test would have missed this, since "Persist exactly"
    isn't literally present in the text."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_COMPOUND_CLASSIFICATION,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("EngineEnv.fieldTwo" in v and "not one of" in v for v in violations),
           f"a compound 'Rebuild + Persist (mixed)' classification is "
           f"rejected as not a single taxonomy label, got {violations}")


def test_audit_detects_aliased_lua_registration():
    """Regression for the alias-bypass gap: a module registered by
    calling saveMods.register through a stored alias, rather than
    directly, must still be reported -- the audit can't trace the
    alias, so it fails on the aliasing pattern itself."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_ALIASED},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"a saveMods.register reference stored in a local is reported "
           f"as an aliasing violation, got {violations}")


def test_audit_detects_unclassified_bracket_form_module_registration():
    """Regression for the bracket-indexing bypass: saveMods["register"]
    is an ordinary direct call, not an alias -- an unclassified module
    registered that way must still be reported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for bracket_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("bracket_module" in v for v in violations),
           f"a module registered via bracket indexing is reported when "
           f"unclassified, got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a bracket-form DIRECT call is not ALSO reported as an "
           f"aliasing violation, got {violations}")


def test_audit_detects_aliased_bracket_form_registration():
    """The alias-bypass gap's bracket-indexed sibling."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_ALIASED},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"a saveMods[\"register\"] reference stored in a local is "
           f"reported as an aliasing violation, got {violations}")


def test_audit_detects_unclassified_require_chained_module_registration():
    """Regression for the require()-chained bypass: a module registered
    via require(...).register(...) with no local binding at all is
    ordinary, fully traceable Lua -- an unclassified module registered
    that way must still be reported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for require_chained_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("require_chained_module" in v for v in violations),
           f"a module registered via require(...).register(...) is "
           f"reported when unclassified, got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a require(...).register(...) DIRECT call is not ALSO "
           f"reported as an aliasing violation, got {violations}")


def test_audit_detects_aliased_require_chained_registration():
    """The alias-bypass gap's require()-chained sibling."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_ALIASED},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"require(...).register stored in a local is reported as an "
           f"aliasing violation, got {violations}")


def test_audit_does_not_flag_longbracket_string_prose_as_an_alias():
    """Regression: a long-bracket STRING literal (not a comment) whose
    content happens to mention "saveMods.register" used to be
    misidentified as a live reference, falsely failing the audit even
    though the real registration on the next line is fine."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_LONGBRACKET_STRING_MENTIONING_REGISTER},
        SYNTHETIC_INVENTORY_COMPLETE,  # classifies unit_ai, the real registration here
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("alias" in v for v in violations),
           f"a long-bracket string literal's prose text is not reported "
           f"as an aliasing violation, got {violations}")


def test_audit_does_not_flag_the_registry_definition_as_an_alias():
    """The real save_modules.lua's own function definition and its
    validation error string (which contains the literal text
    "saveModules.register") must not trip the alias check."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_DEFINITION_WITH_ERROR_STRING},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("alias" in v for v in violations),
           f"the registry's own definition and error message are not "
           f"reported as an aliasing violation, got {violations}")


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
        test_extract_fields_name_and_arrow_on_different_lines,
        test_extract_fields_grouped_declaration,
        test_extract_fields_survives_brace_in_string_literal_type,
        test_extract_fields_survives_dash_in_string_literal_type,
        test_extract_fields_survives_brace_in_char_literal_type,
        test_extract_fields_trailing_primes_are_not_char_literals,
        test_extract_fields_ignores_other_records,
        test_extract_fields_missing_record_raises,
        test_extract_lua_registered_modules,
        test_extract_lua_registered_modules_multiline_call,
        test_extract_lua_registered_modules_spaced_dot_call,
        test_extract_lua_registered_modules_block_commented_out,
        test_extract_lua_registered_modules_survives_dash_in_string,
        test_extract_lua_registered_modules_single_quoted,
        test_extract_lua_registered_modules_longbracket,
        test_extract_lua_registered_modules_longbracket_leveled,
        test_extract_lua_registered_modules_survives_dash_in_longbracket_string,
        test_extract_lua_registered_modules_does_not_see_through_alias,
        test_extract_lua_registered_modules_bracket_form_call,
        test_extract_lua_registered_modules_require_chained_call,
        test_find_lua_register_aliases_detects_stored_reference,
        test_find_lua_register_aliases_ignores_direct_calls,
        test_find_lua_register_aliases_ignores_the_definition_and_its_error_string,
        test_find_lua_register_aliases_ignores_bracket_form_direct_call,
        test_find_lua_register_aliases_detects_bracket_form_stored_reference,
        test_find_lua_register_aliases_ignores_require_chained_direct_call,
        test_find_lua_register_aliases_detects_require_chained_stored_reference,
        test_find_lua_register_aliases_ignores_longbracket_string_prose,
        test_parse_classified_names_scoped_by_owner_heading,
        test_parse_classified_names_ignores_other_columns,
        test_parse_classified_names_does_not_merge_across_owners,
        test_parse_classified_names_finds_classification_column_at_any_index,
        test_audit_clean_repo_state_has_no_violations,
        test_audit_detects_field_hidden_behind_unbalanced_comment_brace,
        test_audit_detects_field_hidden_behind_nested_comment,
        test_audit_detects_field_with_name_and_arrow_on_different_lines,
        test_audit_detects_grouped_field_declaration,
        test_audit_detects_field_hidden_behind_brace_in_string_literal,
        test_audit_detects_field_hidden_behind_brace_in_char_literal,
        test_audit_detects_module_registered_across_multiple_lines,
        test_audit_detects_module_registered_after_dash_string,
        test_audit_detects_single_quoted_module_registration,
        test_audit_detects_longbracket_module_registration,
        test_audit_detects_module_registered_after_longbracket_dash_string,
        test_audit_does_not_let_a_same_named_entry_under_another_owner_count,
        test_audit_does_not_let_a_sibling_record_in_the_same_numbered_section_count,
        test_audit_rejects_a_blank_placeholder_as_a_classification,
        test_audit_accepts_a_decorated_valid_classification,
        test_audit_rejects_a_compound_classification,
        test_audit_detects_aliased_lua_registration,
        test_audit_detects_unclassified_bracket_form_module_registration,
        test_audit_detects_aliased_bracket_form_registration,
        test_audit_detects_unclassified_require_chained_module_registration,
        test_audit_detects_aliased_require_chained_registration,
        test_audit_does_not_flag_longbracket_string_prose_as_an_alias,
        test_audit_does_not_flag_the_registry_definition_as_an_alias,
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
