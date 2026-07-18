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
    find_lua_register_aliases, find_lua_register_dynamic_names,
    find_untracked_registry_aliases,
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
package.loaded["scripts.lib.save_modules"] = saveModules

saveModules.registry = saveModules.registry or {}

function saveModules.register(name, serializeFn, deserializeFn)
    if type(name) ~= "string" then
        error("saveModules.register: name must be a string")
    end
    saveModules.registry[name] = { serialize = serializeFn, deserialize = deserializeFn }
end
"""

# The registry table escapes to a GLOBAL (no `local` keyword) variable
# -- Lua's `=` is always assignment (never comparison), so this is just
# as live a bypass as the `local` form.
SYNTHETIC_LUA_REGISTER_GLOBAL_REALIAS = """\
local saveMods = require("scripts.lib.save_modules")
registry = saveMods
registry.register("untracked_via_global_alias", nil, nil)
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

# Two DIFFERENT fields whose promoted string-literal TYPES happen to
# spell out `{-`/`-}` -- a block-comment stripper that isn't
# literal-aware sees the first as "opening" a comment and the second
# (in a LATER field) as "closing" it, silently swallowing everything
# (including real field declarations) in between, including
# `unclassified` itself.
SYNTHETIC_ENGINE_ENV_FAKE_BLOCK_COMMENT_STRINGS = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , documented ∷ Proxy "{-"
  , unclassified ∷ Proxy "-}"
  } deriving (Eq)
"""

# A long-bracket STRING (not a comment) whose CONTENT is shaped exactly
# like a real registration call. A scanner that doesn't exclude string
# spans from the DIRECT-CALL matcher (not just the alias matcher) reads
# this as a live, unclassified registration and fails CI even though
# nothing here actually executes.
SYNTHETIC_LUA_REGISTER_PROSE_LOOKS_LIKE_CALL = """\
local saveMods = require("scripts.lib.save_modules")

local doc = [[example: saveMods.register("not_a_module", nil, nil)]]
saveMods.register("real_module_2", nil, nil)
"""

# The registry table itself escapes to an ARBITRARILY-named local (not
# saveMods/saveModules) and is called through THAT name -- untraceable
# by any fixed-receiver-name regex.
SYNTHETIC_LUA_REGISTER_UNTRACKED_REQUIRE_LOCAL = """\
local registry = require("scripts.lib.save_modules")

registry.register("untracked_via_registry_local", nil, nil)
"""

# The codebase's own sanctioned pattern: require()'d straight into a
# local literally named `saveMods`, then called by that name later --
# must NOT be flagged as an untracked binding (it's how every real
# call site in the repo is written).
SYNTHETIC_LUA_REGISTER_SANCTIONED_LOCAL = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register("sanctioned_module", nil, nil)
"""

# The already-canonical `saveMods` local is re-aliased into a SECOND,
# arbitrarily-named local, and called through THAT -- a second-level
# alias no fixed-receiver-name matcher can trace, one hop further than
# SYNTHETIC_LUA_REGISTER_UNTRACKED_REQUIRE_LOCAL.
SYNTHETIC_LUA_REGISTER_BARE_NAME_REALIAS = """\
local saveMods = require("scripts.lib.save_modules")
local registry = saveMods
registry.register("untracked_via_bare_alias", nil, nil)
"""

# The already-canonical `saveMods` local is re-aliased into a TABLE KEY
# (bracket-indexed) instead of a bare local/global name -- one further
# hop past SYNTHETIC_LUA_REGISTER_BARE_NAME_REALIAS/GLOBAL_REALIAS that
# neither of those catches, since the assignment target isn't a bare
# identifier at all.
SYNTHETIC_LUA_REGISTER_TABLE_KEY_REALIAS = """\
local saveMods = require("scripts.lib.save_modules")
local holder = {}
holder["registry"] = saveMods
holder["registry"].register("untracked_table_alias", nil, nil)
"""

# Dot-field sibling of the table-key case above -- `holder.registry =
# saveMods` instead of `holder["registry"] = saveMods`.
SYNTHETIC_LUA_REGISTER_DOT_FIELD_REALIAS = """\
local saveMods = require("scripts.lib.save_modules")
local holder = {}
holder.registry = saveMods
holder.registry.register("untracked_dot_field_alias", nil, nil)
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

# A direct call reached off `package.loaded["scripts.lib.save_modules"]`
# with no local binding at all -- `require()` itself reads/writes
# exactly this cache slot, so this is a THIRD spelling of the identical
# singleton table (alongside the bare local name and the require()-
# chained form), just as directly traceable as either.
SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_CHAINED_CALL = """\
package.loaded["scripts.lib.save_modules"].register("pkg_loaded_module", nil, nil)
"""

# package.loaded[...].register is stored in a local and called THROUGH
# the alias -- the package.loaded sibling of
# SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_ALIASED.
SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_CHAINED_ALIASED = """\
local register = package.loaded["scripts.lib.save_modules"].register
register("aliased_pkg_loaded_module", nil, nil)
"""

# The registry TABLE (not just its `.register` function) is fetched via
# `package.loaded[...]` and stored in an arbitrary local, then called
# through THAT -- the package.loaded sibling of
# SYNTHETIC_LUA_REGISTER_UNTRACKED_REQUIRE_LOCAL. The direct-call
# receiver support only recognizes an IMMEDIATE `package.loaded[...]
# .register` chain, so this table-level escape needs its own tracking,
# symmetric with the require()-result escape check.
SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_TABLE_ESCAPE = """\
local registry = package.loaded["scripts.lib.save_modules"]
registry.register("untracked_via_package_loaded_alias", nil, nil)
"""

# The real registry definition file's own idiom, verbatim: fetch via
# `package.loaded[...] or {}` into the sanctioned `saveModules` local,
# then write it straight back to the same cache slot. Neither line is
# an escape -- the fetch lands on the sanctioned name, and the write is
# an assignment TARGET, not a value read. Must NOT be flagged.
SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_DEFINITION_ROUNDTRIP = """\
local saveModules = package.loaded["scripts.lib.save_modules"] or {}
package.loaded["scripts.lib.save_modules"] = saveModules

function saveModules.register(name, serializeFn, deserializeFn)
    saveModules.registry[name] = { serialize = serializeFn, deserialize = deserializeFn }
end
"""

# The module-name argument is a CONCATENATION, not a complete literal --
# `saveModules.register` (the real function) accepts and stores
# whatever this evaluates to at runtime ("unit_ai_untracked"), but the
# literal PREFIX alone ("unit_ai") is already a classified name. A
# scanner that captures just the prefix silently misreads this as a
# harmless re-registration of an already-classified module instead of a
# NEW, unclassified one.
SYNTHETIC_LUA_REGISTER_CONCATENATED_NAME = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register("unit_ai" .. "_untracked", function() end, function() end)
"""

# The receiver is wrapped in redundant parens -- `(saveMods)` is exactly
# as direct a call as bare `saveMods`, just with cosmetic grouping.
SYNTHETIC_LUA_REGISTER_PARENTHESIZED_RECEIVER = """\
local saveMods = require("scripts.lib.save_modules")

(saveMods).register("untracked_parenthesized", function() end, function() end)
"""

# Redundant parens nested to an ARBITRARY (here, 5-deep) level -- proves
# the fix generalizes to any depth in one shot, not just one more level
# past the single-paren case above.
SYNTHETIC_LUA_REGISTER_DEEPLY_PARENTHESIZED_RECEIVER = """\
local saveMods = require("scripts.lib.save_modules")

(((((saveMods))))).register("deeply_parenthesized", function() end, function() end)
"""

# Regression for the round-16 whitespace-drift bug: the registry's own
# function DEFINITION, indented (real code is never at column 0) --
# proves the parenthesized-receiver fix doesn't let match positions
# drift into leading indentation the way the first (reverted) `\(*\s*`
# attempt did, which would have made this misread as a dynamic-name call.
SYNTHETIC_LUA_REGISTER_INDENTED_DEFINITION = """\
local saveModules = package.loaded["scripts.lib.save_modules"] or {}
package.loaded["scripts.lib.save_modules"] = saveModules

    function saveModules.register(name, serializeFn, deserializeFn)
        saveModules.registry[name] = { serialize = serializeFn, deserialize = deserializeFn }
    end
"""

# The canonical name hidden as a TABLE CONSTRUCTOR field's value with an
# explicit bracket key -- `{ [1] = saveMods }` -- rather than the RHS
# of a subsequent assignment statement.
SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_BRACKET_KEY = """\
local saveMods = require("scripts.lib.save_modules")
local holder = { [1] = saveMods }
holder[1].register("untracked_via_table_constructor", nil, nil)
"""

# The named-key sibling of the bracket-key case above.
SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_NAMED_KEY = """\
local saveMods = require("scripts.lib.save_modules")
local holder = { registry = saveMods }
holder.registry.register("untracked_named_key", nil, nil)
"""

# The POSITIONAL sibling -- no explicit key at all, an implicit integer
# key (Lua's array-constructor convention).
SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_POSITIONAL = """\
local saveMods = require("scripts.lib.save_modules")
local holder = { saveMods }
holder[1].register("untracked_positional", nil, nil)
"""

# The canonical name used as a table constructor's KEY (not its value)
# -- `{ saveMods = require(...) }` -- an entirely different, unrelated
# entry that must NOT be mistaken for the value-aliasing case above.
SYNTHETIC_LUA_TABLE_CONSTRUCTOR_KEY_NAME_ONLY = """\
local saveMods = require("scripts.lib.save_modules")
local holder = { saveMods = require("some.other.module") }
"""

# The canonical name PARENTHESIZED inside a table constructor's value
# position -- `{ [1] = (saveMods) }` -- combining round 17's
# parenthesized-receiver support with round 19's table-constructor
# detection, which hadn't been composed together.
SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_PARENTHESIZED_VALUE = """\
local saveMods = require("scripts.lib.save_modules")
local holder = { [1] = (saveMods) }
holder[1].register("untracked_paren_constructor", nil, nil)
"""

# The symmetric sibling gap: a parenthesized RHS in a plain bare-alias
# ASSIGNMENT statement (not a table constructor) -- `local registry =
# (saveMods)`. Not itself reported by a review round, but the same
# missing parens tolerance as the table-constructor case, closed
# preemptively by sharing one fragment between both checks.
SYNTHETIC_LUA_REGISTER_BARE_NAME_PARENTHESIZED_REALIAS = """\
local saveMods = require("scripts.lib.save_modules")
local registry = (saveMods)
registry.register("untracked_paren_bare_realias", nil, nil)
"""

# `package.loaded`'s field access via BRACKET indexing instead of dot
# access -- `package["loaded"]` -- the dot-vs-bracket duality every
# OTHER field access in this scanner already tolerates, direct call form.
SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_CALL = """\
package["loaded"]["scripts.lib.save_modules"].register("bracket_pkg_module", nil, nil)
"""

# The bracket-indexed sibling escaping to an untracked local, the same
# way the dot form's package.loaded[...] table escape does.
SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_TABLE_ESCAPE = """\
local registry = package["loaded"]["scripts.lib.save_modules"]
registry.register("untracked_bracket_pkg", nil, nil)
"""

# The bracket-indexed sibling of the real registry's own fetch-into-
# sanctioned-local-then-write-back idiom -- must NOT be flagged, the
# same as the dot-form version isn't.
SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_DEFINITION_ROUNDTRIP = """\
local saveModules = package["loaded"]["scripts.lib.save_modules"] or {}
package["loaded"]["scripts.lib.save_modules"] = saveModules

function saveModules.register(name, serializeFn, deserializeFn)
    saveModules.registry[name] = { serialize = serializeFn, deserialize = deserializeFn }
end
"""

# The `.register` access reached via a Lua LONG-BRACKET string key
# instead of a quoted one -- `saveMods[ [[register]] ](...)` -- direct
# call form.
SYNTHETIC_LUA_REGISTER_LONGBRACKET_KEY_CALL = """\
local saveMods = require("scripts.lib.save_modules")

saveMods[ [[register]] ]("longbracket_key_module", nil, nil)
"""

# The `=`-padded sibling -- `[ [=[register]=] ]` -- of the long-bracket
# key case above.
SYNTHETIC_LUA_REGISTER_LONGBRACKET_EQ_KEY_CALL = """\
local saveMods = require("scripts.lib.save_modules")

saveMods[ [=[register]=] ]("eq_longbracket_key_module", nil, nil)
"""

# The long-bracket-key form stored in a local and called THROUGH the
# alias -- the long-bracket-key sibling of SYNTHETIC_LUA_REGISTER_ALIASED.
SYNTHETIC_LUA_REGISTER_LONGBRACKET_KEY_ALIASED = """\
local saveMods = require("scripts.lib.save_modules")

local register = saveMods[ [[register]] ]
register("aliased_longbracket_key_module", nil, nil)
"""

# An UNRELATED field whose name merely starts with "register" --
# `saveMods.registerFoo` -- must NOT be mistaken for `.register` access
# (the latent `\b`-boundary gap the long-bracket-key fix also closed).
SYNTHETIC_LUA_UNRELATED_REGISTER_PREFIXED_FIELD = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.registerFoo = 5
"""

# The module-path string `"scripts.lib.save_modules"` reached via a
# Lua LONG-BRACKET string instead of a quoted one --
# `package.loaded[ [[scripts.lib.save_modules]] ]` -- direct call form.
SYNTHETIC_LUA_REGISTER_LONGBRACKET_PACKAGE_LOADED_PATH_CALL = """\
package.loaded[ [[scripts.lib.save_modules]] ].register("longbracket_pkg_path_module", nil, nil)
"""

# The long-bracket-path form escaping to an untracked local -- the
# long-bracket-path sibling of SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_TABLE_ESCAPE.
SYNTHETIC_LUA_REGISTER_LONGBRACKET_PACKAGE_LOADED_PATH_TABLE_ESCAPE = """\
local registry = package.loaded[ [[scripts.lib.save_modules]] ]
registry.register("untracked_longbracket_pkg_path", nil, nil)
"""

# The require()-argument sibling of the long-bracket module-path case
# above -- `require([[scripts.lib.save_modules]])` -- not itself
# reported by a review round, but the same missing long-bracket
# tolerance for the module-path STRING, closed preemptively by sharing
# one fragment between require()'s argument and package.loaded[...]'s
# index.
SYNTHETIC_LUA_REGISTER_LONGBRACKET_REQUIRE_PATH_CALL = """\
local saveMods = require([[scripts.lib.save_modules]])

saveMods.register("longbracket_require_path_module", nil, nil)
"""

# The require()-chained sibling of the case above -- no local binding
# at all, the module path reached via long brackets directly.
SYNTHETIC_LUA_REGISTER_LONGBRACKET_REQUIRE_PATH_CHAINED_CALL = """\
require([[scripts.lib.save_modules]]).register("longbracket_require_path_chained_module", nil, nil)
"""

# Lua's function-call sugar: a call's SOLE argument can be a bare
# string literal with NO parens at all -- `require "path"` is exactly
# as valid, and exactly as live, a call as `require("path")`.
# Paren-free require, chained straight into `.register(...)`.
SYNTHETIC_LUA_REGISTER_PARENFREE_REQUIRE_CHAINED_CALL = """\
require "scripts.lib.save_modules".register("parenfree_require_module", nil, nil)
"""

# Paren-free require bound to the sanctioned local, then called normally.
SYNTHETIC_LUA_REGISTER_PARENFREE_REQUIRE_SANCTIONED_LOCAL = """\
local saveMods = require "scripts.lib.save_modules"

saveMods.register("parenfree_require_local_module", nil, nil)
"""

# Paren-free require escaping to an UNTRACKED local -- the paren-free
# sibling of SYNTHETIC_LUA_REGISTER_UNTRACKED_REQUIRE_LOCAL.
SYNTHETIC_LUA_REGISTER_PARENFREE_REQUIRE_UNTRACKED_LOCAL = """\
local registry = require "scripts.lib.save_modules"

registry.register("untracked_parenfree_require", nil, nil)
"""

# The symmetric sibling gap in `.register` itself, closed preemptively:
# `saveMods.register "modname"` -- a paren-free call with no parens at
# all, the same Lua feature applied to a DIFFERENT call site.
SYNTHETIC_LUA_REGISTER_PARENFREE_CALL = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register "parenfree_register_module"
"""

# The long-bracket sibling of the paren-free `.register` call above.
SYNTHETIC_LUA_REGISTER_PARENFREE_LONGBRACKET_CALL = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register [[parenfree_longbracket_register_module]]
"""

# A paren-free-SHAPED reference stored in a local (NOT called) -- must
# still be flagged as an alias, the same as the parenthesized form is.
SYNTHETIC_LUA_REGISTER_PARENFREE_SHAPED_ALIAS = """\
local saveMods = require("scripts.lib.save_modules")

local register = saveMods.register
register "aliased_parenfree_module"
"""

# The real registry's OWN function DEFINITION, isolated -- a Lua
# parameter list (`name, serializeFn, deserializeFn`) is syntactically
# indistinguishable from a call's argument list to a receiver+`(`
# matcher, and none of the bare parameter names satisfy a
# complete-literal check. Must NOT be misread as a "dynamic name" call.
SYNTHETIC_LUA_REGISTER_DEFINITION_ONLY = """\
function saveModules.register(name, serializeFn, deserializeFn)
    saveModules.registry[name] = { serialize = serializeFn, deserialize = deserializeFn }
end
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


def test_extract_fields_survives_fake_block_comment_delimiters_in_strings():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_FAKE_BLOCK_COMMENT_STRINGS,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "documented", "unclassified"],
           f"two string literals spelling out '{{-' and '-}}' in "
           f"DIFFERENT fields' types do not get mistaken for a block "
           f"comment's open/close, swallowing the field between them, "
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


def test_find_lua_register_aliases_ignores_package_loaded_chained_direct_call():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_CHAINED_CALL})
    expect(offenders == [],
           f"a package.loaded[...].register(...) DIRECT call is not "
           f"flagged as an alias, got {offenders}")


def test_find_lua_register_aliases_detects_package_loaded_chained_stored_reference():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_CHAINED_ALIASED})
    expect(offenders == ["scripts/fake.lua"],
           f"package.loaded[...].register stored in a local (not called "
           f"directly) is flagged, got {offenders}")


def test_extract_lua_registered_modules_finds_bracket_package_loaded_call():
    # Regression: `package["loaded"]` (bracket-indexed) is the same
    # cache slot as `package.loaded` (dot-accessed) -- must be
    # recognized as an equally direct call.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_CALL})
    names = [n for n, _ in found]
    expect(names == ["bracket_pkg_module"],
           f"a register() call through bracket-indexed package[\"loaded\"] "
           f"is extracted, got {names}")


def test_find_lua_register_aliases_ignores_bracket_package_loaded_direct_call():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_CALL})
    expect(offenders == [],
           f"a bracket-indexed package[\"loaded\"][...].register(...) "
           f"DIRECT call is not flagged as an alias, got {offenders}")


def test_extract_lua_registered_modules_finds_longbracket_key_call():
    # Regression: `.register` reached via a Lua LONG-BRACKET string key
    # (`saveMods[ [[register]] ]`) instead of a quoted one -- must be
    # recognized as an equally direct call.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_KEY_CALL})
    names = [n for n, _ in found]
    expect(names == ["longbracket_key_module"],
           f"a register() call through a long-bracket-string KEY is "
           f"extracted, got {names}")


def test_extract_lua_registered_modules_finds_longbracket_eq_key_call():
    # The `=`-padded sibling -- `[ [=[register]=] ]`.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_EQ_KEY_CALL})
    names = [n for n, _ in found]
    expect(names == ["eq_longbracket_key_module"],
           f"a register() call through an =-padded long-bracket-string "
           f"KEY is extracted, got {names}")


def test_find_lua_register_aliases_ignores_longbracket_key_direct_call():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_KEY_CALL})
    expect(offenders == [],
           f"a long-bracket-key-form DIRECT call is not flagged as an "
           f"alias, got {offenders}")


def test_find_lua_register_aliases_detects_longbracket_key_stored_reference():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_KEY_ALIASED})
    expect(offenders == ["scripts/fake.lua"],
           f"the long-bracket-key form stored in a local (not called "
           f"directly) is flagged, got {offenders}")


def test_find_lua_register_aliases_ignores_unrelated_register_prefixed_field():
    # Regression: the latent `\b`-boundary gap the long-bracket-key fix
    # also closed -- `saveMods.registerFoo` (an unrelated field that
    # merely starts with "register") must not be mistaken for
    # `.register` access.
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_UNRELATED_REGISTER_PREFIXED_FIELD})
    expect(offenders == [],
           f"an unrelated field merely starting with \"register\" is "
           f"not flagged as an alias, got {offenders}")


def test_extract_lua_registered_modules_finds_longbracket_package_loaded_path_call():
    # Regression: the module-path string `"scripts.lib.save_modules"`
    # reached via a Lua long-bracket string inside package.loaded[...]'s
    # index -- must be recognized as an equally direct call.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_PACKAGE_LOADED_PATH_CALL})
    names = [n for n, _ in found]
    expect(names == ["longbracket_pkg_path_module"],
           f"a register() call through a long-bracket-string "
           f"package.loaded PATH is extracted, got {names}")


def test_extract_lua_registered_modules_finds_longbracket_require_path_call():
    # The require()-argument sibling: `require([[scripts.lib.save_modules]])`
    # bound to a local, not itself reported but the same missing
    # tolerance, closed preemptively.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_REQUIRE_PATH_CALL})
    names = [n for n, _ in found]
    expect(names == ["longbracket_require_path_module"],
           f"a register() call reached through a require() call whose "
           f"argument is a long-bracket string is extracted, "
           f"got {names}")


def test_extract_lua_registered_modules_finds_longbracket_require_path_chained_call():
    # The require()-chained sibling -- no local binding, module path in
    # long brackets directly.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_REQUIRE_PATH_CHAINED_CALL})
    names = [n for n, _ in found]
    expect(names == ["longbracket_require_path_chained_module"],
           f"a require([[...]]).register(...) chained direct call is "
           f"extracted, got {names}")


def test_extract_lua_registered_modules_finds_parenfree_require_chained_call():
    # Regression: Lua's function-call sugar (`require "path"`, no
    # parens at all) chained straight into `.register(...)`.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_REQUIRE_CHAINED_CALL})
    names = [n for n, _ in found]
    expect(names == ["parenfree_require_module"],
           f"a paren-free require \"path\".register(...) chained direct "
           f"call is extracted, got {names}")


def test_extract_lua_registered_modules_finds_parenfree_require_sanctioned_local():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_REQUIRE_SANCTIONED_LOCAL})
    names = [n for n, _ in found]
    expect(names == ["parenfree_require_local_module"],
           f"a paren-free require bound to the sanctioned local is "
           f"extracted via the normal direct-call path, got {names}")


def test_find_untracked_registry_aliases_detects_parenfree_require_untracked_local():
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_REQUIRE_UNTRACKED_LOCAL})
    expect(offenders == ["scripts/fake.lua"],
           f"a paren-free require bound to an untracked local is "
           f"flagged, got {offenders}")


def test_extract_lua_registered_modules_finds_parenfree_register_call():
    # The symmetric sibling gap in `.register` itself:
    # `saveMods.register "modname"` -- paren-free sugar applied to a
    # DIFFERENT call site, closed preemptively.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_CALL})
    names = [n for n, _ in found]
    expect(names == ["parenfree_register_module"],
           f"a paren-free saveMods.register \"name\" call is extracted, "
           f"got {names}")


def test_extract_lua_registered_modules_finds_parenfree_longbracket_register_call():
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_LONGBRACKET_CALL})
    names = [n for n, _ in found]
    expect(names == ["parenfree_longbracket_register_module"],
           f"a paren-free saveMods.register [[name]] call is extracted, "
           f"got {names}")


def test_find_lua_register_aliases_ignores_parenfree_register_call():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_CALL})
    expect(offenders == [],
           f"a paren-free saveMods.register \"name\" DIRECT call is not "
           f"flagged as an alias, got {offenders}")


def test_find_lua_register_aliases_detects_parenfree_shaped_alias():
    # A paren-free-SHAPED reference stored in a local (NOT called) must
    # still be flagged as an alias, the same as the parenthesized form.
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_SHAPED_ALIAS})
    expect(offenders == ["scripts/fake.lua"],
           f"a register reference stored in a local, later called via "
           f"paren-free sugar, is flagged, got {offenders}")


def test_extract_lua_registered_modules_ignores_concatenated_name():
    # Regression: a module-name argument built via concatenation is NOT
    # a complete literal -- extraction must not silently capture just
    # the literal PREFIX ("unit_ai", already classified) as if it were
    # the whole (differently-named, unclassified) registration.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_CONCATENATED_NAME})
    expect(found == [],
           f"a register() call whose name argument is a concatenation "
           f"is not extracted as a registration of its literal prefix, "
           f"got {found}")


def test_find_lua_register_dynamic_names_detects_concatenated_name():
    offenders = find_lua_register_dynamic_names(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_CONCATENATED_NAME})
    expect(offenders == ["scripts/fake.lua"],
           f"a register() call with a concatenated (non-literal) name "
           f"argument is flagged, got {offenders}")


def test_extract_lua_registered_modules_finds_parenthesized_receiver():
    # `(saveMods).register(...)` is exactly as direct a call as bare
    # `saveMods.register(...)` -- must still be extracted normally.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENTHESIZED_RECEIVER})
    names = [n for n, _ in found]
    expect(names == ["untracked_parenthesized"],
           f"a register() call through a parenthesized receiver is "
           f"extracted, got {names}")


def test_find_lua_register_aliases_ignores_parenthesized_receiver_direct_call():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENTHESIZED_RECEIVER})
    expect(offenders == [],
           f"a parenthesized-receiver DIRECT call is not flagged as an "
           f"alias, got {offenders}")


def test_find_lua_register_dynamic_names_ignores_parenthesized_receiver():
    offenders = find_lua_register_dynamic_names(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENTHESIZED_RECEIVER})
    expect(offenders == [],
           f"a parenthesized-receiver call with a complete literal name "
           f"is not flagged as a dynamic name, got {offenders}")


def test_find_lua_register_dynamic_names_ignores_the_registry_own_definition():
    # Regression: `function saveModules.register(name, ...)` -- the real
    # registry's own DEFINITION -- is syntactically indistinguishable
    # from a call to a receiver+`(` matcher, and its bare parameter
    # names never satisfy a complete-literal check. Must not be
    # misread as a "dynamic name" call.
    offenders = find_lua_register_dynamic_names(
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_DEFINITION_ONLY})
    expect(offenders == [],
           f"the registry's own function definition is not flagged as "
           f"a dynamic name call, got {offenders}")


def test_extract_lua_registered_modules_finds_deeply_parenthesized_receiver():
    # Regression: the parens fix must generalize to ANY depth in one
    # shot, not just the single-level case -- proves it with 5 levels.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_DEEPLY_PARENTHESIZED_RECEIVER})
    names = [n for n, _ in found]
    expect(names == ["deeply_parenthesized"],
           f"a register() call through an arbitrarily deeply "
           f"parenthesized receiver is extracted, got {names}")


def test_find_lua_register_aliases_ignores_deeply_parenthesized_receiver_direct_call():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_DEEPLY_PARENTHESIZED_RECEIVER})
    expect(offenders == [],
           f"a deeply-parenthesized-receiver DIRECT call is not flagged "
           f"as an alias, got {offenders}")


def test_find_lua_register_dynamic_names_ignores_indented_definition():
    # Regression for the round-16 whitespace-drift bug: real code is
    # never at column 0, so the registry's own function definition,
    # indented, must still be excluded -- proves the fix doesn't let
    # match positions drift into leading indentation.
    offenders = find_lua_register_dynamic_names(
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_INDENTED_DEFINITION})
    expect(offenders == [],
           f"the registry's own INDENTED function definition is not "
           f"flagged as a dynamic name call, got {offenders}")


def test_find_lua_register_aliases_ignores_longbracket_string_prose():
    offenders = find_lua_register_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_LONGBRACKET_STRING_MENTIONING_REGISTER})
    expect(offenders == [],
           f"a long-bracket STRING literal mentioning \"saveMods.register\" "
           f"in its text is not mistaken for a live reference, "
           f"got {offenders}")


def test_find_untracked_registry_aliases_detects_arbitrary_local_name():
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_UNTRACKED_REQUIRE_LOCAL})
    expect(offenders == ["scripts/fake.lua"],
           f"require(\"scripts.lib.save_modules\") bound to an "
           f"arbitrarily-named local (not saveMods/saveModules) is "
           f"flagged, got {offenders}")


def test_find_untracked_registry_aliases_ignores_sanctioned_local_name():
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_SANCTIONED_LOCAL})
    expect(offenders == [],
           f"the codebase's own sanctioned pattern (local saveMods = "
           f"require(...)) is not flagged, got {offenders}")


def test_find_untracked_registry_aliases_ignores_chained_direct_call():
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_CALL})
    expect(offenders == [],
           f"require(...).register(...) chained directly (no local at "
           f"all) is not flagged as an untracked binding, got {offenders}")


def test_find_untracked_registry_aliases_ignores_prose_mention():
    # A mention of require("scripts.lib.save_modules") inside a string
    # literal is not real code.
    lua = 'local doc = "see require(\\"scripts.lib.save_modules\\")"\n'
    offenders = find_untracked_registry_aliases({"scripts/fake.lua": lua})
    expect(offenders == [],
           f"a string literal merely mentioning the require() call is "
           f"not flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_bare_name_realias():
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BARE_NAME_REALIAS})
    expect(offenders == ["scripts/fake.lua"],
           f"re-aliasing the already-canonical saveMods local into a "
           f"SECOND, arbitrarily-named local is flagged, got {offenders}")


def test_find_untracked_registry_aliases_ignores_sanctioned_local_use():
    # The canonical `saveMods` name itself, used directly (not
    # re-aliased), must never be flagged -- this is every real call
    # site in the repo.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_SANCTIONED_LOCAL})
    expect(offenders == [],
           f"using the canonical saveMods local directly is not "
           f"flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_global_realias():
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_GLOBAL_REALIAS})
    expect(offenders == ["scripts/fake.lua"],
           f"re-aliasing saveMods into a GLOBAL (non-local) variable is "
           f"flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_table_key_realias():
    # Regression: re-aliasing saveMods into a TABLE KEY
    # (`holder["registry"] = saveMods`) bypassed every earlier check --
    # those all assumed a bare identifier assignment target.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_TABLE_KEY_REALIAS})
    expect(offenders == ["scripts/fake.lua"],
           f"re-aliasing saveMods into a table key (bracket-indexed "
           f"field) is flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_dot_field_realias():
    # Dot-field sibling of the table-key case above.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_DOT_FIELD_REALIAS})
    expect(offenders == ["scripts/fake.lua"],
           f"re-aliasing saveMods into a dot-field table key is "
           f"flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_package_loaded_table_escape():
    # Regression: the registry TABLE fetched via `package.loaded[...]`
    # (not just its `.register` function) stored in an arbitrary local
    # and called through that -- the direct-call receiver support only
    # catches an IMMEDIATE `package.loaded[...].register` chain, so this
    # needed its own escape tracking, symmetric with require()'s.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_TABLE_ESCAPE})
    expect(offenders == ["scripts/fake.lua"],
           f"the registry table fetched via package.loaded[...] and "
           f"stored in an arbitrary local is flagged, got {offenders}")


def test_find_untracked_registry_aliases_ignores_package_loaded_definition_roundtrip():
    # The real registry definition file's own idiom: fetch via
    # `package.loaded[...] or {}` into the sanctioned `saveModules`
    # local, then write it straight back to the same cache slot.
    # Neither line is an escape.
    offenders = find_untracked_registry_aliases(
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_DEFINITION_ROUNDTRIP})
    expect(offenders == [],
           f"the registry's own package.loaded fetch-into-sanctioned-"
           f"local-then-write-back idiom is not flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_bracket_package_loaded_table_escape():
    # The bracket-indexed sibling of the package.loaded table escape --
    # `package["loaded"]` is the same cache slot under a second
    # spelling, and needs the SAME escape tracking as the dot form.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_TABLE_ESCAPE})
    expect(offenders == ["scripts/fake.lua"],
           f"the registry table fetched via bracket-indexed "
           f"package[\"loaded\"][...] and stored in an arbitrary local "
           f"is flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_longbracket_package_loaded_path_table_escape():
    # The long-bracket-path sibling of the package.loaded table escape
    # -- the module-path STRING (not the `loaded` field access) reached
    # via a long-bracket string, stored in an arbitrary local.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_PACKAGE_LOADED_PATH_TABLE_ESCAPE})
    expect(offenders == ["scripts/fake.lua"],
           f"the registry table fetched via a long-bracket-string "
           f"package.loaded PATH and stored in an arbitrary local is "
           f"flagged, got {offenders}")


def test_find_untracked_registry_aliases_ignores_bracket_package_loaded_definition_roundtrip():
    # The bracket-indexed sibling of the real registry's own fetch-
    # into-sanctioned-local-then-write-back idiom -- must NOT be
    # flagged, the same as the dot-form version isn't.
    offenders = find_untracked_registry_aliases(
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_DEFINITION_ROUNDTRIP})
    expect(offenders == [],
           f"the registry's own bracket-indexed package[\"loaded\"] "
           f"fetch-into-sanctioned-local-then-write-back idiom is not "
           f"flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_table_constructor_bracket_key():
    # Regression: hiding the registry table as a table CONSTRUCTOR
    # field's value (`{ [1] = saveMods }`) is structurally different
    # from a subsequent assignment statement and bypassed every earlier
    # check, which all assumed a `TARGET = value` statement shape.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_BRACKET_KEY})
    expect(offenders == ["scripts/fake.lua"],
           f"hiding saveMods as a table constructor's bracket-keyed "
           f"value is flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_table_constructor_named_key():
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_NAMED_KEY})
    expect(offenders == ["scripts/fake.lua"],
           f"hiding saveMods as a table constructor's named-key value "
           f"is flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_table_constructor_positional():
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_POSITIONAL})
    expect(offenders == ["scripts/fake.lua"],
           f"hiding saveMods as a table constructor's positional "
           f"(implicit-key) value is flagged, got {offenders}")


def test_find_untracked_registry_aliases_ignores_table_constructor_key_name():
    # saveMods used as a table constructor's KEY (not its value) is an
    # entirely unrelated entry and must not be mistaken for aliasing.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_TABLE_CONSTRUCTOR_KEY_NAME_ONLY})
    expect(offenders == [],
           f"saveMods used as a table constructor KEY (not a value) is "
           f"not flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_parenthesized_table_constructor_value():
    # Regression: round 17's parenthesized-receiver support and round
    # 19's table-constructor detection hadn't been composed -- a
    # parenthesized value inside a table constructor bypassed both.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_PARENTHESIZED_VALUE})
    expect(offenders == ["scripts/fake.lua"],
           f"a parenthesized value inside a table constructor is "
           f"flagged, got {offenders}")


def test_find_untracked_registry_aliases_detects_parenthesized_bare_realias():
    # The symmetric sibling gap in a plain assignment statement (not a
    # table constructor), closed preemptively via the shared fragment.
    offenders = find_untracked_registry_aliases(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BARE_NAME_PARENTHESIZED_REALIAS})
    expect(offenders == ["scripts/fake.lua"],
           f"a parenthesized RHS in a bare-alias assignment statement "
           f"is flagged, got {offenders}")


def test_find_untracked_registry_aliases_ignores_the_registry_own_reload_guard():
    # Regression: the real save_modules.lua's own reload-safety idiom
    # (`saveModules.registry = saveModules.registry or {}`) assigns a
    # SUB-TABLE field to itself, not the whole module table to a new
    # name. A `\b`-only word boundary check let "registry" (the FIELD
    # name in `saveModules.registry`) get matched as if it were a
    # freestanding variable being aliased from bare `saveModules` --
    # this is the exact false positive that broke the real repo.
    offenders = find_untracked_registry_aliases(
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_DEFINITION_WITH_ERROR_STRING})
    expect(offenders == [],
           f"the registry's own `X.registry = X.registry or {{}}` "
           f"reload guard is not flagged as an untracked alias, "
           f"got {offenders}")


def test_extract_lua_registered_modules_ignores_call_shaped_prose_in_string():
    # Regression: the DIRECT-CALL extractor (not just the alias check)
    # must also exclude matches inside string literals -- a doc string
    # whose content is shaped exactly like a real register() call must
    # not be extracted as one.
    found = extract_lua_registered_modules(
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PROSE_LOOKS_LIKE_CALL})
    names = [n for n, _ in found]
    expect(names == ["real_module_2"],
           f"a call-shaped mention inside a long-bracket string is not "
           f"extracted as a real registration; only the genuine call "
           f"on the next line is, got {names}")


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


def test_audit_detects_field_hidden_behind_fake_block_comment_strings():
    """Regression: two DIFFERENT fields' string-literal types spelling
    out '{-' and '-}' used to be read as a real block comment's
    open/close (block-comment stripping ran BEFORE string-awareness),
    silently swallowing the field between them from the audit
    entirely."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_FAKE_BLOCK_COMMENT_STRINGS},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo; documented/unclassified aren't real names here
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("unclassified" in v for v in violations),
           f"the field after the fake block-comment-shaped string "
           f"literals is still extracted and reported when "
           f"unclassified, got {violations}")


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


def test_audit_detects_unclassified_package_loaded_chained_module_registration():
    """Regression: `package.loaded["scripts.lib.save_modules"]` is a
    THIRD spelling of the identical singleton table require() itself
    reads/writes -- a module registered via
    package.loaded[...].register(...) with no local binding at all was
    invisible to extraction entirely (neither flagged unclassified nor
    alias-flagged), a worse gap than an alias since it went completely
    undetected."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_CHAINED_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for pkg_loaded_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("pkg_loaded_module" in v for v in violations),
           f"a module registered via package.loaded[...].register(...) "
           f"is reported when unclassified, got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a package.loaded[...].register(...) DIRECT call is not "
           f"ALSO reported as an aliasing violation, got {violations}")


def test_audit_detects_aliased_package_loaded_chained_registration():
    """The alias-bypass gap's package.loaded-chained sibling."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_CHAINED_ALIASED},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"package.loaded[...].register stored in a local is reported "
           f"as an aliasing violation, got {violations}")


def test_audit_detects_concatenated_module_name():
    """The req-10 acceptance test's dynamic-name variant: a register()
    call whose module-name argument is a concatenation (not a complete
    literal) is a real, live registration of a DIFFERENT, unclassified
    runtime name -- silently capturing just the classified literal
    prefix would hide this from the audit entirely."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_CONCATENATED_NAME},
        SYNTHETIC_INVENTORY_COMPLETE,  # classifies "unit_ai", not the concatenated runtime name
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "literal" in v.lower() for v in violations),
           f"a register() call with a concatenated name argument is "
           f"reported as a violation, got {violations}")


def test_audit_detects_unclassified_parenthesized_receiver_registration():
    """A module registered through a parenthesized receiver is ordinary,
    fully traceable Lua -- an unclassified module registered that way
    must still be reported (and NOT also as an alias)."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENTHESIZED_RECEIVER},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for untracked_parenthesized
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("untracked_parenthesized" in v for v in violations),
           f"a module registered through a parenthesized receiver is "
           f"reported when unclassified, got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a parenthesized-receiver DIRECT call is not ALSO reported "
           f"as an aliasing violation, got {violations}")


def test_audit_detects_unclassified_deeply_parenthesized_receiver_registration():
    """The req-10 acceptance test's arbitrary-depth-parens variant: a
    module registered through a 5-level-deep parenthesized receiver is
    just as live and traceable as the single-paren case -- proves the
    fix isn't depth-limited."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_DEEPLY_PARENTHESIZED_RECEIVER},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for deeply_parenthesized
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("deeply_parenthesized" in v for v in violations),
           f"a module registered through a deeply parenthesized "
           f"receiver is reported when unclassified, got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a deeply-parenthesized-receiver DIRECT call is not ALSO "
           f"reported as an aliasing violation, got {violations}")


def test_audit_does_not_flag_indented_definition_as_dynamic_name():
    """Regression for the round-16 whitespace-drift bug: the registry's
    own function definition, indented like real code, must not be
    misread as a dynamic-name call."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_INDENTED_DEFINITION},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("literal" in v.lower() for v in violations),
           f"the registry's own INDENTED function definition is not "
           f"reported as a dynamic-name violation, got {violations}")


def test_audit_detects_registration_via_table_constructor_bracket_key():
    """The req-10 acceptance test's table-constructor variant: the
    canonical saveMods local hidden as a table constructor's
    bracket-keyed value is a real, live registration path this audit
    must fail on."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_BRACKET_KEY},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"hiding saveMods as a table constructor's bracket-keyed "
           f"value is reported as an untracked-alias violation, "
           f"got {violations}")


def test_audit_detects_registration_via_table_constructor_positional():
    """The positional (implicit-key) sibling of the bracket-key case
    above."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_POSITIONAL},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"hiding saveMods as a table constructor's positional value "
           f"is reported as an untracked-alias violation, got {violations}")


def test_audit_does_not_flag_table_constructor_key_name_as_an_alias():
    """saveMods used as a table constructor's KEY (not its value) is an
    unrelated entry and must not be flagged."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_TABLE_CONSTRUCTOR_KEY_NAME_ONLY},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("alias" in v for v in violations),
           f"saveMods used as a table constructor KEY (not a value) is "
           f"not reported as an aliasing violation, got {violations}")


def test_audit_detects_registration_via_parenthesized_table_constructor_value():
    """Round 20's finding: a parenthesized value inside a table
    constructor combines two previously-separate fixes (round 17's
    parens, round 19's table constructors) that hadn't been composed."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_PARENTHESIZED_VALUE},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"a parenthesized value inside a table constructor is "
           f"reported as an untracked-alias violation, got {violations}")


def test_audit_detects_registration_via_parenthesized_bare_realias():
    """The symmetric sibling gap in a plain assignment statement,
    closed preemptively via the shared canonical-value fragment."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BARE_NAME_PARENTHESIZED_REALIAS},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"a parenthesized RHS in a bare-alias assignment statement "
           f"is reported as an untracked-alias violation, got {violations}")


def test_audit_does_not_flag_the_registry_own_definition_as_dynamic_name():
    """Regression: the registry's own `function saveModules.register(name,
    ...)` definition must not be misread as a dynamic-name call."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_DEFINITION_ONLY},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("literal" in v.lower() for v in violations),
           f"the registry's own function definition is not reported as "
           f"a dynamic-name violation, got {violations}")


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


def test_audit_does_not_flag_call_shaped_prose_as_unclassified_module():
    """Regression: a call-SHAPED mention inside a long-bracket string
    (e.g. a doc string) used to be extracted by the direct-call matcher
    itself as a live registration, failing CI for a module that never
    actually gets registered -- only the genuine call must be reported,
    and only because it's genuinely unclassified in this fixture."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PROSE_LOOKS_LIKE_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for either name
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("not_a_module" in v for v in violations),
           f"the call-shaped PROSE inside the string is not reported "
           f"as an unclassified module, got {violations}")
    expect(any("real_module_2" in v for v in violations),
           f"the genuine call on the next line IS reported when "
           f"unclassified, got {violations}")


def test_audit_detects_registration_via_untracked_require_local():
    """The req-10 acceptance test's arbitrary-local-name variant: a
    module registered by first binding require("scripts.lib.
    save_modules") to a NON-canonical local name and calling .register
    through it is a real, live registration in the actual Lua registry
    -- the audit must fail on the untracked binding itself, since it
    cannot trace what gets registered through an arbitrary name."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_UNTRACKED_REQUIRE_LOCAL},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "require" in v for v in violations),
           f"binding require(\"scripts.lib.save_modules\") to an "
           f"arbitrary local is reported as an untracked-binding "
           f"violation, got {violations}")


def test_audit_does_not_flag_sanctioned_require_local_as_untracked():
    """The codebase's own sanctioned pattern (local saveMods =
    require(...)) must not be reported as an untracked binding -- only
    as an unclassified module, since this fixture's inventory doesn't
    classify "sanctioned_module"."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_SANCTIONED_LOCAL},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("aliases the save-modules registry table" in v for v in violations),
           f"the sanctioned local pattern is not reported as an "
           f"untracked-alias violation, got {violations}")
    expect(any("sanctioned_module" in v for v in violations),
           f"the module registered through it IS reported when "
           f"unclassified (via the normal direct-call path), "
           f"got {violations}")


def test_audit_detects_registration_via_bare_name_realias():
    """The req-10 acceptance test's second-level-alias variant: a
    module registered by re-aliasing the ALREADY-canonical `saveMods`
    local into a second, arbitrary name and calling .register through
    THAT is a real, live registration in the actual Lua registry -- the
    audit must fail on the re-aliasing itself, one hop further than the
    require()-binding case above."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BARE_NAME_REALIAS},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"re-aliasing the canonical saveMods local into a second "
           f"local is reported as an untracked-alias violation, "
           f"got {violations}")


def test_audit_detects_registration_via_global_realias():
    """The req-10 acceptance test's non-local variant: `registry =
    saveMods` (no `local` keyword) is just as live a bypass as the
    `local` form -- Lua's `=` is unambiguously assignment."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_GLOBAL_REALIAS},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"re-aliasing saveMods into a global (non-local) variable is "
           f"reported as an untracked-alias violation, got {violations}")


def test_audit_detects_registration_via_table_key_realias():
    """One hop further than the bare-name/global re-alias cases: the
    canonical saveMods local re-aliased into a TABLE KEY
    (`holder["registry"] = saveMods`) is still a real, live registration
    path -- the assignment-target grammar must cover bracket/dot-field
    chains, not just bare identifiers."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_TABLE_KEY_REALIAS},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"re-aliasing saveMods into a table key is reported as an "
           f"untracked-alias violation, got {violations}")


def test_audit_detects_registration_via_dot_field_realias():
    """Dot-field sibling of the table-key case above."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_DOT_FIELD_REALIAS},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"re-aliasing saveMods into a dot-field table key is "
           f"reported as an untracked-alias violation, got {violations}")


def test_audit_detects_registration_via_package_loaded_table_escape():
    """The req-10 acceptance test's package.loaded-table variant: the
    registry table itself (not just its .register function) fetched via
    `package.loaded[...]` and stored in an arbitrary local is a real,
    live registration path this audit must fail on -- the P1 gap a
    canonical review round found in the direct-call-only receiver fix."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_TABLE_ESCAPE},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"the registry table fetched via package.loaded[...] and "
           f"stored in an arbitrary local is reported as an "
           f"untracked-alias violation, got {violations}")


def test_audit_does_not_flag_package_loaded_definition_roundtrip_as_an_alias():
    """The real registry definition file's own package.loaded
    fetch-into-sanctioned-local-then-write-back idiom must not be
    flagged."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_DEFINITION_ROUNDTRIP},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("alias" in v for v in violations),
           f"the registry's own package.loaded fetch/write-back idiom "
           f"is not reported as an aliasing violation, got {violations}")


def test_audit_detects_unclassified_bracket_package_loaded_module_registration():
    """Round 21's finding: `package["loaded"]` (bracket-indexed) is the
    same cache slot as `package.loaded` (dot-accessed) and just as
    direct a call -- an unclassified module registered through it must
    still be reported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for bracket_pkg_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("bracket_pkg_module" in v for v in violations),
           f"a module registered via bracket-indexed package[\"loaded\"] "
           f"is reported when unclassified, got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a bracket-indexed package[\"loaded\"] DIRECT call is not "
           f"ALSO reported as an aliasing violation, got {violations}")


def test_audit_detects_registration_via_bracket_package_loaded_table_escape():
    """The bracket-indexed sibling of the package.loaded table-escape
    gap."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_TABLE_ESCAPE},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"the registry table fetched via bracket-indexed "
           f"package[\"loaded\"][...] and stored in an arbitrary local "
           f"is reported as an untracked-alias violation, got {violations}")


def test_audit_does_not_flag_bracket_package_loaded_definition_roundtrip_as_an_alias():
    """The bracket-indexed sibling of the real registry definition
    file's own fetch/write-back idiom must not be flagged."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/save_modules.lua": SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_DEFINITION_ROUNDTRIP},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("alias" in v for v in violations),
           f"the registry's own bracket-indexed package[\"loaded\"] "
           f"fetch/write-back idiom is not reported as an aliasing "
           f"violation, got {violations}")


def test_audit_detects_unclassified_longbracket_key_module_registration():
    """Round 22's finding: `.register` reached via a Lua long-bracket
    string KEY (`saveMods[ [[register]] ]`) is ordinary, fully
    traceable Lua -- an unclassified module registered that way must
    still be reported (and NOT also as an alias)."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_KEY_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for longbracket_key_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("longbracket_key_module" in v for v in violations),
           f"a module registered via a long-bracket-string KEY is "
           f"reported when unclassified, got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a long-bracket-key-form DIRECT call is not ALSO reported "
           f"as an aliasing violation, got {violations}")


def test_audit_does_not_flag_unrelated_register_prefixed_field_as_an_alias():
    """Regression: `saveMods.registerFoo` (an unrelated field merely
    starting with "register") must not be mistaken for `.register`
    access."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_UNRELATED_REGISTER_PREFIXED_FIELD},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("alias" in v for v in violations),
           f"an unrelated field merely starting with \"register\" is "
           f"not reported as an aliasing violation, got {violations}")


def test_audit_detects_unclassified_longbracket_package_loaded_path_registration():
    """Round 23's finding: the module-path string reached via a Lua
    long-bracket string inside package.loaded[...]'s index is ordinary,
    fully traceable Lua -- an unclassified module registered that way
    must still be reported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_PACKAGE_LOADED_PATH_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for longbracket_pkg_path_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("longbracket_pkg_path_module" in v for v in violations),
           f"a module registered via a long-bracket-string "
           f"package.loaded PATH is reported when unclassified, "
           f"got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a long-bracket-path-form DIRECT call is not ALSO reported "
           f"as an aliasing violation, got {violations}")


def test_audit_detects_registration_via_longbracket_package_loaded_path_table_escape():
    """The long-bracket-path sibling of the package.loaded table-escape
    gap."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_PACKAGE_LOADED_PATH_TABLE_ESCAPE},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"the registry table fetched via a long-bracket-string "
           f"package.loaded PATH and stored in an arbitrary local is "
           f"reported as an untracked-alias violation, got {violations}")


def test_audit_detects_unclassified_longbracket_require_path_registration():
    """The require()-argument sibling of the round-23 fix, closed
    preemptively (not itself reported)."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_LONGBRACKET_REQUIRE_PATH_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for longbracket_require_path_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("longbracket_require_path_module" in v for v in violations),
           f"a module registered via require() with a long-bracket "
           f"string path is reported when unclassified, got {violations}")


def test_audit_detects_unclassified_parenfree_require_registration():
    """Round 24's finding: `require "path".register(...)` (Lua's
    function-call sugar, no parens) is ordinary, fully traceable Lua --
    an unclassified module registered that way must still be
    reported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_REQUIRE_CHAINED_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for parenfree_require_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("parenfree_require_module" in v for v in violations),
           f"a module registered via paren-free require \"path\" is "
           f"reported when unclassified, got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a paren-free require chained DIRECT call is not ALSO "
           f"reported as an aliasing violation, got {violations}")


def test_audit_detects_unclassified_parenfree_register_registration():
    """The symmetric sibling gap in `.register` itself, closed
    preemptively: `saveMods.register \"name\"` with no parens at all."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_CALL},
        SYNTHETIC_INVENTORY_COMPLETE,  # has no entry for parenfree_register_module
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("parenfree_register_module" in v for v in violations),
           f"a module registered via paren-free saveMods.register "
           f"\"name\" is reported when unclassified, got {violations}")
    expect(not any("alias" in v for v in violations),
           f"a paren-free saveMods.register \"name\" DIRECT call is "
           f"not ALSO reported as an aliasing violation, "
           f"got {violations}")


def test_audit_detects_parenfree_shaped_alias():
    """A paren-free-SHAPED reference stored in a local (NOT called)
    must still be flagged as an alias."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER_PARENFREE_SHAPED_ALIAS},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("scripts/fake.lua" in v and "alias" in v for v in violations),
           f"a register reference stored in a local, later called via "
           f"paren-free sugar, is reported as an aliasing violation, "
           f"got {violations}")


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
    record_sources, scripts_text_by_file, inventory_text, registered_ids = \
        _load_repo_state()
    violations = audit(record_sources, scripts_text_by_file, inventory_text,
                       registered_ids=registered_ids)
    expect(not violations,
           f"the real repo's inventory has no unclassified root-owner fields, "
           f"Lua save modules, or unregistered persistent save components, "
           f"got {violations}")


# ----- #760 component-registration checks --------------------------------

# The derived registered set, as `derive_registered_component_ids` would
# produce it for these fixtures: only "registered-comp" is truly wired
# into the registry (see the derivation fixtures further below).
SYNTHETIC_REGISTERED_IDS = {"registered-comp"}

# The inventory classifies a component persistent AND documents its
# (registered) ComponentId -- the well-formed case.
SYNTHETIC_INVENTORY_COMPONENT_OK = """\
# Fake inventory

### Save components

| Component DTO | ComponentId | Classification |
|---|---|---|
| `RegisteredDTO` | `registered-comp` | Persist exactly |
"""

# The inventory classifies a component persistent but its ComponentId is
# NOT registered in the Haskell source -- must fail.
SYNTHETIC_INVENTORY_COMPONENT_UNREGISTERED = """\
# Fake inventory

### Save components

| Component DTO | ComponentId | Classification |
|---|---|---|
| `RegisteredDTO` | `registered-comp` | Persist exactly |
| `GhostDTO` | `ghost-comp` | Persist exactly |
"""

# The would-be unregistered owner is instead classified Reset/Exclude --
# a rebuilt/reset/excluded owner requires no registration, so no failure.
SYNTHETIC_INVENTORY_COMPONENT_RESET_OK = """\
# Fake inventory

### Save components

| Component DTO | ComponentId | Classification |
|---|---|---|
| `RegisteredDTO` | `registered-comp` | Persist exactly |
| `GhostDTO` | `ghost-comp` | Reset to default |
"""


def test_component_check_accepts_registered_persistent_owner():
    from persistence_inventory_audit import (  # type: ignore
        find_component_registration_violations)
    v = find_component_registration_violations(
        SYNTHETIC_INVENTORY_COMPONENT_OK, SYNTHETIC_REGISTERED_IDS)
    expect(v == [],
           f"a persistent component whose ComponentId IS registered passes, "
           f"got {v}")


def test_component_check_flags_unregistered_persistent_owner():
    from persistence_inventory_audit import (  # type: ignore
        find_component_registration_violations)
    v = find_component_registration_violations(
        SYNTHETIC_INVENTORY_COMPONENT_UNREGISTERED, SYNTHETIC_REGISTERED_IDS)
    expect(any("ghost-comp" in x for x in v),
           f"a persistent Haskell save-component owner WITHOUT a registered "
           f"ComponentId fails the audit, got {v}")


def test_component_check_reset_owner_needs_no_registration():
    from persistence_inventory_audit import (  # type: ignore
        find_component_registration_violations)
    v = find_component_registration_violations(
        SYNTHETIC_INVENTORY_COMPONENT_RESET_OK, SYNTHETIC_REGISTERED_IDS)
    expect(v == [],
           f"an owner classified reset/rebuilt/excluded requires no component "
           f"registration and does NOT fail, got {v}")


def test_component_check_flags_registered_component_missing_a_row():
    from persistence_inventory_audit import (  # type: ignore
        find_component_registration_violations)
    # registry has "registered-comp" but the inventory documents no row
    # for it at all -- a new component owner landed without a decision.
    v = find_component_registration_violations(
        "# empty inventory\n", SYNTHETIC_REGISTERED_IDS)
    expect(any("registered-comp" in x for x in v),
           f"a registered component with no persistent inventory row fails "
           f"the audit, got {v}")


# ----- #760 registry-DERIVATION checks (round-4 review) ------------------
#
# The audit must derive its registered set from real registry membership,
# not from every `ComponentId "..."` literal that exists. These fixtures
# model that: `ghost-comp` HAS an id literal (and is documented) but is
# never added to `saveComponentRegistry`, so it must NOT be treated as
# registered.

# A registry list that registers ONE codec (regCodec) and the metadata
# component is wired by the envelope, not this list.
SYNTHETIC_REGISTRY_LIST = """\
saveComponentRegistry ∷ [RegisteredComponent]
saveComponentRegistry =
    [ registerComponent regCodec
    ]
"""

# The codec definitions: regCodec resolves to registeredComponentId;
# ghostCodec exists in source but is NOT in the registry list above.
SYNTHETIC_CODEC_SOURCE = """\
regCodec ∷ ComponentCodec RegDTO
regCodec = serializeCodec
    registeredComponentId 1 True []
    encodeReg (\\_ d → Right d) (const [])

ghostCodec ∷ ComponentCodec GhostDTO
ghostCodec = serializeCodec
    ghostComponentId 1 True []
    encodeGhost (\\_ d → Right d) (const [])
"""

# Both id identifiers are DEFINED here -- a naive literal grep would treat
# BOTH as registered. Only registeredComponentId is actually wired.
SYNTHETIC_ID_TYPES = """\
registeredComponentId ∷ ComponentId
registeredComponentId = ComponentId "registered-comp"
ghostComponentId ∷ ComponentId
ghostComponentId = ComponentId "ghost-comp"
metadataComponentId ∷ ComponentId
metadataComponentId = ComponentId "metadata"
"""

# The envelope wires in the metadata component as a direct spec tuple,
# exactly like the real World.Save.Envelope.
SYNTHETIC_ENVELOPE = """\
encodeSessionSnapshot meta snap =
    let metaSpec = (metadataComponentId, metadataComponentVersion, True
                   , S.encode meta)
        specs    = metaSpec : encodeComponentSpecs snap
    in encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion specs
"""


def test_derive_registered_ids_traces_real_membership_not_literals():
    from persistence_inventory_audit import (  # type: ignore
        derive_registered_component_ids)
    ids = derive_registered_component_ids(
        SYNTHETIC_REGISTRY_LIST, SYNTHETIC_CODEC_SOURCE, SYNTHETIC_ID_TYPES,
        SYNTHETIC_ENVELOPE)
    # regCodec -> registeredComponentId -> "registered-comp"; envelope
    # -> "metadata". ghost-comp is DEFINED but never registered.
    expect(ids == {"registered-comp", "metadata"},
           f"the derived set traces registry membership (registered-comp + "
           f"the envelope's metadata) and EXCLUDES the defined-but-"
           f"unregistered ghost-comp, got {ids}")


def test_derive_registered_ids_excludes_defined_but_unregistered_and_audit_flags_it():
    """The exact round-4 gap: an id literal defined + documented persistent
    but NOT wired into saveComponentRegistry must be flagged."""
    from persistence_inventory_audit import (  # type: ignore
        derive_registered_component_ids, find_component_registration_violations)
    registered = derive_registered_component_ids(
        SYNTHETIC_REGISTRY_LIST, SYNTHETIC_CODEC_SOURCE, SYNTHETIC_ID_TYPES,
        SYNTHETIC_ENVELOPE)
    expect("ghost-comp" not in registered,
           f"a defined-but-unregistered ComponentId is NOT in the derived "
           f"registered set, got {registered}")
    # The inventory documents ghost-comp as a persistent component; because
    # it is not truly registered, the linkage check must flag it -- which a
    # literal-grep audit (round-4 bug) would have missed.
    inventory = (
        "# Fake inventory\n\n### Save components\n\n"
        "| Component DTO | ComponentId | Classification |\n"
        "|---|---|---|\n"
        "| `RegDTO` | `registered-comp` | Persist exactly |\n"
        "| `MetaDTO` | `metadata` | Persist exactly |\n"
        "| `GhostDTO` | `ghost-comp` | Persist exactly |\n")
    v = find_component_registration_violations(inventory, registered)
    expect(any("ghost-comp" in x for x in v),
           f"a documented-persistent-but-unregistered component is flagged, "
           f"got {v}")


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
        test_extract_fields_survives_fake_block_comment_delimiters_in_strings,
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
        test_extract_lua_registered_modules_ignores_call_shaped_prose_in_string,
        test_find_lua_register_aliases_detects_stored_reference,
        test_find_lua_register_aliases_ignores_direct_calls,
        test_find_lua_register_aliases_ignores_the_definition_and_its_error_string,
        test_find_lua_register_aliases_ignores_bracket_form_direct_call,
        test_find_lua_register_aliases_detects_bracket_form_stored_reference,
        test_find_lua_register_aliases_ignores_require_chained_direct_call,
        test_find_lua_register_aliases_detects_require_chained_stored_reference,
        test_find_lua_register_aliases_ignores_package_loaded_chained_direct_call,
        test_find_lua_register_aliases_detects_package_loaded_chained_stored_reference,
        test_extract_lua_registered_modules_finds_bracket_package_loaded_call,
        test_find_lua_register_aliases_ignores_bracket_package_loaded_direct_call,
        test_extract_lua_registered_modules_finds_longbracket_key_call,
        test_extract_lua_registered_modules_finds_longbracket_eq_key_call,
        test_find_lua_register_aliases_ignores_longbracket_key_direct_call,
        test_find_lua_register_aliases_detects_longbracket_key_stored_reference,
        test_find_lua_register_aliases_ignores_unrelated_register_prefixed_field,
        test_extract_lua_registered_modules_finds_longbracket_package_loaded_path_call,
        test_extract_lua_registered_modules_finds_longbracket_require_path_call,
        test_extract_lua_registered_modules_finds_longbracket_require_path_chained_call,
        test_extract_lua_registered_modules_finds_parenfree_require_chained_call,
        test_extract_lua_registered_modules_finds_parenfree_require_sanctioned_local,
        test_find_untracked_registry_aliases_detects_parenfree_require_untracked_local,
        test_extract_lua_registered_modules_finds_parenfree_register_call,
        test_extract_lua_registered_modules_finds_parenfree_longbracket_register_call,
        test_find_lua_register_aliases_ignores_parenfree_register_call,
        test_find_lua_register_aliases_detects_parenfree_shaped_alias,
        test_extract_lua_registered_modules_ignores_concatenated_name,
        test_find_lua_register_dynamic_names_detects_concatenated_name,
        test_extract_lua_registered_modules_finds_parenthesized_receiver,
        test_find_lua_register_aliases_ignores_parenthesized_receiver_direct_call,
        test_find_lua_register_dynamic_names_ignores_parenthesized_receiver,
        test_find_lua_register_dynamic_names_ignores_the_registry_own_definition,
        test_extract_lua_registered_modules_finds_deeply_parenthesized_receiver,
        test_find_lua_register_aliases_ignores_deeply_parenthesized_receiver_direct_call,
        test_find_lua_register_dynamic_names_ignores_indented_definition,
        test_find_lua_register_aliases_ignores_longbracket_string_prose,
        test_find_untracked_registry_aliases_detects_arbitrary_local_name,
        test_find_untracked_registry_aliases_ignores_sanctioned_local_name,
        test_find_untracked_registry_aliases_ignores_chained_direct_call,
        test_find_untracked_registry_aliases_ignores_prose_mention,
        test_find_untracked_registry_aliases_detects_bare_name_realias,
        test_find_untracked_registry_aliases_ignores_sanctioned_local_use,
        test_find_untracked_registry_aliases_detects_global_realias,
        test_find_untracked_registry_aliases_detects_table_key_realias,
        test_find_untracked_registry_aliases_detects_dot_field_realias,
        test_find_untracked_registry_aliases_detects_package_loaded_table_escape,
        test_find_untracked_registry_aliases_ignores_package_loaded_definition_roundtrip,
        test_find_untracked_registry_aliases_detects_bracket_package_loaded_table_escape,
        test_find_untracked_registry_aliases_detects_longbracket_package_loaded_path_table_escape,
        test_find_untracked_registry_aliases_ignores_bracket_package_loaded_definition_roundtrip,
        test_find_untracked_registry_aliases_detects_table_constructor_bracket_key,
        test_find_untracked_registry_aliases_detects_table_constructor_named_key,
        test_find_untracked_registry_aliases_detects_table_constructor_positional,
        test_find_untracked_registry_aliases_ignores_table_constructor_key_name,
        test_find_untracked_registry_aliases_detects_parenthesized_table_constructor_value,
        test_find_untracked_registry_aliases_detects_parenthesized_bare_realias,
        test_find_untracked_registry_aliases_ignores_the_registry_own_reload_guard,
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
        test_audit_detects_field_hidden_behind_fake_block_comment_strings,
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
        test_audit_detects_unclassified_package_loaded_chained_module_registration,
        test_audit_detects_aliased_package_loaded_chained_registration,
        test_audit_detects_concatenated_module_name,
        test_audit_detects_unclassified_parenthesized_receiver_registration,
        test_audit_detects_unclassified_deeply_parenthesized_receiver_registration,
        test_audit_does_not_flag_indented_definition_as_dynamic_name,
        test_audit_detects_registration_via_table_constructor_bracket_key,
        test_audit_detects_registration_via_table_constructor_positional,
        test_audit_does_not_flag_table_constructor_key_name_as_an_alias,
        test_audit_detects_registration_via_parenthesized_table_constructor_value,
        test_audit_detects_registration_via_parenthesized_bare_realias,
        test_audit_does_not_flag_the_registry_own_definition_as_dynamic_name,
        test_audit_does_not_flag_longbracket_string_prose_as_an_alias,
        test_audit_does_not_flag_call_shaped_prose_as_unclassified_module,
        test_audit_detects_registration_via_untracked_require_local,
        test_audit_does_not_flag_sanctioned_require_local_as_untracked,
        test_audit_detects_registration_via_bare_name_realias,
        test_audit_detects_registration_via_global_realias,
        test_audit_detects_registration_via_table_key_realias,
        test_audit_detects_registration_via_dot_field_realias,
        test_audit_detects_registration_via_package_loaded_table_escape,
        test_audit_does_not_flag_package_loaded_definition_roundtrip_as_an_alias,
        test_audit_detects_unclassified_bracket_package_loaded_module_registration,
        test_audit_detects_registration_via_bracket_package_loaded_table_escape,
        test_audit_does_not_flag_bracket_package_loaded_definition_roundtrip_as_an_alias,
        test_audit_detects_unclassified_longbracket_key_module_registration,
        test_audit_does_not_flag_unrelated_register_prefixed_field_as_an_alias,
        test_audit_detects_unclassified_longbracket_package_loaded_path_registration,
        test_audit_detects_registration_via_longbracket_package_loaded_path_table_escape,
        test_audit_detects_unclassified_longbracket_require_path_registration,
        test_audit_detects_unclassified_parenfree_require_registration,
        test_audit_detects_unclassified_parenfree_register_registration,
        test_audit_detects_parenfree_shaped_alias,
        test_audit_does_not_flag_the_registry_definition_as_an_alias,
        test_audit_detects_intentionally_unclassified_field,
        test_audit_detects_intentionally_unclassified_lua_module,
        test_audit_against_the_real_repo,
        test_component_check_accepts_registered_persistent_owner,
        test_component_check_flags_unregistered_persistent_owner,
        test_component_check_reset_owner_needs_no_registration,
        test_component_check_flags_registered_component_missing_a_row,
        test_derive_registered_ids_traces_real_membership_not_literals,
        test_derive_registered_ids_excludes_defined_but_unregistered_and_audit_flags_it,
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
