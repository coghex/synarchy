#!/usr/bin/env python3
"""Synthetic Lua sources shared between the two Lua families (#2138).

Each constant is one registration, alias, dynamic-name or registry-escape
spelling. The `lua_parser` family feeds it to the scanner directly and
asserts on the extracted names; the `lua_audit` family feeds the same text
through `audit()` and asserts on the violation it does or does not raise.
Sharing one fixture between the two is deliberate: it is what makes the
direct and end-to-end views provably the same input.

A spelling only one of the two families exercises stays with that family
(#2138 requirement 14). This module imports no case owner.
"""
from __future__ import annotations



SYNTHETIC_LUA_REGISTER = """\
local saveMods = require("scripts.lib.save_modules")

local function init()
    saveMods.register("unit_ai",
        function() return "blob" end,
        function(b) end)
end

-- saveMods.register("commented_out", nil, nil)
"""

# A register() call whose arguments span multiple lines.
SYNTHETIC_LUA_REGISTER_MULTILINE = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register(
    "multiline_module",
    function() return "blob" end,
    function(b) end)
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

# Lua's function-call sugar: a call's SOLE argument can be a bare
# string literal with NO parens at all -- `require "path"` is exactly
# as valid, and exactly as live, a call as `require("path")`.
# Paren-free require, chained straight into `.register(...)`.
SYNTHETIC_LUA_REGISTER_PARENFREE_REQUIRE_CHAINED_CALL = """\
require "scripts.lib.save_modules".register("parenfree_require_module", nil, nil)
"""

# The symmetric sibling gap in `.register` itself, closed preemptively:
# `saveMods.register "modname"` -- a paren-free call with no parens at
# all, the same Lua feature applied to a DIFFERENT call site.
SYNTHETIC_LUA_REGISTER_PARENFREE_CALL = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register "parenfree_register_module"
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
