#!/usr/bin/env python3
"""Direct Lua scanner cases: every registration spelling (#2138).

Seventy groups against `persistence_inventory_audit_lua`'s four public
scanners, asserting on extracted names rather than on an audit verdict:

  `extract_lua_registered_modules`  the module name a registration names;
  `find_lua_register_aliases`       a stored reference to the register fn;
  `find_lua_register_dynamic_names` a name built rather than written;
  `find_untracked_registry_aliases` the registry table escaping by local,
                                    global, table field, table
                                    constructor or `package.loaded`.

The spellings covered are quoted and long-bracket strings, parenthesized
and paren-free calls, `require` / `package.loaded` / bracketed access,
and parenthesized receivers -- each paired with the false-positive
control that keeps it from matching a definition, a reload guard, a
string, a comment or prose.

`lua_audit.py` runs the same fixtures through `audit()`; this module
never calls it.
"""
from __future__ import annotations

from .support import expect
from .fixtures_lua import (
    SYNTHETIC_LUA_LONGBRACKET_STRING_MENTIONING_REGISTER,
    SYNTHETIC_LUA_REGISTER,
    SYNTHETIC_LUA_REGISTER_AFTER_DASH_STRING,
    SYNTHETIC_LUA_REGISTER_AFTER_LONGBRACKET_DASH_STRING,
    SYNTHETIC_LUA_REGISTER_ALIASED,
    SYNTHETIC_LUA_REGISTER_BARE_NAME_PARENTHESIZED_REALIAS,
    SYNTHETIC_LUA_REGISTER_BARE_NAME_REALIAS,
    SYNTHETIC_LUA_REGISTER_BRACKET_ALIASED,
    SYNTHETIC_LUA_REGISTER_BRACKET_CALL,
    SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_CALL,
    SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_DEFINITION_ROUNDTRIP,
    SYNTHETIC_LUA_REGISTER_BRACKET_PACKAGE_LOADED_TABLE_ESCAPE,
    SYNTHETIC_LUA_REGISTER_CONCATENATED_NAME,
    SYNTHETIC_LUA_REGISTER_DEEPLY_PARENTHESIZED_RECEIVER,
    SYNTHETIC_LUA_REGISTER_DEFINITION_ONLY,
    SYNTHETIC_LUA_REGISTER_DEFINITION_WITH_ERROR_STRING,
    SYNTHETIC_LUA_REGISTER_DOT_FIELD_REALIAS,
    SYNTHETIC_LUA_REGISTER_GLOBAL_REALIAS,
    SYNTHETIC_LUA_REGISTER_INDENTED_DEFINITION,
    SYNTHETIC_LUA_REGISTER_LONGBRACKET,
    SYNTHETIC_LUA_REGISTER_LONGBRACKET_KEY_CALL,
    SYNTHETIC_LUA_REGISTER_LONGBRACKET_LEVELED,
    SYNTHETIC_LUA_REGISTER_LONGBRACKET_PACKAGE_LOADED_PATH_CALL,
    SYNTHETIC_LUA_REGISTER_LONGBRACKET_PACKAGE_LOADED_PATH_TABLE_ESCAPE,
    SYNTHETIC_LUA_REGISTER_LONGBRACKET_REQUIRE_PATH_CALL,
    SYNTHETIC_LUA_REGISTER_MULTILINE,
    SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_CHAINED_ALIASED,
    SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_CHAINED_CALL,
    SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_DEFINITION_ROUNDTRIP,
    SYNTHETIC_LUA_REGISTER_PACKAGE_LOADED_TABLE_ESCAPE,
    SYNTHETIC_LUA_REGISTER_PARENFREE_CALL,
    SYNTHETIC_LUA_REGISTER_PARENFREE_REQUIRE_CHAINED_CALL,
    SYNTHETIC_LUA_REGISTER_PARENFREE_SHAPED_ALIAS,
    SYNTHETIC_LUA_REGISTER_PARENTHESIZED_RECEIVER,
    SYNTHETIC_LUA_REGISTER_PROSE_LOOKS_LIKE_CALL,
    SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_ALIASED,
    SYNTHETIC_LUA_REGISTER_REQUIRE_CHAINED_CALL,
    SYNTHETIC_LUA_REGISTER_SANCTIONED_LOCAL,
    SYNTHETIC_LUA_REGISTER_SINGLE_QUOTED,
    SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_BRACKET_KEY,
    SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_PARENTHESIZED_VALUE,
    SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_POSITIONAL,
    SYNTHETIC_LUA_REGISTER_TABLE_KEY_REALIAS,
    SYNTHETIC_LUA_REGISTER_UNTRACKED_REQUIRE_LOCAL,
    SYNTHETIC_LUA_TABLE_CONSTRUCTOR_KEY_NAME_ONLY,
    SYNTHETIC_LUA_UNRELATED_REGISTER_PREFIXED_FIELD,
)
from persistence_inventory_audit_lua import (  # type: ignore
    extract_lua_registered_modules,
    find_lua_register_aliases,
    find_lua_register_dynamic_names,
    find_untracked_registry_aliases,
)


# A register() call written with whitespace around the dot (legal Lua).
SYNTHETIC_LUA_REGISTER_SPACED_DOT = """\
local saveMods = require("scripts.lib.save_modules")

saveMods . register("spaced_dot_module", nil, nil)
"""

# The named-key sibling of the bracket-key case above.
SYNTHETIC_LUA_REGISTER_TABLE_CONSTRUCTOR_NAMED_KEY = """\
local saveMods = require("scripts.lib.save_modules")
local holder = { registry = saveMods }
holder.registry.register("untracked_named_key", nil, nil)
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

# The require()-chained sibling of the case above -- no local binding
# at all, the module path reached via long brackets directly.
SYNTHETIC_LUA_REGISTER_LONGBRACKET_REQUIRE_PATH_CHAINED_CALL = """\
require([[scripts.lib.save_modules]]).register("longbracket_require_path_chained_module", nil, nil)
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

# The long-bracket sibling of the paren-free `.register` call above.
SYNTHETIC_LUA_REGISTER_PARENFREE_LONGBRACKET_CALL = """\
local saveMods = require("scripts.lib.save_modules")

saveMods.register [[parenfree_longbracket_register_module]]
"""


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


# ----- Registry ----------------------------------------------------------
#: The 70 groups this family owns, in the aggregate's order. One
#: unbroken block: every direct scanner case runs before the first
#: end-to-end group of any family.
TESTS = (
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
)
