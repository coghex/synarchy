#!/usr/bin/env python3
"""End-to-end audit verdicts for every Lua registration form (#2138).

Forty-four groups in two fragments, matching the aggregate's order:

  `TESTS_LITERAL_FORMS`           5 groups over the plain string
                                  spellings -- multiline, single-quoted,
                                  long-bracket, and the two dash-in-string
                                  cases;
  `TESTS_ALIAS_AND_ESCAPE_FORMS`  39 groups over aliases, dynamic names,
                                  registry-table escapes and their
                                  false-positive controls.

Each passes a fixture from `fixtures_lua` through the composed `audit()`
and asserts the classification violation it must or must not raise --
the same inputs `lua_parser.py` checks at the scanner. This module owns
no fixture of its own: sharing them with the parser family is what makes
the two views comparable.
"""
from __future__ import annotations

from .support import expect
from .fixtures_haskell import (
    FAKE_ROOT_RECORDS,
    SYNTHETIC_ENGINE_ENV,
)
from .fixtures_inventory import SYNTHETIC_INVENTORY_COMPLETE
from .fixtures_lua import (
    SYNTHETIC_LUA_LONGBRACKET_STRING_MENTIONING_REGISTER,
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
from persistence_inventory_audit import audit  # type: ignore


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


# ----- Registry ----------------------------------------------------------
#: The 44 groups this family owns, in the aggregate's order. Two
#: fragments, because the inventory's owner-scoping and taxonomy groups
#: run between them.

#: 5 groups over the plain string spellings of a registration.
TESTS_LITERAL_FORMS = (
    test_audit_detects_module_registered_across_multiple_lines,
    test_audit_detects_module_registered_after_dash_string,
    test_audit_detects_single_quoted_module_registration,
    test_audit_detects_longbracket_module_registration,
    test_audit_detects_module_registered_after_longbracket_dash_string,
)

#: 39 groups over aliases, dynamic names and registry-table escapes,
#: each paired with the false-positive control that keeps a definition, a
#: reload guard, a string or prose from matching.
TESTS_ALIAS_AND_ESCAPE_FORMS = (
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
)

#: The family's complete inventory: its fragments, in their own order.
TESTS = TESTS_LITERAL_FORMS + TESTS_ALIAS_AND_ESCAPE_FORMS
