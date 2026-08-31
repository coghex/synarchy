#!/usr/bin/env python3
"""Unit tests for lua_registration_audit.py (issue #1996).

Mirrors tools/test_lua_duplicate_function_audit.py: import the real
audit's `main()` and drive it over synthetic temporary roots, so these
tests exercise the shipped discovery, registrar-grammar, lexing and
scope-resolution paths. A test carrying its own private copy of the
scanner would happily pass while the real gate stayed blind, and the
defects this gate exists to prevent are all false-GREENS.

Every rule is pinned in BOTH directions: a positive fixture that must
pass, and a negative fixture that must exit nonzero AND print a
diagnostic naming the real problem, attributed to a file and a line. A
checker that simply failed everything, or failed with an unattributed
message, cannot pass this suite.

The two nonzero outcomes are pinned apart as well. A *finding* (exit 1)
is the semantic defect the gate hunts; a *certification failure*
(exit 2) is the analyzer declining to vouch for an input. A fixture that
should fail loudly must not pass by being reported as an ordinary
finding, and vice versa.

Among the failing cases is a fixture reproducing `UI.setSpriteColor`,
the live defect #1914 fixed in `7f46468e`, so that defect stays
permanently detectable after its fix.

Every fixture is built in its own temporary directory and passed to
`main()` as an explicit root; nothing here reads or writes the shipped
scripts/ or src/ trees.

Usage:
  python3 tools/test_lua_registration_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from lua_registration_audit import (  # type: ignore
    EXIT_CERTIFICATION, EXIT_FINDINGS, EXIT_OK, main)

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


# ---------------------------------------------------------------------------
# Fixture construction
# ---------------------------------------------------------------------------

REGISTRAR_DIR = "src/Engine/Scripting/Lua/API/Register"


def _write(root: Path, rel: str, body: str) -> None:
    path = root / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def registrar(namespace: str, verbs: list[str], *, augment: bool = False) -> str:
    """One registrar module in the accepted grammar.

    `augment=True` reproduces Register/Debug.hs's shape: the block opens
    from the existing global that openlibs installed, recreates the table
    only on the branch where that global is not a table, and installs on
    the other.
    """
    lines = [
        f"module Engine.Scripting.Lua.API.Register.{namespace.capitalize()}",
        f"  ( register{namespace.capitalize()}API",
        "  ) where",
        "",
        "import Engine.Scripting.Lua.API.Internal (registerLuaFunction)",
        "import qualified HsLua as Lua",
        "",
        f"register{namespace.capitalize()}API :: Lua.LuaE Lua.Exception ()",
        f"register{namespace.capitalize()}API = do",
    ]
    if augment:
        lines += [
            f'  _ <- Lua.getglobal (Lua.Name "{namespace}")',
            "  isTbl <- Lua.istable (-1)",
            "  unless isTbl $ do",
            "    Lua.pop 1",
            "    Lua.newtable",
        ]
    else:
        lines.append("  Lua.newtable")
    lines += [f'  registerLuaFunction "{verb}" (someFn)' for verb in verbs]
    if augment:
        lines += ["  if isTbl", "    then Lua.pop 1",
                  f'    else Lua.setglobal (Lua.Name "{namespace}")']
    else:
        lines.append(f'  Lua.setglobal (Lua.Name "{namespace}")')
    return "\n".join(lines) + "\n"


def build(registrars: dict[str, list[str]], scripts: dict[str, str],
          *, augment: set[str] | None = None,
          raw_registrars: dict[str, str] | None = None) -> Path:
    """Materialize a complete fixture tree and return its root."""
    root = Path(tempfile.mkdtemp(prefix="lua_reg_audit_"))
    augment = augment or set()
    for namespace, verbs in registrars.items():
        _write(root, f"{REGISTRAR_DIR}/{namespace.capitalize()}.hs",
               registrar(namespace, verbs, augment=namespace in augment))
    for name, body in (raw_registrars or {}).items():
        _write(root, f"{REGISTRAR_DIR}/{name}.hs", body)
    for rel, body in scripts.items():
        _write(root, rel, body)
    return root


def run(root: Path) -> tuple[int, str]:
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        code = main(["--root", str(root)])
    return code, out.getvalue() + err.getvalue()


# -- outcome assertions -----------------------------------------------------

def expect_clean(root: Path, what: str) -> str:
    code, output = run(root)
    expect(code == EXIT_OK, f"{what} must exit {EXIT_OK}, got {code}: {output!r}")
    return output


def expect_finding(root: Path, member: str, what: str, *, count: int = 1) -> str:
    code, output = run(root)
    expect(code == EXIT_FINDINGS,
           f"{what} must exit {EXIT_FINDINGS} (a finding), got {code}: {output!r}")
    expect(member in output, f"{what} must name {member} in its report, got: {output!r}")
    expect(f"{count} finding(s)" in output,
           f"{what} must report exactly {count} finding(s), got: {output!r}")
    return output


def expect_certification_failure(root: Path, needle: str, what: str) -> str:
    code, output = run(root)
    expect(code == EXIT_CERTIFICATION,
           f"{what} must exit {EXIT_CERTIFICATION} (a certification failure), "
           f"got {code}: {output!r}")
    expect("CERTIFICATION FAILURE" in output,
           f"{what} must say so explicitly, got: {output!r}")
    expect(needle in output, f"{what} must name {needle!r}, got: {output!r}")
    expect("lua registration audit:" not in output,
           f"{what} must not also print a clean-scan summary, got: {output!r}")
    return output


def expect_attributed(output: str, rel: str, line: int, what: str) -> None:
    expect(f"{rel}:{line}:" in output,
           f"{what} must be attributed to {rel}:{line}, got: {output!r}")


# ===========================================================================
# The motivating defect
# ===========================================================================

def test_ui_set_sprite_color_is_a_finding() -> None:
    """#1914's exact defect: bar.lua called UI.setSpriteColor, and the
    registered verb is UI.setColor. It survived from 9af2585c to
    7f46468e only because bar.setFillColor had no callers yet."""
    root = build(
        {"UI": ["setColor", "setSize"]},
        {"scripts/ui/bar.lua":
            "local bar = {}\n"
            "function bar.setFillColor(h, r, g, b, a)\n"
            "    UI.setSpriteColor(h, r, g, b, a)\n"
            "end\n"
            "return bar\n"})
    output = expect_finding(root, "UI.setSpriteColor", "the pre-#1914 bar.lua")
    expect_attributed(output, "scripts/ui/bar.lua", 3, "the pre-#1914 defect")


def test_the_post_1914_spelling_is_clean() -> None:
    """The same fixture with the registered verb must pass, so the test
    above is pinning the verb name and not merely the file."""
    root = build(
        {"UI": ["setColor", "setSize"]},
        {"scripts/ui/bar.lua":
            "local bar = {}\n"
            "function bar.setFillColor(h, r, g, b, a)\n"
            "    UI.setColor(h, r, g, b, a)\n"
            "end\n"
            "return bar\n"})
    expect_clean(root, "the post-#1914 bar.lua")


# ===========================================================================
# Mutation: renaming a registered verb must fail its call sites
# ===========================================================================

def test_renaming_a_registered_verb_fails_every_call_site() -> None:
    call_sites = ("engine.logInfo('a')\n"
                  "engine.logInfo('b')\n"
                  "local f = engine.logInfo\n")
    clean = build({"engine": ["logInfo"]}, {"scripts/a.lua": call_sites})
    expect_clean(clean, "call sites naming the registered verb")

    renamed = build({"engine": ["logInformation"]}, {"scripts/a.lua": call_sites})
    output = expect_finding(renamed, "engine.logInfo",
                            "call sites after the verb is renamed", count=3)
    for line in (1, 2, 3):
        expect_attributed(output, "scripts/a.lua", line, "each renamed call site")


def test_adding_a_registration_does_not_fail_on_the_count() -> None:
    """The 27/617 values are extraction oracles for one snapshot, not
    limits: an intentional, fully parsed addition is an ordinary change."""
    before = build({"engine": ["quit"]}, {"scripts/a.lua": "engine.quit()\n"})
    output = expect_clean(before, "a one-verb registrar")
    expect("1 registrations across 1 namespaces" in output,
           f"the summary must derive its counts from source, got: {output!r}")

    after = build({"engine": ["quit", "restart"]},
                  {"scripts/a.lua": "engine.quit()\nengine.restart()\n"})
    output = expect_clean(after, "the same registrar with a verb added")
    expect("2 registrations across 1 namespaces" in output,
           f"the added registration must be counted, got: {output!r}")


# ===========================================================================
# False-positive class 1 and 3: comments and string literals
# ===========================================================================

def test_comments_are_not_call_sites() -> None:
    root = build(
        {"engine": ["quit"]},
        {"scripts/a.lua":
            "-- engine.getBlood is gone; see engine.xxx\n"
            "--[[ engine.save was replaced by\n"
            "     engine.saveWorld ]]\n"
            "--[==[ engine.unloadTexture ]==]\n"
            "engine.quit()\n"})
    expect_clean(root, "short and long comments naming unregistered verbs")


def test_strings_are_not_call_sites() -> None:
    root = build(
        {"engine": ["quit", "loadScript"]},
        {"scripts/a.lua":
            'engine.loadScript("scripts/debug.lua")\n'
            "engine.loadScript('scripts/engine.missing.lua')\n"
            "local doc = [[ engine.vanished ]]\n"
            "local more = [==[ engine.alsoVanished ]==]\n"
            "engine.quit()\n"})
    expect_clean(root, "short and long strings naming unregistered verbs")


def test_the_same_spellings_outside_a_comment_or_string_are_findings() -> None:
    """The other direction: lexing is what suppresses the class, not a
    name-based allowlist that would hide the real thing too."""
    root = build(
        {"engine": ["quit"]},
        {"scripts/a.lua": "engine.getBlood()\nlocal x = engine.vanished\nengine.quit()\n"})
    output = expect_finding(root, "engine.getBlood", "the same spellings in code", count=2)
    expect("engine.vanished" in output,
           f"every unregistered reference must be reported, got: {output!r}")


# ===========================================================================
# False-positive class 5: longer dotted paths
# ===========================================================================

def test_only_the_first_hop_of_a_dotted_path_is_checked() -> None:
    """`unitAi.till.execute` is rooted at a module no registrar installs,
    so its `till.execute` tail is not a call site -- which is what the
    naive scanner got wrong. `item.getInfo.x` IS rooted at a namespace,
    so its first hop is checked and the rest is not."""
    root = build(
        {"item": ["getInfo"], "till": ["designate"]},
        {"scripts/a.lua":
            "local unitAi = require('scripts.unit_ai')\n"
            "unitAi.till.execute(1)\n"
            "unitAi.plant.utility(2)\n"
            "local r = {}\n"
            "local name = r.item.displayName\n"
            "local z = item.getInfo(1).nested\n"
            "till.designate(0, 0)\n"})
    expect_clean(root, "tails of longer dotted paths")


def test_the_first_hop_of_a_longer_path_is_still_checked() -> None:
    root = build({"item": ["getInfo"]},
                 {"scripts/a.lua": "local z = item.getMissing(1).nested\n"})
    expect_finding(root, "item.getMissing", "the first hop of a longer path")


# ===========================================================================
# False-positive class 4: lexical shadowing
# ===========================================================================

def test_a_local_shadowing_a_registered_global_is_not_a_finding() -> None:
    root = build(
        {"item": ["getInfo"], "combat": ["getState"]},
        {"scripts/a.lua":
            "local combat = require('scripts.unit_ai_combat')\n"
            "local item = { label = 'x', defName = 'y' }\n"
            "print(item.label, item.defName)\n"
            "combat.attackScore(1)\n"})
    expect_clean(root, "locals shadowing registered globals")


def test_the_same_members_without_the_local_are_findings() -> None:
    root = build(
        {"item": ["getInfo"]},
        {"scripts/a.lua": "print(item.label, item.defName)\n"})
    expect_finding(root, "item.label", "the same members with no local in scope", count=2)


def test_function_parameters_shadow() -> None:
    """scripts/preview_manager.lua's `unit` and `building` are function
    PARAMETERS, not loop variables, so parameter scope is load-bearing."""
    root = build(
        {"unit": ["getInfo"], "building": ["getInfo"]},
        {"scripts/a.lua":
            "local function buildUnitUI(unit, fbW, fbH)\n"
            "    for i, a in ipairs(unit.animations or {}) do print(i, a) end\n"
            "end\n"
            "local function buildBuildingUI(building, fbW)\n"
            "    for i, e in ipairs(building.entries or {}) do print(i, e) end\n"
            "end\n"
            "return { buildUnitUI, buildBuildingUI }\n"})
    expect_clean(root, "function parameters shadowing namespaces")


def test_method_definition_binds_self() -> None:
    root = build({"unit": ["getInfo"]},
                 {"scripts/a.lua":
                     "local t = {}\n"
                     "function t:step(unit)\n"
                     "    return self.value, unit.anything\n"
                     "end\n"})
    expect_clean(root, "an implicit self and a shadowing parameter")


def test_numeric_and_generic_for_variables_shadow() -> None:
    root = build(
        {"item": ["getInfo"], "unit": ["getInfo"]},
        {"scripts/a.lua":
            "for item = 1, 10 do print(item.notAVerb) end\n"
            "for _, unit in ipairs(roster) do print(unit.alsoNotAVerb) end\n"})
    expect_clean(root, "numeric and generic for variables shadowing namespaces")


def test_block_locals_shadow_and_the_global_is_restored_on_scope_exit() -> None:
    """The shadow must end with its block, or every later call site in
    the file silently stops being checked."""
    root = build(
        {"item": ["getInfo"]},
        {"scripts/a.lua":
            "do\n"
            "    local item = { label = 'x' }\n"
            "    print(item.label)\n"
            "end\n"
            "item.afterTheBlock()\n"})
    output = expect_finding(root, "item.afterTheBlock",
                            "a reference after the shadowing block closes")
    expect_attributed(output, "scripts/a.lua", 5, "the restored global")


def test_shadow_is_restored_after_a_function_body() -> None:
    root = build(
        {"unit": ["getInfo"]},
        {"scripts/a.lua":
            "local function f(unit)\n"
            "    return unit.whatever\n"
            "end\n"
            "unit.afterTheBody()\n"})
    expect_finding(root, "unit.afterTheBody", "a reference after the function body closes")


def test_a_local_declaration_does_not_shadow_its_own_right_hand_side() -> None:
    """Lua binds a `local`'s names only after its expression list, which
    is what makes `local blood = unit.getBlood(uid)` read the outer
    binding on the right of the `=`. Binding at the keyword instead would
    put the shadow over the very expression that reads through it."""
    root = build(
        {"blood": ["getPool"]},
        {"scripts/a.lua":
            "local blood = blood.notAVerb(uid)\n"
            "print(blood.field)\n"})
    output = expect_finding(root, "blood.notAVerb", "the right-hand side of its own local")
    expect("blood.field" not in output,
           f"the name must be shadowed AFTER the statement, got: {output!r}")


def test_a_local_nested_in_an_outer_locals_rhs_does_not_displace_it() -> None:
    """Deferred `local` bindings NEST. An expression list can hold a
    function body or a table constructor with `local` statements of its
    own; if the inner one displaced the outer, the outer would never bind
    and every later reference through that name would resolve to the
    global it was shadowing."""
    root = build(
        {"engine": ["quit"]},
        {"scripts/a.lua":
            "local engine = { localFunction = function()\n"
            "    local sentinel = true\n"
            "    return sentinel\n"
            "end }\n"
            "engine.localFunction()\n"})
    expect_clean(root, "a local declared inside an outer local's right-hand side")


def test_nested_pending_locals_survive_several_levels() -> None:
    root = build(
        {"engine": ["quit"], "unit": ["getInfo"]},
        {"scripts/a.lua":
            "local engine = { a = function()\n"
            "    local unit = { c = function() local d = 1; return d end }\n"
            "    return unit.c\n"
            "end }\n"
            "engine.a()\n"
            "unit.getInfo(1)\n"})
    expect_clean(root, "three levels of nested deferred locals")


def test_an_inner_nested_local_still_binds() -> None:
    """The other direction: the inner declaration must not be lost either."""
    root = build(
        {"unit": ["getInfo"]},
        {"scripts/a.lua":
            "local t = { f = function()\n"
            "    local unit = 1\n"
            "    return unit.notAVerb\n"
            "end }\n"})
    expect_clean(root, "the inner declaration of a nested pair")


def test_an_unshadowed_reference_beside_a_nested_local_is_still_a_finding() -> None:
    root = build(
        {"engine": ["quit"]},
        {"scripts/a.lua":
            "local t = { f = function() local sentinel = 1; return sentinel end }\n"
            "engine.vanished()\n"})
    expect_finding(root, "engine.vanished", "a real defect beside a nested local")


def test_multiple_local_names_all_shadow() -> None:
    root = build(
        {"item": ["getInfo"], "loot": ["roll"]},
        {"scripts/a.lua":
            "local item, loot = f(), g()\n"
            "print(item.a, loot.b)\n"})
    expect_clean(root, "a multiple-name local declaration")


def test_a_bare_local_declaration_shadows() -> None:
    root = build({"item": ["getInfo"]},
                 {"scripts/a.lua": "local item\nitem = {}\nprint(item.a)\n"})
    expect_clean(root, "a local declared with no initializer")


def test_local_function_is_visible_inside_its_own_body() -> None:
    root = build({"unit": ["getInfo"]},
                 {"scripts/a.lua":
                     "local function unit(n)\n"
                     "    if n > 0 then return unit.recurse end\n"
                     "end\n"})
    expect_clean(root, "a local function visible inside its own body")


def test_an_anonymous_function_local_is_not_visible_inside_its_own_body() -> None:
    """`local f = function() ... end` differs from `local function f`:
    the name is bound only after the expression, so a reference inside
    the body reaches the global."""
    root = build({"unit": ["getInfo"]},
                 {"scripts/a.lua":
                     "local unit = function(n)\n"
                     "    return unit.notAVerb\n"
                     "end\n"})
    expect_finding(root, "unit.notAVerb", "a reference inside an anonymous function body")


# ===========================================================================
# Call forms: the candidate is a member REFERENCE, not a parenthesised call
# ===========================================================================

def test_first_class_function_references_are_checked() -> None:
    """scripts/startup_loader.lua passes engine loaders as values. A
    parenthesis-only recognizer would never see them."""
    root = build(
        {"engine": ["loadMaterialYaml"]},
        {"scripts/a.lua":
            "local loaders = {\n"
            "    engine.loadMaterialYaml,\n"
            "    engine.loadVanishedYaml,\n"
            "}\n"
            "return loaders\n"})
    output = expect_finding(root, "engine.loadVanishedYaml", "a first-class function reference")
    expect("loadMaterialYaml" not in output,
           f"the registered reference must pass, got: {output!r}")


def test_table_constructor_call_syntax_is_checked() -> None:
    """`debug.recordOutcome{...}` and `structure.registerPackArt{...}`
    are live in the tree and carry no parentheses at all."""
    root = build(
        {"debug": ["recordOutcome"], "structure": ["registerPackArt"]},
        {"scripts/a.lua":
            "debug.recordOutcome{ ok = true }\n"
            "structure.registerPackArt{ id = 'x' }\n"
            "debug.recordVanished{ ok = true }\n"})
    expect_finding(root, "debug.recordVanished", "a table-constructor call")


def test_string_call_syntax_is_checked() -> None:
    root = build({"engine": ["logInfo"]},
                 {"scripts/a.lua": 'engine.logInfo"hello"\nengine.logVanished"hello"\n'})
    expect_finding(root, "engine.logVanished", "a string-literal call")


def test_sibling_references_on_one_line_are_isolated() -> None:
    root = build(
        {"engine": ["logInfo", "getTextWidth"]},
        {"scripts/a.lua":
            "engine.logInfo(engine.getTextWidth(f, s), engine.vanished(1))\n"})
    output = expect_finding(root, "engine.vanished",
                            "one bad sibling among three on a line")
    for good in ("engine.logInfo", "engine.getTextWidth"):
        expect(good not in output, f"{good} must not be reported, got: {output!r}")


def test_sibling_references_in_one_block_are_isolated() -> None:
    root = build(
        {"world": ["getTerrainAt", "getFluidAt"]},
        {"scripts/a.lua":
            "local function probe(gx, gy)\n"
            "    local t = world.getTerrainAt(gx, gy)\n"
            "    local u = world.getVanishedAt(gx, gy)\n"
            "    local v = world.getFluidAt(gx, gy)\n"
            "    return t, u, v\n"
            "end\n"})
    output = expect_finding(root, "world.getVanishedAt",
                            "one bad sibling among three in a block")
    expect_attributed(output, "scripts/a.lua", 3, "the isolated sibling")


# ===========================================================================
# Fail-loud: constructs that begin in the grammar and then leave it
# ===========================================================================

def test_a_method_call_on_a_namespace_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "engine:quit()\n"})
    output = expect_certification_failure(
        root, "method call", "a `:` method call on an engine namespace")
    expect_attributed(output, "scripts/a.lua", 1, "the method call")


def test_an_unclosed_block_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua": "local function f()\n    engine.quit()\n"})
    expect_certification_failure(root, "unclosed block", "a file with an unclosed block")


def test_a_stray_end_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "engine.quit()\nend\n"})
    expect_certification_failure(root, "never opened", "a file with a stray `end`")


def test_an_end_closing_the_wrong_block_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua": "repeat\n    engine.quit()\nend\n"})
    expect_certification_failure(root, "closes a `repeat` block",
                                 "an `end` closing a `repeat`")


def test_an_unclosed_bracket_is_a_certification_failure() -> None:
    """An unterminated expression leaves a Lua construct unclassified.
    Rejecting only SURPLUS closers would certify `engine.quit(` clean."""
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "engine.quit(\n"})
    output = expect_certification_failure(root, "unclosed '('",
                                          "a file ending inside an open call")
    expect_attributed(output, "scripts/a.lua", 1, "the unclosed bracket")


def test_an_unclosed_table_constructor_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "local t = {\n    a = 1\n"})
    expect_certification_failure(root, "unclosed '{'", "a file ending inside a table")


def test_a_mismatched_bracket_pair_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "engine.quit(1]\n"})
    expect_certification_failure(root, "closes a '('", "a bracket closed by the wrong kind")


def test_balanced_brackets_of_every_kind_are_clean() -> None:
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua": "engine.quit({ a = 1 }, (2), t[3])\n"})
    expect_clean(root, "balanced brackets of every kind")


def test_an_unterminated_string_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "local s = 'oops\nengine.quit()\n"})
    expect_certification_failure(root, "unterminated string",
                                 "a file with an unterminated string")


def test_an_unterminated_long_comment_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "--[[ oops\nengine.quit()\n"})
    expect_certification_failure(root, "unterminated long comment",
                                 "a file with an unterminated long comment")


def test_an_unreadable_function_header_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua": "local function f(1, b)\n    return b\nend\n"})
    expect_certification_failure(root, "parameter", "an unreadable parameter list")


def test_an_unreadable_for_header_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua": "for i, j do\n    engine.quit()\nend\n"})
    expect_certification_failure(root, "`for` header",
                                 "a `for` header that is neither numeric nor generic")


def test_an_unreadable_local_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "local 5 = 1\n"})
    expect_certification_failure(root, "`local`", "a `local` that names no variable")


def test_a_repeat_until_block_is_analyzed() -> None:
    """The until expression can still read the body's locals, so the
    frame must outlive the `until` keyword itself."""
    root = build(
        {"item": ["getInfo"], "engine": ["quit"]},
        {"scripts/a.lua":
            "repeat\n"
            "    local item = next()\n"
            "until item.done\n"
            "item.afterTheLoop()\n"})
    output = expect_finding(root, "item.afterTheLoop",
                            "a reference after a repeat block closes")
    expect("item.done" not in output,
           f"the until expression must still see the body local, got: {output!r}")


def test_an_end_inside_an_until_expression_does_not_close_the_repeat() -> None:
    """`end` is a boundary keyword, so an anonymous function inside the
    `until` expression must not retire the loop's frame early."""
    root = build(
        {"item": ["getInfo"]},
        {"scripts/a.lua":
            "repeat\n"
            "    local item = next()\n"
            "until (function() return item.done end)()\n"
            "item.afterTheLoop()\n"})
    output = expect_finding(root, "item.afterTheLoop",
                            "a repeat whose until expression nests a function")
    expect("item.done" not in output,
           f"the nested `end` must not unshadow the body local, got: {output!r}")


def test_lua_54_local_attributes_are_read() -> None:
    root = build({"item": ["getInfo"]},
                 {"scripts/a.lua": "local item <const> = f()\nprint(item.label)\n"})
    expect_clean(root, "a Lua 5.4 <const> local attribute")


# ===========================================================================
# The registrar side: every module and every construct must be certified
# ===========================================================================

HEADER = ("module Engine.Scripting.Lua.API.Register.X (registerXAPI) where\n"
          "import Engine.Scripting.Lua.API.Internal (registerLuaFunction)\n"
          "import qualified HsLua as Lua\n"
          "\n"
          "registerXAPI :: Lua.LuaE Lua.Exception ()\n"
          "registerXAPI = do\n")


def raw(body: str, name: str = "X") -> dict[str, str]:
    return {name: HEADER + body}


def test_the_import_list_is_not_a_registration() -> None:
    """Every registrar imports `registerLuaFunction` by name, and the
    module export list names its entry point. Neither is executable."""
    root = build({}, {"scripts/a.lua": "engine.quit()\n"},
                 raw_registrars=raw('  Lua.newtable\n'
                                    '  registerLuaFunction "quit" (quitFn env)\n'
                                    '  Lua.setglobal (Lua.Name "engine")\n', name="Engine"))
    output = expect_clean(root, "a registrar whose import names registerLuaFunction")
    expect("1 registrations across 1 namespaces" in output,
           f"only the executable registration must be counted, got: {output!r}")


def test_haskell_comments_and_literals_are_not_registrations() -> None:
    root = build({}, {"scripts/a.lua": "engine.ghost()\n"},
                 raw_registrars=raw(
                     '  Lua.newtable\n'
                     '  -- registerLuaFunction "ghost" (ghostFn env)\n'
                     '  {- registerLuaFunction "alsoGhost" (f env) -}\n'
                     '  let doc = "registerLuaFunction \\"stringGhost\\""\n'
                     '  registerLuaFunction "quit" (quitFn env)\n'
                     '  Lua.setglobal (Lua.Name "engine")\n', name="Engine"))
    output = expect_finding(root, "engine.ghost",
                            "a verb named only in a Haskell comment")
    expect("1 registrations" in output,
           f"only the executable registration must be counted, got: {output!r}")


def test_a_registration_outside_any_block_is_a_certification_failure() -> None:
    root = build({}, {"scripts/a.lua": "engine.quit()\n"},
                 raw_registrars=raw('  registerLuaFunction "quit" (quitFn env)\n'
                                    '  Lua.newtable\n'
                                    '  Lua.setglobal (Lua.Name "engine")\n'))
    expect_certification_failure(root, "attached to no open table block",
                                 "a registration before any newtable")


def test_a_registration_with_no_setglobal_is_a_certification_failure() -> None:
    """An unassociated registration must fail rather than yield a
    quietly smaller map."""
    root = build({}, {"scripts/a.lua": "engine.quit()\n"},
                 raw_registrars=raw('  Lua.newtable\n'
                                    '  registerLuaFunction "quit" (quitFn env)\n'))
    expect_certification_failure(root, "no\n  Lua.setglobal".replace("\n  ", " "),
                                 "a block that is never installed")


def test_an_empty_block_is_a_certification_failure() -> None:
    root = build({}, {"scripts/a.lua": "engine.quit()\n"},
                 raw_registrars=raw('  Lua.newtable\n'
                                    '  Lua.setglobal (Lua.Name "engine")\n'))
    expect_certification_failure(root, "no registrations",
                                 "a namespace installed with no verbs")


def test_a_computed_namespace_name_is_a_certification_failure() -> None:
    root = build({}, {"scripts/a.lua": "engine.quit()\n"},
                 raw_registrars=raw('  Lua.newtable\n'
                                    '  registerLuaFunction "quit" (quitFn env)\n'
                                    '  Lua.setglobal (Lua.Name nsVar)\n'))
    expect_certification_failure(root, "Lua.Name", "a namespace named by a variable")


def test_a_computed_verb_name_is_a_certification_failure() -> None:
    root = build({}, {"scripts/a.lua": "engine.quit()\n"},
                 raw_registrars=raw('  Lua.newtable\n'
                                    '  registerLuaFunction verbName (quitFn env)\n'
                                    '  Lua.setglobal (Lua.Name "engine")\n'))
    expect_certification_failure(root, "literal verb name",
                                 "a verb named by a variable")


def test_an_unsupported_install_construct_is_a_certification_failure() -> None:
    """Lua.setfield could attach a member this analyzer cannot see, so
    its presence in a registrar must fail rather than shrink the map."""
    root = build({}, {"scripts/a.lua": "engine.quit()\n"},
                 raw_registrars=raw('  Lua.newtable\n'
                                    '  registerLuaFunction "quit" (quitFn env)\n'
                                    '  Lua.pushcfunction sneakyFn\n'
                                    '  Lua.setfield (-2) "sneaky"\n'
                                    '  Lua.setglobal (Lua.Name "engine")\n'))
    expect_certification_failure(root, "unsupported registration construct",
                                 "a registrar using Lua.setfield")


def test_a_partially_unrecognized_registrar_fails_whole() -> None:
    """A nonempty registrar corpus that is only partly analyzed must
    fail: a smaller map would silently stop checking a whole namespace."""
    root = build(
        {"engine": ["quit"]},
        {"scripts/a.lua": "engine.quit()\nworld.show('x')\n"},
        raw_registrars=raw('  Lua.newtable\n'
                           '  registerLuaFunction "show" (showFn env)\n'
                           '  Lua.rawset (-3)\n'
                           '  Lua.setglobal (Lua.Name "world")\n', name="World"))
    expect_certification_failure(root, "unsupported registration construct",
                                 "one unrecognized registrar beside a clean one")


def test_a_registrar_installing_no_namespace_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "engine.quit()\n"},
                 raw_registrars={"Helper": HEADER + "  pure ()\n"})
    expect_certification_failure(root, "installs no namespace",
                                 "a registrar module that installs nothing")


def test_an_unreadable_registrar_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "engine.quit()\n"})
    (root / REGISTRAR_DIR / "Broken.hs").write_bytes(b"\xff\xfe\x00 not utf-8")
    expect_certification_failure(root, "unreadable registrar",
                                 "a registrar that is not valid UTF-8")


def test_an_unreadable_script_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {"scripts/a.lua": "engine.quit()\n"})
    (root / "scripts" / "broken.lua").write_bytes(b"\xff\xfe\x00 not utf-8")
    expect_certification_failure(root, "unreadable script",
                                 "a script that is not valid UTF-8")


# ===========================================================================
# The augmenting form: Register/Debug.hs adds to openlibs' stock table
# ===========================================================================

def test_stock_debug_members_are_provisioned() -> None:
    root = build({"debug": ["recordOutcome"]},
                 {"scripts/a.lua":
                     "debug.recordOutcome{ ok = true }\n"
                     "print(debug.traceback())\n"
                     "print(debug.getinfo(1))\n"},
                 augment={"debug"})
    expect_clean(root, "stock Lua debug members beside the engine's own")


def test_an_invented_debug_member_is_still_a_finding() -> None:
    root = build({"debug": ["recordOutcome"]},
                 {"scripts/a.lua": "print(debug.invented())\n"},
                 augment={"debug"})
    expect_finding(root, "debug.invented", "an invented member of the augmented table")


def test_stock_members_are_not_provisioned_on_a_fresh_table() -> None:
    """The allowance follows the augmenting FORM, not the name: a
    namespace built with Lua.newtable has only what it registers."""
    root = build({"debug": ["recordOutcome"]},
                 {"scripts/a.lua": "print(debug.traceback())\n"})
    expect_finding(root, "debug.traceback",
                   "a stock member on a namespace built fresh")


def test_augmenting_an_unknown_stdlib_table_is_a_certification_failure() -> None:
    root = build({"string": ["myVerb"]}, {"scripts/a.lua": "string.myVerb()\n"},
                 augment={"string"})
    expect_certification_failure(root, "stock\nmembers".replace("\n", " "),
                                 "augmenting a stdlib table with unknown members")


def test_a_mismatched_augmenting_install_is_a_certification_failure() -> None:
    root = build({}, {"scripts/a.lua": "debug.quit()\n"},
                 raw_registrars=raw('  _ <- Lua.getglobal (Lua.Name "debug")\n'
                                    '  Lua.newtable\n'
                                    '  registerLuaFunction "quit" (quitFn env)\n'
                                    '  Lua.setglobal (Lua.Name "engine")\n'))
    expect_certification_failure(root, "opened on", "a block installed under another name")


# ===========================================================================
# Corpus discovery: an empty side is a failure, never a clean run
# ===========================================================================

def test_an_empty_registrar_corpus_is_a_certification_failure() -> None:
    root = build({}, {"scripts/a.lua": "engine.quit()\n"})
    expect_certification_failure(root, "no registrar modules matched",
                                 "a tree with no registrar modules")


def test_an_empty_script_corpus_is_a_certification_failure() -> None:
    root = build({"engine": ["quit"]}, {})
    expect_certification_failure(root, "no Lua scripts matched",
                                 "a tree with no scripts")


def test_nested_script_directories_are_in_scope() -> None:
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua": "engine.quit()\n",
                  "scripts/ui/b.lua": "engine.quit()\n",
                  "scripts/lib/deep/c.lua": "engine.vanished()\n",
                  "scripts/notes.txt": "engine.vanished()\n"})
    output = expect_finding(root, "engine.vanished", "a deeply nested script")
    expect("3 scripts" in output,
           f"every nested .lua and no other file must be scanned, got: {output!r}")


def test_a_non_ascii_script_is_analyzed() -> None:
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua": "-- ° é ✓ comment\nengine.vanished()\n"})
    expect_finding(root, "engine.vanished", "a UTF-8 script with non-ASCII text")


def test_if_elseif_else_branches_scope_independently() -> None:
    """Each branch is its own scope, so a local in one must not shadow
    the global in the next."""
    root = build(
        {"unit": ["getInfo"]},
        {"scripts/a.lua":
            "if a then\n"
            "    local unit = 1\n"
            "    print(unit.x)\n"
            "elseif b then\n"
            "    unit.bad()\n"
            "else\n"
            "    local unit = 2\n"
            "    print(unit.y)\n"
            "end\n"})
    output = expect_finding(root, "unit.bad", "an unshadowed branch between two shadowed ones")
    expect_attributed(output, "scripts/a.lua", 5, "the middle branch")


def test_goto_and_labels_are_analyzed() -> None:
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua": "::top::\nengine.vanished()\ngoto top\n"})
    expect_finding(root, "engine.vanished", "a chunk using goto and a label")


def test_computed_indexing_is_outside_the_grammar() -> None:
    """`engine[k]` names no member statically, so it is not a candidate
    -- and the direct references around it must still be checked."""
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua":
                     "local k = 'quit'\nengine[k]()\nengine.quit()\nengine.vanished()\n"})
    output = expect_finding(root, "engine.vanished", "a file mixing computed and direct access")
    expect_attributed(output, "scripts/a.lua", 4, "the direct reference beside a computed one")


def test_escaped_quotes_do_not_end_a_string() -> None:
    root = build({"engine": ["quit"]},
                 {"scripts/a.lua":
                     "local s = 'it\\'s engine.gone'\n"
                     'local t = "back\\\\"\n'
                     "engine.quit()\n"})
    expect_clean(root, "strings containing escaped quotes and backslashes")


TESTS = [
    test_ui_set_sprite_color_is_a_finding,
    test_the_post_1914_spelling_is_clean,
    test_renaming_a_registered_verb_fails_every_call_site,
    test_adding_a_registration_does_not_fail_on_the_count,
    test_comments_are_not_call_sites,
    test_strings_are_not_call_sites,
    test_the_same_spellings_outside_a_comment_or_string_are_findings,
    test_only_the_first_hop_of_a_dotted_path_is_checked,
    test_the_first_hop_of_a_longer_path_is_still_checked,
    test_a_local_shadowing_a_registered_global_is_not_a_finding,
    test_the_same_members_without_the_local_are_findings,
    test_function_parameters_shadow,
    test_method_definition_binds_self,
    test_numeric_and_generic_for_variables_shadow,
    test_block_locals_shadow_and_the_global_is_restored_on_scope_exit,
    test_shadow_is_restored_after_a_function_body,
    test_a_local_declaration_does_not_shadow_its_own_right_hand_side,
    test_multiple_local_names_all_shadow,
    test_a_local_nested_in_an_outer_locals_rhs_does_not_displace_it,
    test_nested_pending_locals_survive_several_levels,
    test_an_inner_nested_local_still_binds,
    test_an_unshadowed_reference_beside_a_nested_local_is_still_a_finding,
    test_a_bare_local_declaration_shadows,
    test_local_function_is_visible_inside_its_own_body,
    test_an_anonymous_function_local_is_not_visible_inside_its_own_body,
    test_first_class_function_references_are_checked,
    test_table_constructor_call_syntax_is_checked,
    test_string_call_syntax_is_checked,
    test_sibling_references_on_one_line_are_isolated,
    test_sibling_references_in_one_block_are_isolated,
    test_a_method_call_on_a_namespace_is_a_certification_failure,
    test_an_unclosed_block_is_a_certification_failure,
    test_a_stray_end_is_a_certification_failure,
    test_an_end_closing_the_wrong_block_is_a_certification_failure,
    test_an_unterminated_string_is_a_certification_failure,
    test_an_unclosed_bracket_is_a_certification_failure,
    test_an_unclosed_table_constructor_is_a_certification_failure,
    test_a_mismatched_bracket_pair_is_a_certification_failure,
    test_balanced_brackets_of_every_kind_are_clean,
    test_an_unterminated_long_comment_is_a_certification_failure,
    test_an_unreadable_function_header_is_a_certification_failure,
    test_an_unreadable_for_header_is_a_certification_failure,
    test_an_unreadable_local_is_a_certification_failure,
    test_a_repeat_until_block_is_analyzed,
    test_an_end_inside_an_until_expression_does_not_close_the_repeat,
    test_lua_54_local_attributes_are_read,
    test_if_elseif_else_branches_scope_independently,
    test_goto_and_labels_are_analyzed,
    test_computed_indexing_is_outside_the_grammar,
    test_escaped_quotes_do_not_end_a_string,
    test_the_import_list_is_not_a_registration,
    test_haskell_comments_and_literals_are_not_registrations,
    test_a_registration_outside_any_block_is_a_certification_failure,
    test_a_registration_with_no_setglobal_is_a_certification_failure,
    test_an_empty_block_is_a_certification_failure,
    test_a_computed_namespace_name_is_a_certification_failure,
    test_a_computed_verb_name_is_a_certification_failure,
    test_an_unsupported_install_construct_is_a_certification_failure,
    test_a_partially_unrecognized_registrar_fails_whole,
    test_a_registrar_installing_no_namespace_is_a_certification_failure,
    test_an_unreadable_registrar_is_a_certification_failure,
    test_an_unreadable_script_is_a_certification_failure,
    test_stock_debug_members_are_provisioned,
    test_an_invented_debug_member_is_still_a_finding,
    test_stock_members_are_not_provisioned_on_a_fresh_table,
    test_augmenting_an_unknown_stdlib_table_is_a_certification_failure,
    test_a_mismatched_augmenting_install_is_a_certification_failure,
    test_an_empty_registrar_corpus_is_a_certification_failure,
    test_an_empty_script_corpus_is_a_certification_failure,
    test_nested_script_directories_are_in_scope,
    test_a_non_ascii_script_is_analyzed,
]


def main_() -> int:
    for test in TESTS:
        print(f"{test.__name__}:")
        test()
    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s)")
        return 1
    print(f"\nAll {len(TESTS)} tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main_())
