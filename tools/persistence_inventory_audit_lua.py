#!/usr/bin/env python3
"""Lua persistence scanning for the persistence-inventory audit (issue
#2124): the ONE owner of Lua comment/string handling, save-module
registration discovery, registry-alias detection, and typed Lua
reference-kind discovery.

A pure leaf. Every function takes a `{relpath: source text}` mapping
(or one file's text) and returns extracted facts; nothing here reads a
repository file, parses an inventory classification, or imports another
owner. The façade, tools/persistence_inventory_audit.py, loads the
script tree once and hands the same immutable mapping to each scanner.

This is deliberately NOT a general Lua parser, and the split that
created it changed neither what it recognizes nor what it refuses. The
regex fragments below accumulated 24 review-hardening rounds (single-,
double- and long-bracket strings; leveled long brackets; parenthesized
and paren-free calls; direct `require` chains; `package.loaded` and
`package["loaded"]`; dot, quoted-bracket and long-bracket `register`
access; arbitrarily parenthesized canonical receivers; global, local,
table-field and table-constructor aliases; registry-definition and
reload-guard exclusions; strings, comments and prose-shaped false
positives; delegated `references` helpers), and every one of those
spellings is pinned by tools/test_persistence_inventory_audit.py. The
accepted limitations of a static, non-interpreting scan are recorded in
docs/persistence_contract.md SS7 item 6 / SS8 -- anything a fixed-name
matcher cannot trace is reported as a failure in its own right rather
than followed.
"""
from __future__ import annotations

import re
from collections.abc import Mapping

# `package.loaded`'s own field access -- Lua lets `loaded` be reached
# either by dot access (`package.loaded`, the common idiom, used
# everywhere in this codebase) or by BRACKET indexing the exact same
# field (`package["loaded"]`/`package['loaded']`) -- the same dot-vs-
# bracket duality every OTHER field access in this file already
# tolerates (`saveMods.register` vs `saveMods["register"]`). Shared by
# every "is this the package.loaded cache slot" check below, so the two
# forms can't drift apart the way _REGISTER_TABLE_REF/_TABLE_CONSTRUCTOR
# fixes did when the parens tolerance wasn't originally shared (round
# 20). The dot form keeps a trailing `\b` (guards against a coincidental
# longer field name like a hypothetical `package.loadedFoo` being
# mistaken for this one); the bracket form doesn't need one -- the
# quote delimiters around the literal `'loaded'`/`"loaded"` key already
# make it exact, with no prefix-matching ambiguity possible.
_PACKAGE_LOADED_ACCESS_RE_FRAGMENT = (
    r"package\s*(?:\.\s*loaded\b|\[\s*(?:'loaded'|\"loaded\")\s*\])"
)
# The module-path string `"scripts.lib.save_modules"` itself, in every
# Lua string-literal form this scanner recognizes elsewhere (single-
# quoted, double-quoted, or a long-bracket string) -- `require()` and
# `package.loaded[...]`'s bracket index both take this as a plain
# string ARGUMENT, so both accept a long-bracket string
# (`require([[scripts.lib.save_modules]])`,
# `package.loaded[ [[scripts.lib.save_modules]] ]`) exactly as validly
# as either quote form; only the quoted forms were originally
# recognized. Ungrouped `=*` on each side, same reasoning as the
# `.register`-access-key fix (round 22): a new capturing group here
# would renumber every group after it in the several patterns that
# already depend on positional `\1`/`\2` backreferences elsewhere for
# unrelated matching (the module NAME argument, the `.register`-key
# `=`-run), silently breaking them; an actual open/close `=`-run
# mismatch is a Lua syntax error regardless, so this can't be
# exploited. Shared by every occurrence of this literal string
# throughout the file, closing require() and package.loaded[...]
# together in one place rather than leaving one fixed and one not (the
# exact shape of gap rounds 20/21 both found).
_SAVE_MODULES_PATH_STRING_RE_FRAGMENT = (
    r"(?:'scripts\.lib\.save_modules'|\"scripts\.lib\.save_modules\""
    r"|\[=*\[scripts\.lib\.save_modules\]=*\])"
)
# `require("scripts.lib.save_modules")` (or any recognized string form
# of its argument), as a complete call expression -- shared by every
# place that needs to match the CALL, not just the bare path string.
# Lua's function-call sugar lets a call's SOLE argument be a bare
# string (or table) literal with NO parens at all --
# `require "scripts.lib.save_modules"` is exactly as valid, and exactly
# as live, a call as the parenthesized form (this is a real, common Lua
# idiom for `require` specifically). The paren-free branch needs no
# extra "complete argument" check the way REGISTER_RE's parenthesized
# module-NAME argument does: sugar syntax syntactically permits ONLY a
# single string/table literal as the entire argument list, so a
# computed/concatenated name (`require "a" .. "b"`) isn't even
# expressible this way -- there's nothing after the string to mistake
# for a continuation of the SAME argument, only a chained access on the
# call's RESULT (`.register(...)`), which the callers of this fragment
# already handle. Every consumer of this fragment (REQUIRE_SAVE_MODULES_RE,
# the chained-access/sanctioned-local escape checks) inherits paren-free
# recognition automatically since they're all built from this one
# shared fragment (round 23) rather than duplicating it.
_REQUIRE_SAVE_MODULES_CALL_RE_FRAGMENT = (
    r"require\s*(?:\(\s*" + _SAVE_MODULES_PATH_STRING_RE_FRAGMENT + r"\s*\)"
    r"|" + _SAVE_MODULES_PATH_STRING_RE_FRAGMENT + r")"
)
# `[...]` bracket-indexing the module path string -- shared by every
# place that indexes `package.loaded` (or, historically, anything else)
# with this literal path, so a long-bracket-string index isn't
# recognized in some call sites and not others.
_SAVE_MODULES_PATH_INDEX_RE_FRAGMENT = (
    r"\[\s*" + _SAVE_MODULES_PATH_STRING_RE_FRAGMENT + r"\s*\]"
)
# The registry table is called `saveMods` at every real require site
# (`local saveMods = require("scripts.lib.save_modules")`) but is
# `saveModules` inside its OWN definition file -- match either local
# name, the table reached directly off its OWN require() call with
# no local binding at all (`require("scripts.lib.save_modules")
# .register(...)`, fully traceable since the module path is a literal
# string identifying this exact registry), OR the table reached
# directly off `package.loaded["scripts.lib.save_modules"]` (or its
# bracket-indexed sibling `package["loaded"]["scripts.lib.save_modules"]`,
# per _PACKAGE_LOADED_ACCESS_RE_FRAGMENT above) -- Lua's `require()`
# itself reads/writes exactly this cache slot, so this is a THIRD
# spelling of the identical singleton table, not a different one;
# `package.loaded[...].register(...)` is exactly as direct and
# traceable as the require()-chained form. Lua also lets a table field
# be reached by BRACKET indexing instead of dot access
# (`saveMods["register"](...)`/`saveMods['register'](...)`) -- a
# perfectly ordinary direct call, not an alias, so it's recognized as
# an alternate spelling of the same access rather than flagged. Lua
# also allows the bare name to be wrapped in ANY number of redundant
# parens -- `(saveMods).register(...)`, `((saveMods)).register(...)`,
# arbitrarily deep, are all exactly as direct a call as the bare form,
# just with cosmetic grouping (the require()/package.loaded forms are
# already parenthesized-looking via their own call/index syntax and
# aren't wrapped like this in practice, so parens support is scoped to
# the bare-name alternative). Unlike an arbitrary-depth ALIAS chain
# (each hop introduces a genuinely NEW identifier this audit can't
# enumerate in advance -- real interpretation territory, the accepted
# limitation described in docs/persistence_contract.md SS7 item 6),
# redundant parens around
# one FIXED, already-known token are a regular pattern: `\(+`/`\)+`
# (one-or-more, not "optional") cover any depth in a single bounded
# match, so this is fully general, not "one more level" -- no future
# depth of parens can bypass it. This is its OWN alternative (requiring
# AT LEAST one paren on each side) rather than making the parens
# optional around the plain bare-name alternative: an UNCONDITIONALLY
# optional paren (`\(*`/`\)*`, tried first and reverted) let its
# accompanying `\s*` swallow ordinary PRECEDING whitespace/indentation
# even with NO paren present at all (the regex engine finds the
# leftmost successful match, and whitespace before an unparenthesized
# `saveMods` satisfies `\(*\s*` with zero parens matched), silently
# shifting every real match's start position earlier by however much
# leading whitespace precedes it -- desyncing the position-based
# function-definition-site exclusion in find_lua_register_dynamic_names
# that surfaced this. Requiring `\(+` (never satisfiable with zero
# parens) keeps the parenthesized alternative fully separate from the
# whitespace-free bare-name one, so this failure mode can't recur.
_REGISTER_TABLE_REF = (
    r"(?:\(+\s*save(?:Mods|Modules)\s*\)+"
    r"|save(?:Mods|Modules)"
    r"|" + _REQUIRE_SAVE_MODULES_CALL_RE_FRAGMENT
    + r"|" + _PACKAGE_LOADED_ACCESS_RE_FRAGMENT
    + r"\s*" + _SAVE_MODULES_PATH_INDEX_RE_FRAGMENT + r")"
)
# The `.register` access itself, in every form this scanner
# recognizes: dot access, bracket-indexed with either quote style, or
# bracket-indexed with a Lua LONG-BRACKET string
# (`[ [[register]] ]`/`[ [=[register]=] ]`/...) -- the same
# long-bracket string form REGISTER_RE_LONGBRACKET already tolerates
# for the MODULE NAME argument, just not yet applied to the ACCESS KEY
# itself. DELIBERATELY UNGROUPED (`=*` on each side, not a capturing
# `(=*)` backreferenced for equality) -- this fragment gets embedded
# into REGISTER_RE/REGISTER_RE_LONGBRACKET/ALIAS_RE, which each already
# use their OWN positional `\1` backreference downstream (for the
# MODULE NAME's quote/bracket matching); inserting a NEW capturing
# group here would renumber every group after it, silently breaking
# those unrelated backreferences (confirmed by testing group counts
# before settling on this fix). Not requiring the open/close `=`-run
# lengths to match is a deliberate, harmless over-acceptance: an
# actual mismatch (`[=[register]==]`) is a Lua SYNTAX ERROR, so no real
# or adversarial script could ever contain one for this to matter. A
# trailing `\b` on the dot form (absent before this fix) also closes a
# latent imprecision: without it, `saveMods.registerFoo` was matched as
# if `.register` were a complete access with unrelated trailing text,
# not as the (correctly unrelated) longer identifier it actually is.
# Shared by every "is this a `.register` access" check in this file, so
# a future new access spelling can't drift between call sites the way
# `package.loaded`'s dot-vs-bracket spellings did before they shared a
# fragment (round 21).
_REGISTER_ACCESS_SUFFIX_RE_FRAGMENT = (
    r"(?:\.\s*register\b"
    r"|\[\s*(?:'register'|\"register\"|\[=*\[register\]=*\])\s*\])"
)
_REGISTER_ACCESS = (
    _REGISTER_TABLE_REF + r"\s*" + _REGISTER_ACCESS_SUFFIX_RE_FRAGMENT
)
# `require("scripts.lib.save_modules")` itself, standalone -- used to
# find every occurrence of the registry table being fetched, so each
# one can be checked against the sanctioned patterns below.
REQUIRE_SAVE_MODULES_RE = re.compile(_REQUIRE_SAVE_MODULES_CALL_RE_FRAGMENT)
# Sanctioned continuation #1: the require() result is chained straight
# into `.register`/`["register"]` access (a direct call, or the
# require-chained alias form -- both already handled by REGISTER_RE/
# REGISTER_RE_LONGBRACKET/ALIAS_RE via _REGISTER_ACCESS).
_REQUIRE_CHAINED_ACCESS_RE = re.compile(
    _REQUIRE_SAVE_MODULES_CALL_RE_FRAGMENT + r"\s*"
    + _REGISTER_ACCESS_SUFFIX_RE_FRAGMENT)
# Sanctioned continuation #2: the require() result is bound to a local
# named EXACTLY `saveMods`/`saveModules`, the codebase's own
# convention -- a later `saveMods.register(...)` is tracked by that
# name already (REGISTER_RE etc. above).
_REQUIRE_SANCTIONED_LOCAL_RE = re.compile(
    r"local\s+(?:saveMods|saveModules)\s*=\s*"
    + _REQUIRE_SAVE_MODULES_CALL_RE_FRAGMENT)
# `package.loaded["scripts.lib.save_modules"]` itself, standalone --
# the FETCH-side sibling of REQUIRE_SAVE_MODULES_RE, since it's the
# exact same singleton table under a second legitimate spelling. Every
# occurrence must be checked against its OWN three sanctioned
# continuations below, the same way every require() occurrence is --
# otherwise `local registry = package.loaded["scripts.lib.save_modules"];
# registry.register(...)` re-aliases the table through a spelling the
# original require()-only escape check never looked at, invisibly to
# the audit (the register-access recognizer added alongside the direct-
# call support only catches an IMMEDIATE `.register`/`["register"]`
# chain, not a table reference stored in a local first).
_PACKAGE_LOADED_SAVE_MODULES_RE = re.compile(
    _PACKAGE_LOADED_ACCESS_RE_FRAGMENT
    + r"\s*" + _SAVE_MODULES_PATH_INDEX_RE_FRAGMENT)
# Sanctioned continuation #1: chained straight into `.register`/
# `["register"]` access -- a direct call, or the package.loaded-chained
# alias form (both already handled by REGISTER_RE/REGISTER_RE_LONGBRACKET/
# ALIAS_RE via _REGISTER_ACCESS, which now includes this receiver).
_PACKAGE_LOADED_CHAINED_ACCESS_RE = re.compile(
    _PACKAGE_LOADED_ACCESS_RE_FRAGMENT
    + r"\s*" + _SAVE_MODULES_PATH_INDEX_RE_FRAGMENT + r"\s*"
    + _REGISTER_ACCESS_SUFFIX_RE_FRAGMENT)
# Sanctioned continuation #2: bound to a local named EXACTLY
# `saveMods`/`saveModules` -- the real registry's OWN definition-file
# idiom, `local saveModules = package.loaded[...] or {}` (the trailing
# `or {}` fallback, for the first-ever require(), is tolerated but not
# required).
_PACKAGE_LOADED_SANCTIONED_LOCAL_RE = re.compile(
    r"local\s+(?:saveMods|saveModules)\s*=\s*"
    + _PACKAGE_LOADED_ACCESS_RE_FRAGMENT
    + r"\s*" + _SAVE_MODULES_PATH_INDEX_RE_FRAGMENT
    + r"(?:\s*or\s*\{\s*\})?")
# Sanctioned continuation #3: it's the ASSIGNMENT TARGET, not a fetch at
# all -- `package.loaded["scripts.lib.save_modules"] = saveModules` is
# the require()-caching WRITE half of the same real-file idiom (the
# read half is continuation #2 above); a `=` immediately after (not
# `==`, a comparison) means this occurrence is never read as a value
# here, so it cannot itself be the source of a new alias.
_PACKAGE_LOADED_ASSIGNMENT_TARGET_RE = re.compile(
    _PACKAGE_LOADED_ACCESS_RE_FRAGMENT
    + r"\s*" + _SAVE_MODULES_PATH_INDEX_RE_FRAGMENT
    + r"\s*=(?!=)")
# Tolerates whitespace/newlines before the opening paren/string (a call
# split across lines, `saveMods . register(...)` with a spaced dot, or
# `saveMods[ "register" ](...)` with spaced brackets), and either Lua
# quote style for the module name -- `(['"])` captures the opening
# quote and `\1` backreferences it as the closing delimiter, so
# `'name'` and `"name"` both match and neither is truncated by the
# OTHER quote character appearing inside it. The trailing `(?=\s*[,)])`
# requires the literal to be the COMPLETE first argument -- immediately
# followed by the arg-separating comma or the call's closing paren, not
# concatenated with further expression text (`"unit_ai" .. "_untracked"`).
# Without this, the regex captures just the literal PREFIX of whatever
# the real (dynamically-built) module name turns out to be at runtime,
# silently misreporting a call that actually registers a DIFFERENT,
# unclassified name as if it re-registered the already-classified
# literal alone -- see find_lua_register_dynamic_names for the
# complementary hard-fail on a call that fails this completeness check.
REGISTER_RE = re.compile(
    _REGISTER_ACCESS + r"\s*\(\s*(['\"])((?:(?!\1).)*)\1(?=\s*[,)])")
# Lua long-bracket strings: `[[name]]`, `[=[name]=]`, `[==[name]==]`, ...
# -- the `=` run's LENGTH must match on both sides (Lua's own rule),
# enforced here via backreference `\1` same as the quote form above.
# Same completeness lookahead as REGISTER_RE.
REGISTER_RE_LONGBRACKET = re.compile(
    _REGISTER_ACCESS + r"\s*\(\s*\[(=*)\[(.*?)\]\1\](?=\s*[,)])", re.DOTALL)
# The PAREN-FREE sibling of REGISTER_RE/REGISTER_RE_LONGBRACKET -- Lua's
# function-call sugar lets a call's SOLE argument be a bare string
# literal with NO parens at all (`saveMods.register "modname"` is
# exactly as valid, and exactly as live, a call as
# `saveMods.register("modname")` -- see _REQUIRE_SAVE_MODULES_CALL_RE_FRAGMENT
# for the identical Lua feature applied to `require`). No completeness
# lookahead is needed here the way REGISTER_RE's parenthesized form
# needs one: sugar syntax syntactically permits ONLY a single string/
# table literal as the ENTIRE argument list, so a computed/concatenated
# name isn't even expressible this way -- there's no "more argument
# text" position for a concatenation to occupy. Kept as fully SEPARATE
# compiled patterns (not an optional `\(?` folded into REGISTER_RE
# itself) specifically to avoid shifting REGISTER_RE's own group
# numbering, which extract_lua_registered_modules depends on via
# positional `m.group(2)`; two clean, self-contained patterns is safer
# than one pattern juggling a conditional completeness check.
REGISTER_RE_PARENFREE = re.compile(
    _REGISTER_ACCESS + r"\s*(['\"])((?:(?!\1).)*)\1")
REGISTER_RE_PARENFREE_LONGBRACKET = re.compile(
    _REGISTER_ACCESS + r"\s*\[(=*)\[(.*?)\]\1\]", re.DOTALL)
# Any direct call at all (register access immediately followed by an
# opening paren), regardless of what the argument looks like -- used to
# find calls whose module-name argument ISN'T a complete literal (see
# find_lua_register_dynamic_names), by finding every direct call and
# then checking which ones REGISTER_RE/REGISTER_RE_LONGBRACKET do NOT
# also match at the exact same position.
_REGISTER_CALL_RE = re.compile(_REGISTER_ACCESS + r"\s*\(")
# `function saveModules.register(name, serializeFn, deserializeFn)` --
# the real registry's OWN function DEFINITION (a Lua parameter list, not
# a call) is syntactically indistinguishable from a call to
# _REGISTER_CALL_RE (both are "register access immediately followed by
# `(`"), and its bare-identifier parameter names never satisfy
# REGISTER_RE/REGISTER_RE_LONGBRACKET's literal-argument check, so
# without this exclusion the definition site itself would be
# misreported as a "dynamic name" call. Captures the register-access
# START position (group 1) so find_lua_register_dynamic_names can skip
# any _REGISTER_CALL_RE match that starts there.
_REGISTER_DEFINITION_RE = re.compile(r"function\s+(" + _REGISTER_ACCESS + r")")
# A reference to `saveMods.register`/`saveModules.register` (dot OR
# bracket form) NOT immediately followed by a call -- either the
# parenthesized form `(` or a paren-free-sugar argument start (a quote
# character, or a long-bracket opener `[[`/`[=`) -- i.e. the function is
# being ALIASED into a variable/table field rather than called directly
# (`local register = saveMods.register; register(...)` or
# `local register = saveMods["register"]`). Without the paren-free
# branches here, a genuine paren-free CALL (`saveMods.register
# "modname"`) would be misread as a stored alias reference instead of
# recognized as the live registration REGISTER_RE_PARENFREE/
# REGISTER_RE_PARENFREE_LONGBRACKET now extract. REGISTER_RE/
# REGISTER_RE_LONGBRACKET/REGISTER_RE_PARENFREE/
# REGISTER_RE_PARENFREE_LONGBRACKET only recognize direct calls, so a
# stored alias would silently bypass req 10's audit; rather than trying
# to trace what an alias eventually gets called with (real
# interpretation territory), any such reference is treated as a hard
# failure on its own -- see find_lua_register_aliases.
ALIAS_RE = re.compile(_REGISTER_ACCESS + r"(?!\s*(?:\(|['\"]|\[(?:\[|=)))")
# A Lua long-bracket opener `[`, zero-or-more `=`, `[` -- shared by the
# comment stripper (both long comments and long strings) and the
# register-call matcher above.
LONG_BRACKET_OPEN_RE = re.compile(r"\[(=*)\[")


def _strip_lua_comments(text: str, *, keep_strings: bool = True) -> str:
    """Blank out Lua comments, preserving line structure.

    String-aware in the FULL sense: quoted (`'`/`"`, with `\\`-escapes
    honored) AND long-bracket (`[[...]]`, `[=[...]=]`, ...) string
    literals are recognized and their content is never treated as a
    comment trigger -- a `--` embedded in EITHER string form must not
    truncate the line, or a real `saveMods.register(...)` call
    following it on the same line would be silently discarded. Lua long
    COMMENTS (`--[[...]]`/`--[=[...]=]`/...) are likewise recognized
    with their `=`-run level matched on both delimiters, and (per Lua's
    own rule) don't nest.

    By default string CONTENT is kept verbatim (`keep_strings=True`) --
    callers that parse call arguments (which live inside those strings)
    need it. Pass `keep_strings=False` for a code-SHAPE check, where a
    string literal's text must not be mistaken for real code (e.g. an
    error message that happens to mention "saveModules.register" is
    not a reference to the function).
    """
    out: list[str] = []
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if ch in ("'", '"'):
            quote = ch
            if keep_strings:
                out.append(ch)
            i += 1
            while i < n and text[i] != quote:
                if text[i] == "\\" and i + 1 < n:
                    if keep_strings:
                        out.append(text[i])
                        out.append(text[i + 1])
                    i += 2
                    continue
                if keep_strings:
                    out.append(text[i])
                i += 1
            if i < n:
                if keep_strings:
                    out.append(text[i])
                i += 1
            continue
        if text[i:i + 2] == "--":
            long_open = LONG_BRACKET_OPEN_RE.match(text, i + 2)
            if long_open:
                close = "]" + long_open.group(1) + "]"
                end = text.find(close, long_open.end())
                i = n if end == -1 else end + len(close)
                continue
            nl = text.find("\n", i)
            i = n if nl == -1 else nl
            continue
        # A bare long-bracket STRING (no leading `--`) must not be
        # treated as code -- its content, which may itself contain
        # `--`, is never a comment trigger either way.
        long_open = LONG_BRACKET_OPEN_RE.match(text, i)
        if long_open:
            close = "]" + long_open.group(1) + "]"
            end = text.find(close, long_open.end())
            span_end = n if end == -1 else end + len(close)
            if keep_strings:
                out.append(text[i:span_end])
            i = span_end
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def _string_literal_spans(text: str) -> list[tuple[int, int]]:
    """[start, end) ranges of Lua string literals (delimiters included)
    in comment-stripped Lua text: quoted ('...'/"...") AND long-bracket
    ([[...]]/[=[...]=]/...). `text` has already had comments stripped
    (see callers), so any remaining long-bracket-shaped span here IS a
    string, never a long comment -- those are already gone.

    A quoted OR long-bracket string can contain prose that happens to
    mention "saveModules.register" (an error message, a doc string
    literal like `[[saveMods.register]]`); a match whose start falls in
    one of these spans is not a real reference to the function.
    """
    spans: list[tuple[int, int]] = []
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if ch in ("'", '"'):
            quote = ch
            start = i
            i += 1
            while i < n and text[i] != quote:
                i += 2 if text[i] == "\\" and i + 1 < n else 1
            if i < n:
                i += 1
            spans.append((start, i))
            continue
        long_open = LONG_BRACKET_OPEN_RE.match(text, i)
        if long_open:
            start = i
            close = "]" + long_open.group(1) + "]"
            end = text.find(close, long_open.end())
            i = n if end == -1 else end + len(close)
            spans.append((start, i))
            continue
        i += 1
    return spans


def extract_lua_registered_modules(
        scripts_text_by_file: Mapping[str, str]) -> list[tuple[str, str]]:
    """(module name, file) for every saveMods.register("name", ...) call site.

    Scans the whole (comment-stripped, string-PRESERVING) file as one
    string rather than line-by-line, so a call whose arguments span
    multiple lines is still found. Covers both Lua quoting forms for
    the module name: `'...'`/`"..."` (REGISTER_RE) and long brackets
    `[[...]]`/`[=[...]=]`/... (REGISTER_RE_LONGBRACKET), each with a
    PAREN-FREE sugar-call sibling (REGISTER_RE_PARENFREE/
    REGISTER_RE_PARENFREE_LONGBRACKET, for `saveMods.register "name"`
    with no parens at all) -- a single call site uses exactly one form,
    so none of the four ever double-match the same call.

    Filters out any match whose START falls inside an (unrelated)
    string-literal span -- otherwise a call-SHAPED mention inside prose
    (a doc string like `[[example: saveMods.register("x", nil, nil)]]`)
    reads as a real, live registration and produces a false CI failure
    for a module that never actually gets registered. A real call's OWN
    argument literal is never itself "unrelated": the match starts at
    the receiver (`saveMods`/`require(...)`), before that literal
    begins, so this never rejects a genuine call.
    """
    found: list[tuple[str, str]] = []
    for relpath, text in sorted(scripts_text_by_file.items()):
        cleaned = _strip_lua_comments(text)
        spans = _string_literal_spans(cleaned)
        for pattern in (REGISTER_RE, REGISTER_RE_LONGBRACKET,
                        REGISTER_RE_PARENFREE, REGISTER_RE_PARENFREE_LONGBRACKET):
            for m in pattern.finditer(cleaned):
                if not any(start <= m.start() < end for start, end in spans):
                    found.append((m.group(2), relpath))
    return found


def find_lua_register_aliases(scripts_text_by_file: Mapping[str, str]) -> list[str]:
    """Files that reference saveMods.register/saveModules.register
    WITHOUT calling it directly (e.g. `local r = saveMods.register`, or
    the bracket form `local r = saveMods["register"]`).

    extract_lua_registered_modules can only trace DIRECT calls; an
    alias stored in a variable or table field and invoked later would
    silently escape req 10's audit. Rather than attempting to trace
    what an alias eventually gets called with, this enforces a
    direct-call-only registration convention: any such reference is
    itself reported, regardless of whether it's ever actually called.

    Runs against string-PRESERVING stripped text (comments removed,
    string content intact) -- unlike a blanket keep_strings=False,
    which would also destroy the legitimate `["register"]` bracket
    form's own quoted key -- and then discards any match whose START
    falls inside a string literal's span, so a string literal's TEXT
    (e.g. `error("saveModules.register: name must be a string")`, the
    real registry's own validation message) is never mistaken for a
    reference to the function.
    """
    offenders: list[str] = []
    for relpath, text in sorted(scripts_text_by_file.items()):
        cleaned = _strip_lua_comments(text)
        spans = _string_literal_spans(cleaned)
        if any(not any(start <= m.start() < end for start, end in spans)
               for m in ALIAS_RE.finditer(cleaned)):
            offenders.append(relpath)
    return offenders


def find_lua_register_dynamic_names(scripts_text_by_file: Mapping[str, str]) -> list[str]:
    """Files with a direct saveMods.register(...)-shaped call whose
    module-name argument is NOT a complete, standalone string/
    long-bracket literal -- e.g. `saveMods.register("unit_ai" ..
    "_untracked", ...)`, where `saveModules.register` (the real
    function -- see scripts/lib/save_modules.lua) accepts and stores
    whatever string the argument expression evaluates to at runtime,
    but the literal PREFIX visible to static analysis ("unit_ai") is
    already a classified name.

    extract_lua_registered_modules (via REGISTER_RE/REGISTER_RE_LONGBRACKET)
    only recognizes a call whose entire first argument is one literal;
    tracing an arbitrary Lua expression (concatenation, a variable,
    string.format(...), ...) to the string it evaluates to at runtime is
    real interpretation territory, not a tractable regex improvement --
    the same reasoning that makes an alias itself the failure elsewhere
    in this module (find_lua_register_aliases,
    find_untracked_registry_aliases). So rather than silently ignoring
    a call whose argument extraction fails, or worse, silently matching
    just the literal PREFIX and treating that as the whole registration,
    this flags the CALL itself as a failure: every direct call
    (_REGISTER_CALL_RE) whose argument doesn't ALSO satisfy REGISTER_RE/
    REGISTER_RE_LONGBRACKET's completeness check is reported. The
    codebase's real registration convention is a plain literal name
    (verified against all 4 real call sites), so this never fires on
    genuine code. Explicitly skips the registry's own `function
    saveModules.register(name, ...)` DEFINITION site -- a Lua parameter
    list is syntactically indistinguishable from a call to
    _REGISTER_CALL_RE, but it's a declaration, not a registration.
    """
    offenders: list[str] = []
    for relpath, text in sorted(scripts_text_by_file.items()):
        cleaned = _strip_lua_comments(text)
        spans = _string_literal_spans(cleaned)
        definition_starts = {
            m.start(1) for m in _REGISTER_DEFINITION_RE.finditer(cleaned)}
        for m in _REGISTER_CALL_RE.finditer(cleaned):
            if any(start <= m.start() < end for start, end in spans):
                continue  # inside a string literal, not real code
            if m.start() in definition_starts:
                continue  # the registry's own function DEFINITION site
            if REGISTER_RE.match(cleaned, m.start()) or \
                    REGISTER_RE_LONGBRACKET.match(cleaned, m.start()):
                continue  # a complete, well-formed literal argument
            offenders.append(relpath)
            break
    return offenders


# `TARGET = saveMods`/`TARGET = saveModules` (with or without a leading
# `local`) where TARGET is anything other than the bare canonical name
# -- re-aliasing the already-canonical table into a second variable OR
# TABLE FIELD, the same violation class as an untracked require()
# binding (see find_untracked_registry_aliases), just one hop later.
# `local` is OPTIONAL: Lua's `=` is unambiguously assignment (unlike
# C-style languages, Lua has no `==`-vs-`=` confusion inside an `if`,
# since assignment is a statement, never an expression), so a bare
# `registry = saveMods` re-assigning an already-declared (or even
# implicitly global) variable is just as live a bypass as the `local`
# form.
#
# TARGET covers Lua's full (finite, well-defined) assignment-target
# grammar: a bare name, OR a name followed by one or more `.field`/
# `[key]` accesses (`holder.registry`, `holder["registry"]`, chained
# combinations) -- storing the registry table under a TABLE KEY, not
# just a plain variable, is exactly as untraceable as a bare re-alias
# once something later does `holder["registry"].register(...)`.
#
# The RHS must be the BARE name with NOTHING chained after it (no
# `.field`/`[key]` at all -- not just `.register`/`["register"]`).
# `\b` alone is satisfied by a following `.`, so without this the
# regex would misread `saveModules.registry = saveModules.registry`
# (the real registry's own reload-safety idiom, assigning its
# `registry` SUB-TABLE to itself) as "bare saveModules aliased into
# `registry`" -- `registry` there is a field access on the FIRST
# `saveModules.registry`, not a plain variable, and the RHS is that
# same sub-table, not the module table itself. Any `.register`/
# `["register"]` access specifically is a different, already-covered
# case (find_lua_register_aliases via ALIAS_RE) and is correctly
# excluded here the same way any other field access is.
_ASSIGNMENT_TARGET_RE_FRAGMENT = (
    r"\w+(?:\s*\.\s*\w+|\s*\[\s*(?:'[^']*'|\"[^\"]*\"|\w+)\s*\])*"
)
# `package.loaded[modname] = <module table>` is Lua's own universal
# require()-caching idiom (used by every Lua module in this codebase,
# including save_modules.lua's own definition, per its header comment:
# "Singleton via package.loaded so script reloads + multiple require()s
# share the same registry") -- it is not a bypass attempt, it's how
# require() itself expects a module to register its cache entry, and
# nobody would realistically call `.register` through
# `package.loaded["scripts.lib.save_modules"].register(...)` instead of
# the local binding sitting right there. Excluded explicitly rather
# than letting the general table-key case flag it.
# A negative LOOKBEHIND for `.` or a word character is required so a
# match can only start at a genuine, top-of-chain identifier boundary.
# `\b` alone (tried first) is not enough: it blocks a match starting
# mid-WORD (e.g. "ackage", a suffix of "package") but NOT one starting
# right after a "." that continues a longer dotted chain -- since
# ".loaded[...]" independently satisfies the target grammar as if
# "loaded" were its own bare identifier, `finditer` happily starts a
# match there instead, sidestepping the `package.loaded` exclusion
# just as effectively as starting mid-word did. Requiring "nothing
# word-like AND no dot" immediately before the match start closes both
# routes at once.
# The canonical name as a "complete value" -- optionally wrapped in ANY
# depth of redundant parens (same reasoning as _REGISTER_TABLE_REF's
# parenthesized receiver: a FIXED, already-known token wrapped in
# `\(+`/`\)+` is a regular pattern coverable for any depth in one shot,
# unlike arbitrary-depth aliasing), with nothing chained after it at
# all. Shared by every "is this RHS/value truly the bare canonical
# name" check below -- `{ [1] = (saveMods) }` needs the exact same
# parens tolerance a plain `registry = (saveMods)` assignment would,
# and duplicating the fragment per call site is exactly how the two
# diverged before (round 20's finding: the table-constructor check
# alone got the parens fix, the assignment-statement check didn't).
_BARE_CANONICAL_VALUE_RE_FRAGMENT = (
    r"\(*\s*save(?:Mods|Modules)\b\s*\)*(?!\s*[.\[])"
)
_BARE_REGISTRY_ALIAS_RE = re.compile(
    r"(?<![.\w])(?:local\s+)?(?!saveMods\b|saveModules\b)"
    r"(?!" + _PACKAGE_LOADED_ACCESS_RE_FRAGMENT + r")"
    + _ASSIGNMENT_TARGET_RE_FRAGMENT
    + r"\s*=\s*" + _BARE_CANONICAL_VALUE_RE_FRAGMENT)
# The canonical name hidden as a TABLE CONSTRUCTOR field's value --
# `{ [1] = saveMods }` (explicit key), `{ saveMods }` (positional, an
# implicit integer key), or `{ registry = saveMods }` (named key) --
# rather than the RHS of a subsequent `=` statement. Structurally
# different from _BARE_REGISTRY_ALIAS_RE's grammar (a `{`/`,`-delimited
# entry inside a table literal, not a standalone assignment statement),
# so it needs its own pattern: a value position starts right after `{`
# or `,` (optionally preceded by a `[expr] =` or `name =` key), and the
# canonical name must be the COMPLETE entry -- bare (or parenthesized,
# per the shared fragment above), with nothing chained after it,
# immediately followed by the next `,` or the constructor's closing
# `}` -- so `{ saveMods = require(...) }` (the canonical name used as a
# KEY whose value is something else entirely) is correctly NOT matched.
_TABLE_CONSTRUCTOR_ALIAS_RE = re.compile(
    r"[{,]\s*(?:\[[^\]]*\]\s*=\s*|[A-Za-z_]\w*\s*=\s*)?"
    + _BARE_CANONICAL_VALUE_RE_FRAGMENT + r"\s*[,}]")


def find_untracked_registry_aliases(scripts_text_by_file: Mapping[str, str]) -> list[str]:
    """Files where the registry table escapes to an untracked local
    name -- either `require("scripts.lib.save_modules")`'s result
    directly (`local registry = require("scripts.lib.save_modules")`),
    the same via `package.loaded["scripts.lib.save_modules"]` (the
    identical singleton table under its second legitimate spelling), or
    a SECOND-level alias of the already-canonical name
    (`local registry = saveMods`). Either way, a later
    `registry.register("untracked", ...)` is a real, live registration
    this audit's fixed-receiver-name matchers cannot trace.

    `find_lua_register_aliases`/REGISTER_RE only ever look for the
    FIXED receiver spellings `saveMods`/`saveModules`/a direct
    `require(...)` or `package.loaded[...]` chain -- binding the
    registry table to an ARBITRARY local name (or hiding it as a table
    CONSTRUCTOR field's value, `{ [1] = saveMods }`/`{ saveMods }`/
    `{ registry = saveMods }`) is a data-flow problem no amount of
    regex matching on fixed names can trace (Lua allows any identifier,
    and allows aliasing an alias). Rather than trying to enumerate
    every possible name or chase arbitrary aliasing depth, this flags
    the ESCAPE itself: every `require("scripts.lib.save_modules")`
    occurrence, every `package.loaded["scripts.lib.save_modules"]`
    occurrence, and every bare `saveMods`/`saveModules` occurrence, must
    be either (a) chained straight into `.register`/`["register"]`
    access (a direct call, or the alias-of-the-function form -- both
    already covered elsewhere), (b) itself assigned to a local named
    EXACTLY `saveMods`/`saveModules`, the codebase's own convention, or
    (c) for `package.loaded[...]` specifically, itself the ASSIGNMENT
    TARGET of the require()-caching write idiom
    (`package.loaded[...] = saveModules`) rather than a value being
    read. Anything else -- bound to another name, passed as an
    argument, stored in a table under an arbitrary key OR as a table
    constructor's field value -- means the registry table is now
    reachable only through something this audit cannot trace, so it's a
    hard failure on its own. A THIRD level of aliasing (re-aliasing the
    SECOND local yet again), or hiding the canonical name behind
    OTHER Lua binding constructs this audit doesn't specifically
    pattern-match (multiple assignment, a function-call argument, a
    for-loop variable, a closure, a coroutine, a metatable proxy), is a
    known, accepted limitation of this static, non-interpreting
    approach -- see docs/persistence_contract.md SS7 item 6 / SS8.
    """
    offenders: list[str] = []
    for relpath, text in sorted(scripts_text_by_file.items()):
        cleaned = _strip_lua_comments(text)
        string_spans = _string_literal_spans(cleaned)
        sanctioned_local_spans = [
            (m.start(), m.end()) for m in _REQUIRE_SANCTIONED_LOCAL_RE.finditer(cleaned)]
        package_loaded_sanctioned_local_spans = [
            (m.start(), m.end())
            for m in _PACKAGE_LOADED_SANCTIONED_LOCAL_RE.finditer(cleaned)]
        untracked = False
        for m in REQUIRE_SAVE_MODULES_RE.finditer(cleaned):
            if any(start <= m.start() < end for start, end in string_spans):
                continue  # inside a string literal, not real code
            if _REQUIRE_CHAINED_ACCESS_RE.match(cleaned, m.start()):
                continue  # chained into .register/["register"] access
            if any(start <= m.start() < end for start, end in sanctioned_local_spans):
                continue  # local saveMods/saveModules = require(...)
            untracked = True
            break
        if not untracked:
            for m in _PACKAGE_LOADED_SAVE_MODULES_RE.finditer(cleaned):
                if any(start <= m.start() < end for start, end in string_spans):
                    continue  # inside a string literal, not real code
                if _PACKAGE_LOADED_CHAINED_ACCESS_RE.match(cleaned, m.start()):
                    continue  # chained into .register/["register"] access
                if _PACKAGE_LOADED_ASSIGNMENT_TARGET_RE.match(cleaned, m.start()):
                    continue  # package.loaded[...] = saveModules cache write
                if any(start <= m.start() < end
                       for start, end in package_loaded_sanctioned_local_spans):
                    continue  # local saveMods/saveModules = package.loaded(...)
                untracked = True
                break
        if not untracked:
            for m in _BARE_REGISTRY_ALIAS_RE.finditer(cleaned):
                if not any(start <= m.start() < end for start, end in string_spans):
                    untracked = True
                    break
        if not untracked:
            for m in _TABLE_CONSTRUCTOR_ALIAS_RE.finditer(cleaned):
                if not any(start <= m.start() < end for start, end in string_spans):
                    untracked = True
                    break
        if untracked:
            offenders.append(relpath)
    return offenders


# Lua reference kinds (issue #764): the controlled `kind = "..."`
# vocabulary a save component's `references()` hook reports
# (scripts/unit_ai_save_refs.lua / scripts/building_spawn.lua). Scoped to
# any file that registers a `references = <something>` spec field at
# all -- either an inline `references = function(data) ... end` or (the
# form both real registrations actually use) a named function reference
# `references = unitAiReferences` -- not to the hook's own function
# BODY specifically, since reliably finding a Lua function's matching
# `end` (distinct from every nested `if`/`for`/`while`'s own `end`)
# needs real block-structure parsing this regex-based audit doesn't
# otherwise do anywhere. A same-file `kind = "..."` used for an
# unrelated purpose would only ever cause over-inclusion (one more
# string requiring a documented vocabulary entry), never a silently-
# missed reference kind -- the same fail-safe direction this audit
# already takes elsewhere (see the module docstring's "anything this
# mapping can't classify" philosophy in tools/ci_probes.py, which this
# mirrors).
# Two established call shapes report a kind string (both real
# registrations use one or the other): a direct table-constructor
# literal (`{ kind = "building", id = bid }`, building_spawn.lua) and a
# same-file `addRef(kind, id)`-style helper invoked with a literal
# first argument (`addRef("unit", uid)`, unit_ai_save_refs.lua -- the
# helper's OWN table constructor is `{ kind = kind, id = id }`, a
# variable, not a literal, so only the CALL SITE's literal argument is
# textually findable).
LUA_REFERENCE_KIND_RES = (
    re.compile(r'kind\s*=\s*"([a-z_]+)"'),
    re.compile(r'addRef(?:List)?\(\s*"([a-z_]+)"'),
)
_LUA_REFERENCES_SPEC_FIELD_RE = re.compile(r"\breferences\s*=")

# Round-5 review (issue #764): a REGISTRATION site can delegate its
# `references = ` spec field to an imported helper module
# (`references = refsMod.references`, unit_ai_save.lua) rather than
# defining the hook inline or as a same-file named function
# (`references = buildingSpawnReferences`, building_spawn.lua -- the
# latter already worked under the original per-file gate, since the
# kind literals AND the `references =` text share one file). The
# former case only worked before by ACCIDENT: unit_ai_save_refs.lua
# happens to independently satisfy the gate itself, via its own
# internal `M.references = unitAiReferences` re-export line -- true
# today, but not a structural guarantee (a differently-named re-export,
# or a helper module split that never re-exports under that literal
# name at all, would silently stop being scanned). These two regexes
# instead trace the REAL relationship: which `require()`d module a
# registration's `references = <var>.<field>` delegation actually
# points at.
_LUA_REQUIRE_LOCAL_RE = re.compile(
    r"""\blocal\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*require\(\s*"""
    r"""["']([\w.]+)["']\s*\)""")
_LUA_REFERENCES_DELEGATE_RE = re.compile(
    r"\breferences\s*=\s*([A-Za-z_][A-Za-z0-9_]*)\.[A-Za-z_][A-Za-z0-9_]*\b")


def _delegated_reference_module_paths(cleaned_text: str) -> set[str]:
    """relpaths of every `require()`d module a `references = <var>.<field>`
    delegation in `cleaned_text` resolves to -- e.g. `references =
    refsMod.references` alongside `local refsMod =
    require("scripts.unit_ai_save_refs")` resolves to
    "scripts/unit_ai_save_refs.lua". A delegation whose variable was
    never `require()`d in the same file (or whose require target isn't
    a real, present script) resolves to nothing -- this only ever
    WIDENS which files get scanned for kind literals, it can't narrow
    the existing per-file gate below.
    """
    delegated_vars = {
        m.group(1) for m in _LUA_REFERENCES_DELEGATE_RE.finditer(cleaned_text)
    }
    if not delegated_vars:
        return set()
    paths: set[str] = set()
    for m in _LUA_REQUIRE_LOCAL_RE.finditer(cleaned_text):
        varname, module = m.group(1), m.group(2)
        if varname in delegated_vars:
            paths.add(module.replace(".", "/") + ".lua")
    return paths


def _required_module_paths(cleaned_text: str) -> set[str]:
    """relpaths of every module `cleaned_text` `require()`s into a local.

    Only ever applied to a DELEGATE helper module (see
    `find_lua_reference_kinds`), never to registration sites at large.
    """
    return {
        m.group(2).replace(".", "/") + ".lua"
        for m in _LUA_REQUIRE_LOCAL_RE.finditer(cleaned_text)
    }


def find_lua_reference_kinds(
        scripts_text_by_file: Mapping[str, str]) -> list[tuple[str, str]]:
    """(kind, relpath) for every distinct reference-kind literal in a
    file that registers a `references()` hook, OR in a helper module a
    registration site `require()`s and delegates its `references = `
    spec field to, OR in a module THAT helper itself `require()`s (see
    LUA_REFERENCE_KIND_RES for the two kind-literal call shapes
    recognised, and `_delegated_reference_module_paths` for the
    delegation-following that closes the round-5 review gap).

    The last hop closes the same class of gap one level further out
    (issue #1589): a delegate helper can itself be split, moving the
    kind literals into a module it requires -- as
    scripts/unit_ai_save_refs.lua did when its reference SCHEMA moved to
    scripts/unit_ai_ref_schema.lua. Without following it, four of the
    nine documented kinds silently stopped being scanned and the audit
    kept passing. It is deliberately taken only from a DELEGATE module,
    not from every registration site: a registration site requires the
    whole world (claim registries, the codec, save_modules), while a
    delegate helper is by construction the reference layer itself. Even
    so the direction stays the audit's usual fail-safe one -- an extra
    scanned file can only demand one more documented vocabulary entry,
    never hide a kind."""
    cleaned_by_file = {
        relpath: _strip_lua_comments(text)
        for relpath, text in scripts_text_by_file.items()
    }
    scannable: set[str] = set()
    delegates: set[str] = set()
    for relpath, cleaned in cleaned_by_file.items():
        if _LUA_REFERENCES_SPEC_FIELD_RE.search(cleaned):
            scannable.add(relpath)
        for delegated in _delegated_reference_module_paths(cleaned):
            if delegated in scripts_text_by_file:
                scannable.add(delegated)
                delegates.add(delegated)
    for delegate in sorted(delegates):
        for required in _required_module_paths(cleaned_by_file[delegate]):
            if required in scripts_text_by_file:
                scannable.add(required)

    out: list[tuple[str, str]] = []
    for relpath in sorted(scannable):
        cleaned = cleaned_by_file[relpath]
        seen: set[str] = set()
        for pattern in LUA_REFERENCE_KIND_RES:
            for m in pattern.finditer(cleaned):
                kind = m.group(1)
                if kind not in seen:
                    seen.add(kind)
                    out.append((kind, relpath))
    return out
