#!/usr/bin/env python3
"""Persistence-inventory audit (issue #756, persistence contract req 10).

Guards docs/persistence_state_inventory.md against silent drift. Per
docs/persistence_contract.md SS2, a "root state owner" is a field on one
of the aggregator records everything else hangs off (EngineEnv,
EngineState, WorldManager, WorldState, and the World.Save.Types
envelope), or a Lua module registered with scripts/lib/save_modules.lua.
Every such field/module must have a classification entry in the
inventory doc: a backtick-quoted name in the first column of one of its
markdown tables, under the SAME `### OwnerName` heading that owns that
field/module (see ROOT_RECORDS/LUA_OWNER_HEADING below) -- classification
is scoped PER OWNER, not per name or per numbered section, so a field on
one record can't be "classified" by sheer coincidence of sharing a name
with an unrelated Lua module or a field on a DIFFERENT record that
happens to share the same `## N.` section (e.g. WorldManager and
WorldState both live under "## 3.", but each gets its own "### " heading
so a name collision between them still can't mask a missing decision).
The row's own Classification cell must also contain one of the five
taxonomy labels (VALID_CLASSIFICATIONS) -- a bare placeholder like "—"
is name-presence without an actual decision, and is rejected too.

This is a static presence/well-formedness check, not a serialization-
correctness proof (it cannot verify a field classified "Persist exactly"
is actually wired into the save/load path) -- see
docs/persistence_contract.md SS7 for what it does and does not
guarantee. Its job is narrower and mechanical: nothing gets ADDED to a
root owner or the Lua save registry without an explicit classification
decision landing alongside it.

Usage:
  python3 tools/persistence_inventory_audit.py
Exit codes: 0 = every root-owner field and Lua module has a valid
classification, 1 = one or more are missing or invalid.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
INVENTORY_PATH = REPO_ROOT / "docs" / "persistence_state_inventory.md"
SCRIPTS_DIR = "scripts"

# The `### ` heading text every Lua registration is classified under
# (docs/persistence_state_inventory.md SS7, "Lua persistence registry").
LUA_OWNER_HEADING = "Lua persistence registry"

# (label, file relative to repo root, regex matching the record's
# `data X = X` line). `label` doubles as the exact `### label` heading
# text the inventory doc must use to classify this record's fields --
# see OWNER_HEADING_RE / parse_classified_names.
ROOT_RECORDS: list[tuple[str, str, str]] = [
    ("EngineEnv", "src/Engine/Core/State.hs", r"^data EngineEnv = EngineEnv\b"),
    ("EngineState", "src/Engine/Core/State.hs", r"^data EngineState = EngineState\b"),
    ("WorldManager", "src/World/State/Types.hs", r"^data WorldManager = WorldManager\b"),
    ("WorldState", "src/World/State/Types.hs", r"^data WorldState = WorldState\b"),
    ("SaveHeader", "src/World/Save/Types.hs", r"^data SaveHeader = SaveHeader\b"),
    ("SaveMetadata", "src/World/Save/Types.hs", r"^data SaveMetadata = SaveMetadata\b"),
    ("WorldPageSave", "src/World/Save/Types.hs", r"^data WorldPageSave = WorldPageSave\b"),
    ("SaveData", "src/World/Save/Types.hs", r"^data SaveData = SaveData\b"),
]

# Matches a field declaration's leading name + arrow within a single
# top-level record segment (see _split_top_level_fields) -- `\s*` here
# already spans newlines, so a field name and its `∷`/`::` written on
# DIFFERENT physical lines still match.
FIELD_NAME_RE = re.compile(r"^\s*([a-zA-Z_][a-zA-Z0-9_']*)\s*(?:∷|::)")
# A segment that is JUST a bare identifier (no arrow) -- part of a
# grouped declaration `name1, name2 :: Type` where several names share
# one trailing type signature. See extract_record_fields.
BARE_NAME_RE = re.compile(r"^\s*([a-zA-Z_][a-zA-Z0-9_']*)\s*$")
# The registry table is called `saveMods` at every real require site
# (`local saveMods = require("scripts.lib.save_modules")`) but is
# `saveModules` inside its OWN definition file -- match either local
# name, OR the table reached directly off its OWN require() call with
# no local binding at all (`require("scripts.lib.save_modules")
# .register(...)`, fully traceable since the module path is a literal
# string identifying this exact registry). Lua also lets a table field
# be reached by BRACKET indexing instead of dot access
# (`saveMods["register"](...)`/`saveMods['register'](...)`) -- a
# perfectly ordinary direct call, not an alias, so it's recognized as
# an alternate spelling of the same access rather than flagged.
_REGISTER_TABLE_REF = (
    r"(?:save(?:Mods|Modules)"
    r"|require\s*\(\s*(?:'scripts\.lib\.save_modules'|\"scripts\.lib\.save_modules\")\s*\))"
)
_REGISTER_ACCESS = (
    _REGISTER_TABLE_REF + r"\s*"
    r"(?:\.\s*register|\[\s*(?:'register'|\"register\")\s*\])"
)
# `require("scripts.lib.save_modules")` itself, standalone -- used to
# find every occurrence of the registry table being fetched, so each
# one can be checked against the sanctioned patterns below.
REQUIRE_SAVE_MODULES_RE = re.compile(
    r"require\s*\(\s*(?:'scripts\.lib\.save_modules'|\"scripts\.lib\.save_modules\")\s*\)")
# Sanctioned continuation #1: the require() result is chained straight
# into `.register`/`["register"]` access (a direct call, or the
# require-chained alias form -- both already handled by REGISTER_RE/
# REGISTER_RE_LONGBRACKET/ALIAS_RE via _REGISTER_ACCESS).
_REQUIRE_CHAINED_ACCESS_RE = re.compile(
    r"require\s*\(\s*(?:'scripts\.lib\.save_modules'|\"scripts\.lib\.save_modules\")\s*\)\s*"
    r"(?:\.\s*register\b|\[\s*(?:'register'|\"register\")\s*\])")
# Sanctioned continuation #2: the require() result is bound to a local
# named EXACTLY `saveMods`/`saveModules`, the codebase's own
# convention -- a later `saveMods.register(...)` is tracked by that
# name already (REGISTER_RE etc. above).
_REQUIRE_SANCTIONED_LOCAL_RE = re.compile(
    r"local\s+(?:saveMods|saveModules)\s*=\s*"
    r"require\s*\(\s*(?:'scripts\.lib\.save_modules'|\"scripts\.lib\.save_modules\")\s*\)")
# Tolerates whitespace/newlines before the opening paren/string (a call
# split across lines, `saveMods . register(...)` with a spaced dot, or
# `saveMods[ "register" ](...)` with spaced brackets), and either Lua
# quote style for the module name -- `(['"])` captures the opening
# quote and `\1` backreferences it as the closing delimiter, so
# `'name'` and `"name"` both match and neither is truncated by the
# OTHER quote character appearing inside it.
REGISTER_RE = re.compile(_REGISTER_ACCESS + r"\s*\(\s*(['\"])((?:(?!\1).)*)\1")
# Lua long-bracket strings: `[[name]]`, `[=[name]=]`, `[==[name]==]`, ...
# -- the `=` run's LENGTH must match on both sides (Lua's own rule),
# enforced here via backreference `\1` same as the quote form above.
REGISTER_RE_LONGBRACKET = re.compile(
    _REGISTER_ACCESS + r"\s*\(\s*\[(=*)\[(.*?)\]\1\]", re.DOTALL)
# A reference to `saveMods.register`/`saveModules.register` (dot OR
# bracket form) NOT immediately followed by a call `(` -- i.e. the
# function is being ALIASED into a variable/table field rather than
# called directly (`local register = saveMods.register; register(...)`
# or `local register = saveMods["register"]`). REGISTER_RE/
# REGISTER_RE_LONGBRACKET only recognize direct calls, so a stored
# alias would silently bypass req 10's audit; rather than trying to
# trace what an alias eventually gets called with (real interpretation
# territory), any such reference is treated as a hard failure on its
# own -- see find_lua_register_aliases.
ALIAS_RE = re.compile(_REGISTER_ACCESS + r"(?!\s*\()")
# A Lua long-bracket opener `[`, zero-or-more `=`, `[` -- shared by the
# comment stripper (both long comments and long strings) and the
# register-call matcher above.
LONG_BRACKET_OPEN_RE = re.compile(r"\[(=*)\[")
BACKTICK_RE = re.compile(r"`([^`]+)`")
OWNER_HEADING_RE = re.compile(r"^###\s+(.+?)\s*$")


def _strip_haskell_comments(source: str) -> str:
    """Blank out Haskell comments, preserving line structure.

    Haskell `{- -}` block comments legally NEST, so a naive
    non-nesting regex can leave a stray `}` behind (from inside an
    outer comment whose first `-}` belongs to an inner one) that then
    desyncs brace-depth tracking downstream. This walks the text once,
    tracking nesting depth explicitly, so arbitrarily nested block
    comments are fully removed regardless of what they contain.

    Literal-aware in BOTH passes: `DataKinds`/`GHC.TypeLits` promoted
    string/char literals (`Proxy "--"`, `Proxy "{-"`, `Proxy '}'`) are
    legal even in a field's own type signature, and a literal's content
    must never be scanned for `{-`/`-}`/`--` markers -- outside a
    literal a `"{-"`-shaped substring is real code, but the equivalent
    substring INSIDE a string literal is just three ordinary characters.
    Skipping literals whole (rather than character-by-character) before
    the block-comment nesting check is what prevents one field's
    literal accidentally "opening" a comment that a LATER field's
    literal then appears to "close", silently swallowing everything
    (including real field declarations) in between.
    """
    out: list[str] = []
    i = 0
    n = len(source)
    depth = 0
    while i < n:
        if depth == 0 and source[i] in ('"', "'"):
            lit_end = _haskell_literal_end(source, i)
            if lit_end is not None:
                out.append(source[i:lit_end])
                i = lit_end
                continue
        if source[i:i + 2] == "{-":
            depth += 1
            i += 2
            continue
        if depth > 0 and source[i:i + 2] == "-}":
            depth -= 1
            i += 2
            continue
        if depth > 0:
            if source[i] == "\n":
                out.append("\n")
            i += 1
            continue
        out.append(source[i])
        i += 1
    no_block = "".join(out)
    return _strip_haskell_line_comments(no_block)


def _strip_haskell_line_comments(text: str) -> str:
    """String-aware `--`-to-end-of-line comment strip (see caller)."""
    out: list[str] = []
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if ch == '"':
            out.append(ch)
            i += 1
            while i < n and text[i] != '"':
                if text[i] == "\\" and i + 1 < n:
                    out.append(text[i])
                    out.append(text[i + 1])
                    i += 2
                    continue
                out.append(text[i])
                i += 1
            if i < n:
                out.append(text[i])
                i += 1
            continue
        if text[i:i + 2] == "--":
            nl = text.find("\n", i)
            i = n if nl == -1 else nl
            continue
        out.append(ch)
        i += 1
    return "".join(out)


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


# A promoted Char literal: `'` + one (possibly escaped) character + `'`.
_CHAR_LITERAL_RE = re.compile(r"'(?:\\.|[^'\\])'")


def _haskell_literal_end(text: str, i: int) -> int | None:
    """If a Haskell string or char literal starts at text[i], return the
    index just past its closing delimiter; otherwise None.

    `DataKinds`/`GHC.TypeLits` promoted literals make BOTH string
    (`Proxy "}"`) and char (`Proxy '}'`) literals legal even in a
    field's own type, and either can contain a `{`/`}`/`,` that must
    not be mistaken for a structural character.

    A `'` is only treated as a literal opener when it's NOT a trailing
    "prime" on the identifier just consumed (`foo'`, `bar''` are
    ordinary Haskell identifiers) -- i.e. when the previous character
    isn't itself an identifier character. A `'` that doesn't close
    within one (possibly escaped) character is left alone too -- that's
    a DataKinds promoted-constructor tick (`'Just`, `'[Int]`), which
    contains no characters this scan needs to skip over.
    """
    ch = text[i]
    if ch == '"':
        j = i + 1
        n = len(text)
        while j < n and text[j] != '"':
            j += 2 if text[j] == "\\" and j + 1 < n else 1
        return min(j + 1, n)
    if ch == "'":
        if i > 0 and (text[i - 1].isalnum() or text[i - 1] in "_'"):
            return None
        m = _CHAR_LITERAL_RE.match(text, i)
        return m.end() if m else None
    return None


def _find_matching_brace(text: str, open_index: int) -> int:
    """Index of the `}` that closes the `{` at `open_index` in `text`.

    String/char-literal-aware (see _haskell_literal_end) -- a promoted
    literal's content is skipped over whole, never scanned
    character-by-character, so it can't be mistaken for a structural
    brace.
    """
    depth = 0
    i = open_index
    n = len(text)
    while i < n:
        ch = text[i]
        if ch in ('"', "'"):
            end = _haskell_literal_end(text, i)
            if end is not None:
                i = end
                continue
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return i
        i += 1
    raise ValueError("no matching closing brace found")


def _split_top_level_fields(block: str) -> list[str]:
    """Split a record's `{ ... }` block into one raw segment per field.

    `block` includes the outer braces. Splits ONLY on commas at nesting
    depth 0 relative to the block's own content (tracking `(`/`[`/`{`
    vs `)`/`]`/`}` generically), so a comma inside a field's own type --
    a tuple `(WorldPageId, WorldState)`, a list-of-tuples, etc. -- is
    never mistaken for a field separator. String/char-literal-aware for
    the same reason as _find_matching_brace: a literal's structural-
    looking characters (braces, brackets, commas) are never counted.
    """
    inner = block[1:-1]
    depth = 0
    current: list[str] = []
    segments: list[str] = []
    i = 0
    n = len(inner)
    while i < n:
        ch = inner[i]
        if ch in ('"', "'"):
            end = _haskell_literal_end(inner, i)
            if end is not None:
                current.append(inner[i:end])
                i = end
                continue
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        if ch == "," and depth == 0:
            segments.append("".join(current))
            current = []
        else:
            current.append(ch)
        i += 1
    segments.append("".join(current))
    return segments


def extract_record_fields(source: str, record_start_pattern: str) -> list[str]:
    """Field names declared in one Haskell record's brace block.

    Comments are stripped first so a haddock comment's prose can never
    desync the brace-depth tracker that finds the block's end. Field
    names are extracted from top-level comma-delimited segments (see
    _split_top_level_fields), not per PHYSICAL LINE, so a field whose
    name and `∷`/`::` are written on different lines -- legal Haskell,
    e.g. `, someField\n    ∷ Int` -- is still found.

    Also handles GROUPED field declarations, where several names share
    one trailing type signature: `{ name1, name2 ∷ Int }`. Each comma
    still produces its own top-level segment, but only the LAST one
    carries the arrow; a run of bare-identifier segments immediately
    before an arrow-bearing one all belong to that same declaration.
    """
    cleaned = _strip_haskell_comments(source)
    pat = re.compile(record_start_pattern, re.MULTILINE)
    m = pat.search(cleaned)
    if m is None:
        raise ValueError(f"record start not found: {record_start_pattern!r}")
    brace_start = cleaned.find("{", m.end())
    if brace_start == -1:
        raise ValueError(
            f"no opening brace found after record start: {record_start_pattern!r}")
    brace_end = _find_matching_brace(cleaned, brace_start)
    block = cleaned[brace_start:brace_end + 1]
    fields: list[str] = []
    pending: list[str] = []
    for segment in _split_top_level_fields(block):
        fm = FIELD_NAME_RE.match(segment)
        if fm:
            fields.extend(pending)
            fields.append(fm.group(1))
            pending = []
            continue
        bm = BARE_NAME_RE.match(segment)
        if bm:
            pending.append(bm.group(1))
        else:
            pending = []
    return fields


def extract_lua_registered_modules(
        scripts_text_by_file: dict[str, str]) -> list[tuple[str, str]]:
    """(module name, file) for every saveMods.register("name", ...) call site.

    Scans the whole (comment-stripped, string-PRESERVING) file as one
    string rather than line-by-line, so a call whose arguments span
    multiple lines is still found. Covers both Lua quoting forms for
    the module name: `'...'`/`"..."` (REGISTER_RE) and long brackets
    `[[...]]`/`[=[...]=]`/... (REGISTER_RE_LONGBRACKET) -- a single
    call site uses exactly one form, so the two never double-match.

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
        for m in REGISTER_RE.finditer(cleaned):
            if not any(start <= m.start() < end for start, end in spans):
                found.append((m.group(2), relpath))
        for m in REGISTER_RE_LONGBRACKET.finditer(cleaned):
            if not any(start <= m.start() < end for start, end in spans):
                found.append((m.group(2), relpath))
    return found


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


def find_lua_register_aliases(scripts_text_by_file: dict[str, str]) -> list[str]:
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


# `X = saveMods`/`X = saveModules` (with or without a leading `local`)
# where X is a DIFFERENT name -- re-aliasing the already-canonical
# table into a second variable, the same violation class as an
# untracked require() binding (see find_untracked_registry_aliases),
# just one hop later. `local` is OPTIONAL: Lua's `=` is unambiguously
# assignment (unlike C-style languages, Lua has no `==`-vs-`=`
# confusion inside an `if`, since assignment is a statement, never an
# expression), so a bare `registry = saveMods` re-assigning an
# already-declared (or even implicitly global) variable is just as
# live a bypass as the `local` form.
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
_BARE_REGISTRY_ALIAS_RE = re.compile(
    r"(?:local\s+)?(?!saveMods\b|saveModules\b)\w+\s*=\s*save(?:Mods|Modules)\b"
    r"(?!\s*[.\[])")


def find_untracked_registry_aliases(scripts_text_by_file: dict[str, str]) -> list[str]:
    """Files where the registry table escapes to an untracked local
    name -- either `require("scripts.lib.save_modules")`'s result
    directly (`local registry = require("scripts.lib.save_modules")`),
    or a SECOND-level alias of the already-canonical name
    (`local registry = saveMods`). Either way, a later
    `registry.register("untracked", ...)` is a real, live registration
    this audit's fixed-receiver-name matchers cannot trace.

    `find_lua_register_aliases`/REGISTER_RE only ever look for the
    FIXED receiver spellings `saveMods`/`saveModules`/a direct
    `require(...)` chain -- binding the registry table to an ARBITRARY
    local name is a data-flow problem no amount of regex matching on
    fixed names can trace (Lua allows any identifier, and allows
    aliasing an alias). Rather than trying to enumerate every possible
    name or chase arbitrary aliasing depth, this flags the ESCAPE
    itself: every `require("scripts.lib.save_modules")` occurrence, and
    every bare `saveMods`/`saveModules` occurrence, must be either
    (a) chained straight into `.register`/`["register"]` access (a
    direct call, or the alias-of-the-function form -- both already
    covered elsewhere), or (b) itself assigned to a local named EXACTLY
    `saveMods`/`saveModules`, the codebase's own convention. Anything
    else -- bound to another name, passed as an argument, stored in a
    table under an arbitrary key -- means the registry table is now
    reachable only through something this audit cannot trace, so it's a
    hard failure on its own. A THIRD level of aliasing (re-aliasing the
    SECOND local yet again) is a known, accepted limitation of this
    static, non-interpreting approach.
    """
    offenders: list[str] = []
    for relpath, text in sorted(scripts_text_by_file.items()):
        cleaned = _strip_lua_comments(text)
        string_spans = _string_literal_spans(cleaned)
        sanctioned_local_spans = [
            (m.start(), m.end()) for m in _REQUIRE_SANCTIONED_LOCAL_RE.finditer(cleaned)]
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
            for m in _BARE_REGISTRY_ALIAS_RE.finditer(cleaned):
                if not any(start <= m.start() < end for start, end in string_spans):
                    untracked = True
                    break
        if untracked:
            offenders.append(relpath)
    return offenders


# The five classifications the contract defines (docs/persistence_contract.md
# SS2). The contract requires EXACTLY ONE per item, so a cell counts only
# if its CORE text (after stripping bold markup and a trailing parenthetical
# aside -- see _classification_core) EQUALS one of these exactly. That
# accepts decorated variants ("Persist exactly (container)",
# "**Exclude (new format)**") while rejecting both a bare "--"/blank
# placeholder (core matches none of them) and a compound value like
# "Rebuild + Persist (mixed)" (core is "Rebuild + Persist", which matches
# none of them exactly either -- a plain substring test would have missed
# this, since "Persist exactly" isn't literally present).
VALID_CLASSIFICATIONS = (
    "Persist exactly",
    "Persist as identity/reference",
    "Rebuild",
    "Reset to default",
    "Exclude",
)
_TRAILING_PAREN_RE = re.compile(r"\s*\([^)]*\)\s*$")


def _classification_core(cell_text: str) -> str:
    """Strip bold markup and one trailing parenthetical aside."""
    text = cell_text.strip().replace("**", "")
    text = _TRAILING_PAREN_RE.sub("", text)
    return text.strip()


def _is_valid_classification(cell_text: str) -> bool:
    return _classification_core(cell_text) in VALID_CLASSIFICATIONS


_NO_CLASSIFICATION_COLUMN = -1


def parse_classified_names(inventory_text: str) -> dict[str, dict[str, str]]:
    """Every backtick-quoted first-column name and its classification
    cell's raw text, keyed by the nearest preceding `### OwnerName`
    heading: `{owner: {name: classification_text}}`.

    Classification is scoped PER OWNER, not globally and not merely per
    `## N.` section: several distinct owners can share one numbered
    section (WorldManager/WorldState both live under "## 3.", all four
    save-envelope records under "## 4.") so a name is only "classified"
    for the specific `###`-headed owner it's documented under -- a
    different owner (a sibling record under the same section, or the
    Lua registry) happening to share that name can't mask a missing
    decision.

    The "Classification" column's INDEX varies by table (EngineEnv/
    EngineState put it 3rd, after Field/Scope; WorldManager/WorldState/
    the save-envelope records put it 2nd; the Lua registry puts it 4th),
    so each table's own header row is parsed to find it, rather than
    assuming a fixed position.
    """
    by_owner: dict[str, dict[str, str]] = {}
    current_owner: str | None = None
    classification_idx: int | None = None
    for line in inventory_text.splitlines():
        heading = OWNER_HEADING_RE.match(line)
        if heading:
            current_owner = heading.group(1)
            classification_idx = None
            continue
        if not line.startswith("|"):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if classification_idx is None:
            classification_idx = (cells.index("Classification")
                                   if "Classification" in cells
                                   else _NO_CLASSIFICATION_COLUMN)
            continue
        if classification_idx == _NO_CLASSIFICATION_COLUMN or current_owner is None:
            continue
        names = BACKTICK_RE.findall(cells[0]) if cells else []
        if not names:
            continue  # e.g. the `|---|---|` separator row
        classification_text = (cells[classification_idx]
                                if classification_idx < len(cells) else "")
        for bt in names:
            by_owner.setdefault(current_owner, {})[bt] = classification_text
    return by_owner


def audit(record_sources: dict[str, str], scripts_text_by_file: dict[str, str],
          inventory_text: str,
          root_records: list[tuple[str, str, str]] | None = None) -> list[str]:
    """Pure audit core. Returns a list of human-readable violations."""
    if root_records is None:
        root_records = ROOT_RECORDS
    classified = parse_classified_names(inventory_text)
    violations: list[str] = []

    for label, relpath, pattern in root_records:
        source = record_sources.get(relpath)
        if source is None:
            violations.append(f"{label}: source not provided for {relpath}")
            continue
        try:
            fields = extract_record_fields(source, pattern)
        except ValueError as exc:
            violations.append(f"{label}: {exc}")
            continue
        if not fields:
            violations.append(
                f"{label}: no fields extracted from {relpath} -- the parser "
                f"may be out of sync with this record's layout")
            continue
        classified_here = classified.get(label, {})
        for field in fields:
            if field not in classified_here:
                violations.append(
                    f"{label}.{field} ({relpath}) has no classification under "
                    f"the '### {label}' heading in {INVENTORY_PATH.name}")
            elif not _is_valid_classification(classified_here[field]):
                violations.append(
                    f"{label}.{field} ({relpath})'s classification "
                    f"{classified_here[field]!r} under the '### {label}' "
                    f"heading in {INVENTORY_PATH.name} is not one of "
                    f"{VALID_CLASSIFICATIONS}")

    classified_lua = classified.get(LUA_OWNER_HEADING, {})
    for name, relpath in extract_lua_registered_modules(scripts_text_by_file):
        if name not in classified_lua:
            violations.append(
                f'Lua save module "{name}" (registered in {relpath}) has no '
                f"classification under the '### {LUA_OWNER_HEADING}' heading "
                f"in {INVENTORY_PATH.name}")
        elif not _is_valid_classification(classified_lua[name]):
            violations.append(
                f'Lua save module "{name}" (registered in {relpath})\'s '
                f"classification {classified_lua[name]!r} under the "
                f"'### {LUA_OWNER_HEADING}' heading in {INVENTORY_PATH.name} "
                f"is not one of {VALID_CLASSIFICATIONS}")

    for relpath in find_lua_register_aliases(scripts_text_by_file):
        violations.append(
            f"{relpath} references saveMods.register/saveModules.register "
            f"without calling it directly (e.g. assigning it to a local "
            f"or table field) -- the audit can only trace direct calls; "
            f"call saveMods.register(...) directly instead of aliasing it")

    for relpath in find_untracked_registry_aliases(scripts_text_by_file):
        violations.append(
            f"{relpath} aliases the save-modules registry table "
            f'(via require("scripts.lib.save_modules") or the '
            f"saveMods/saveModules name) into something other than a "
            f"direct .register()/[\"register\"] access or a local named "
            f"exactly saveMods/saveModules -- the audit cannot trace a "
            f".register(...) call made through an arbitrarily-named "
            f"alias; use one of the two sanctioned patterns instead")

    return violations


def _load_repo_state() -> tuple[dict[str, str], dict[str, str], str]:
    record_sources: dict[str, str] = {}
    for _, relpath, _ in ROOT_RECORDS:
        if relpath not in record_sources:
            record_sources[relpath] = (REPO_ROOT / relpath).read_text(encoding="utf-8")
    scripts_text_by_file: dict[str, str] = {}
    for path in (REPO_ROOT / SCRIPTS_DIR).rglob("*.lua"):
        rel = str(path.relative_to(REPO_ROOT))
        scripts_text_by_file[rel] = path.read_text(encoding="utf-8")
    inventory_text = INVENTORY_PATH.read_text(encoding="utf-8")
    return record_sources, scripts_text_by_file, inventory_text


def main() -> int:
    record_sources, scripts_text_by_file, inventory_text = _load_repo_state()
    violations = audit(record_sources, scripts_text_by_file, inventory_text)
    if violations:
        print(f"{len(violations)} persistence-inventory violation(s):")
        for v in violations:
            print(f"  - {v}")
        print(f"\nAdd a classification row for each item above to "
              f"{INVENTORY_PATH.relative_to(REPO_ROOT)} (see "
              f"docs/persistence_contract.md for the taxonomy).")
        return 1

    total_fields = sum(
        len(extract_record_fields(record_sources[relpath], pattern))
        for _, relpath, pattern in ROOT_RECORDS)
    total_lua = len(extract_lua_registered_modules(scripts_text_by_file))
    print(f"persistence-inventory audit: {total_fields} root-owner fields + "
          f"{total_lua} Lua save module(s) all classified")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
