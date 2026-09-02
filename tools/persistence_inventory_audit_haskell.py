#!/usr/bin/env python3
"""Haskell source extraction for the persistence-inventory audit (issue
#2124): the ONE owner of comment/literal-aware Haskell record parsing
and typed persistent-reference field discovery.

A pure leaf. It receives source TEXT and returns extracted facts (or
raises `ValueError` with a specific message when the requested record
cannot be found); it never reads a repository file, never parses an
inventory classification, and imports nothing from the other owners --
so tools/persistence_inventory_audit.py (the façade),
tools/persistence_inventory_audit_policy.py, and the EngineEnv
capability audit (tools/engine_env_capability_common.py) can all share
`extract_record_fields` as the single canonical record parser without a
cycle. The façade re-exports `extract_record_fields` under its own name
as a compatibility contract for those consumers.

What it handles, each pinned by tools/test_persistence_inventory_audit.py:
nested `{- -}` block comments; `{`/`}`/`--` markers inside promoted
string/char literals; primed identifiers that are NOT char literals; a
field name and its `∷`/`::` on different lines; grouped declarations
sharing one type; unrelated records in the same file; and a clear
failure when the requested record is absent.
"""
from __future__ import annotations

import re
from collections.abc import Mapping

# Matches a field declaration's leading name + arrow within a single
# top-level record segment (see _split_top_level_fields) -- `\s*` here
# already spans newlines, so a field name and its `∷`/`::` written on
# DIFFERENT physical lines still match.
FIELD_NAME_RE = re.compile(r"^\s*([a-zA-Z_][a-zA-Z0-9_']*)\s*(?:∷|::)")
# A segment that is JUST a bare identifier (no arrow) -- part of a
# grouped declaration `name1, name2 :: Type` where several names share
# one trailing type signature. See extract_record_fields.
BARE_NAME_RE = re.compile(r"^\s*([a-zA-Z_][a-zA-Z0-9_']*)\s*$")


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


# Typed persistent reference fields (issue #764, save-overhaul C3):
# a DTO field typed with a "World.Save.Reference" wrapper
# (SamePageRef/CrossPageRef) declares a durable cross-component
# reference's scope at the TYPE level (requirement 2). Requirement 15's
# persistence-reference audit: a newly introduced one without a
# documented target kind/scope/validation/migration decision fails
# in the façade's audit(), exactly the way an unclassified root-owner
# field or Lua save module already does -- reusing
# persistence_inventory_audit_policy.parse_classified_names' existing
# generic "### Owner" + "Classification" table parser (no new doc-
# parsing machinery needed; see docs/persistence_state_inventory.md's
# "### Typed persistent references" heading).
#
# Matches the wrapper ANYWHERE on the field's declaration line after the
# `∷`/`::` separator (round-3 review, issue #764) -- not just when it's
# the outermost type constructor. A field like
# `psSim ∷ !(HM.HashMap (SamePageRef UnitId) UnitSimStateDTO)` types a
# reference nested inside a HashMap KEY, not as the field's own top-level
# type, which the original front-anchored pattern couldn't see. This
# codebase's one-field-per-line style (already relied on by
# `_extract_fields_from_brace_block`) keeps the match scoped to a single
# field's own declaration rather than spilling into a sibling field.
#
# The leading punctuation class accepts `{` as well as `,` (issue #1246):
# a record's FIRST field opens with `{ name :: ...` while every later one
# opens with `, name :: ...`, so a comma-only prefix silently skipped
# whichever typed reference happened to be declared first in its DTO.
# Every reference field that existed before #1246 was a later one, which
# is why the gap went unnoticed until `qtdInstance` (QueuedTransferDTO's
# item-instance reference) landed at the head of its record.
REFERENCE_FIELD_RE = re.compile(
    r"^\s*[,{]?\s*([a-zA-Z_][a-zA-Z0-9_']*)\s*(?:∷|::)[^\n]*"
    r"\b(?:SamePageRef|CrossPageRef)\b",
    re.MULTILINE,
)


def find_typed_reference_fields(
        component_sources: Mapping[str, str]) -> list[tuple[str, str]]:
    """(fieldName, relpath) for every DTO field in `component_sources`
    typed as a SamePageRef/CrossPageRef wrapper. Scans each whole file
    (comment-stripped) rather than parsing individual record
    boundaries -- these wrapper types are only ever used on a durable
    reference field, so a bare textual match is unambiguous without
    needing to know which specific record a field belongs to.
    """
    out: list[tuple[str, str]] = []
    for relpath, source in sorted(component_sources.items()):
        text = _strip_haskell_comments(source)
        for m in REFERENCE_FIELD_RE.finditer(text):
            out.append((m.group(1), relpath))
    return out
