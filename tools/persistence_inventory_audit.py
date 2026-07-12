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
# Tolerates whitespace/newlines around the dot and before the opening
# paren/string (a call split across lines, or `saveMods . register(...)`
# with spaced dots), and either Lua quote style -- `(['"])` captures the
# opening quote and `\1` backreferences it as the closing delimiter, so
# `'name'` and `"name"` both match and neither is truncated by the
# OTHER quote character appearing inside it.
REGISTER_RE = re.compile(r"saveMods\s*\.\s*register\s*\(\s*(['\"])((?:(?!\1).)*)\1")
# Lua long-bracket strings: `[[name]]`, `[=[name]=]`, `[==[name]==]`, ...
# -- the `=` run's LENGTH must match on both sides (Lua's own rule),
# enforced here via backreference `\1` same as the quote form above.
REGISTER_RE_LONGBRACKET = re.compile(
    r"saveMods\s*\.\s*register\s*\(\s*\[(=*)\[(.*?)\]\1\]", re.DOTALL)
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

    Record field declarations in this codebase never contain string
    literals, so a plain `--`-to-end-of-line strip for line comments is
    safe (no risk of treating an in-string `--` as a comment start) --
    unlike the Lua stripper below, which does need to be string-aware.
    """
    out: list[str] = []
    i = 0
    n = len(source)
    depth = 0
    while i < n:
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
    return "\n".join(line[:idx] if (idx := line.find("--")) != -1 else line
                      for line in no_block.splitlines())


def _strip_lua_comments(text: str) -> str:
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
    """
    out: list[str] = []
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if ch in ("'", '"'):
            quote = ch
            out.append(ch)
            i += 1
            while i < n and text[i] != quote:
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
            long_open = LONG_BRACKET_OPEN_RE.match(text, i + 2)
            if long_open:
                close = "]" + long_open.group(1) + "]"
                end = text.find(close, long_open.end())
                i = n if end == -1 else end + len(close)
                continue
            nl = text.find("\n", i)
            i = n if nl == -1 else nl
            continue
        # A bare long-bracket STRING (no leading `--`) must be copied
        # through verbatim -- its content, which may itself contain
        # `--`, is never a comment trigger.
        long_open = LONG_BRACKET_OPEN_RE.match(text, i)
        if long_open:
            close = "]" + long_open.group(1) + "]"
            end = text.find(close, long_open.end())
            span_end = n if end == -1 else end + len(close)
            out.append(text[i:span_end])
            i = span_end
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def _find_matching_brace(text: str, open_index: int) -> int:
    """Index of the `}` that closes the `{` at `open_index` in `text`."""
    depth = 0
    for i in range(open_index, len(text)):
        if text[i] == "{":
            depth += 1
        elif text[i] == "}":
            depth -= 1
            if depth == 0:
                return i
    raise ValueError("no matching closing brace found")


def _split_top_level_fields(block: str) -> list[str]:
    """Split a record's `{ ... }` block into one raw segment per field.

    `block` includes the outer braces. Splits ONLY on commas at nesting
    depth 0 relative to the block's own content (tracking `(`/`[`/`{`
    vs `)`/`]`/`}` generically), so a comma inside a field's own type --
    a tuple `(WorldPageId, WorldState)`, a list-of-tuples, etc. -- is
    never mistaken for a field separator.
    """
    inner = block[1:-1]
    depth = 0
    current: list[str] = []
    segments: list[str] = []
    for ch in inner:
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        if ch == "," and depth == 0:
            segments.append("".join(current))
            current = []
        else:
            current.append(ch)
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

    Scans the whole (comment/string-aware-stripped) file as one string
    rather than line-by-line, so a call whose arguments span multiple
    lines is still found. Covers both Lua quoting forms for the module
    name: `'...'`/`"..."` (REGISTER_RE) and long brackets `[[...]]`/
    `[=[...]=]`/... (REGISTER_RE_LONGBRACKET) -- a single call site uses
    exactly one form, so the two never double-match.
    """
    found: list[tuple[str, str]] = []
    for relpath, text in sorted(scripts_text_by_file.items()):
        cleaned = _strip_lua_comments(text)
        for m in REGISTER_RE.finditer(cleaned):
            found.append((m.group(2), relpath))
        for m in REGISTER_RE_LONGBRACKET.finditer(cleaned):
            found.append((m.group(2), relpath))
    return found


# The five classifications the contract defines (docs/persistence_contract.md
# SS2). A table cell "counts" if it CONTAINS one of these as a substring, so
# parenthetical/bold-wrapped variants ("Persist exactly (container)",
# "**Exclude (new format)**", "Rebuild + Persist (mixed)") all still count --
# but a bare "—"/blank placeholder, which contains none of them, does not.
VALID_CLASSIFICATIONS = (
    "Persist exactly",
    "Persist as identity/reference",
    "Rebuild",
    "Reset to default",
    "Exclude",
)


def _is_valid_classification(cell_text: str) -> bool:
    return any(label in cell_text for label in VALID_CLASSIFICATIONS)


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
