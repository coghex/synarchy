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

This is a static presence check, not a serialization-correctness proof
(it cannot verify a field classified "Persist exactly" is actually wired
into the save/load path) -- see docs/persistence_contract.md SS7 for what
it does and does not guarantee. Its job is narrower and mechanical:
nothing gets ADDED to a root owner or the Lua save registry without an
explicit classification decision landing alongside it.

Usage:
  python3 tools/persistence_inventory_audit.py
Exit codes: 0 = every root-owner field and Lua module is classified,
1 = one or more are missing from the inventory.
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

# Matches a record-field declaration line: leading `{` or `,`, the field
# name, then the (UnicodeSyntax or plain) type-signature arrow.
FIELD_LINE_RE = re.compile(r"^\s*[{,]\s*([a-zA-Z_][a-zA-Z0-9_']*)\s*(?:∷|::)")
# Tolerates whitespace/newlines around the dot and before the opening
# paren/string (a call split across lines, or `saveMods . register(...)`
# with spaced dots), and either Lua quote style -- `(['"])` captures the
# opening quote and `\1` backreferences it as the closing delimiter, so
# `'name'` and `"name"` both match and neither is truncated by the
# OTHER quote character appearing inside it.
REGISTER_RE = re.compile(r"saveMods\s*\.\s*register\s*\(\s*(['\"])((?:(?!\1).)*)\1")
ROW_FIRST_CELL_RE = re.compile(r"^\|([^|]*)\|")
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

    String-aware: a `--` or `--[[` inside a quoted Lua string literal
    (single or double quoted, with `\\`-escapes honored) is part of the
    string, not a comment marker, and must not truncate the line --
    otherwise a real `saveMods.register(...)` call following a string
    containing `--` on the same line would be silently discarded along
    with it. Lua `--[[ ... ]]` block comments do not nest (unlike
    Haskell's `{- -}`), so a first-`]]`-wins scan is correct.
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
            if text[i:i + 4] == "--[[":
                end = text.find("]]", i + 4)
                i = n if end == -1 else end + 2
                continue
            nl = text.find("\n", i)
            i = n if nl == -1 else nl
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def extract_record_fields(source: str, record_start_pattern: str) -> list[str]:
    """Field names declared in one Haskell record's brace block.

    Comments are stripped first so a haddock comment's prose can never
    desync the brace-depth tracker that finds the block's end.
    """
    lines = _strip_haskell_comments(source).splitlines()
    pat = re.compile(record_start_pattern)
    start = next((i for i, line in enumerate(lines) if pat.search(line)), None)
    if start is None:
        raise ValueError(f"record start not found: {record_start_pattern!r}")
    fields: list[str] = []
    depth = 0
    opened = False
    for line in lines[start:]:
        for ch in line:
            if ch == "{":
                depth += 1
                opened = True
            elif ch == "}":
                depth -= 1
        m = FIELD_LINE_RE.match(line)
        if opened and m:
            fields.append(m.group(1))
        if opened and depth <= 0:
            break
    return fields


def extract_lua_registered_modules(
        scripts_text_by_file: dict[str, str]) -> list[tuple[str, str]]:
    """(module name, file) for every saveMods.register("name", ...) call site.

    Scans the whole (comment/string-aware-stripped) file as one string
    rather than line-by-line, so a call whose arguments span multiple
    lines is still found.
    """
    found: list[tuple[str, str]] = []
    for relpath, text in sorted(scripts_text_by_file.items()):
        cleaned = _strip_lua_comments(text)
        for m in REGISTER_RE.finditer(cleaned):
            found.append((m.group(2), relpath))
    return found


def parse_classified_names(inventory_text: str) -> dict[str, set[str]]:
    """Every backtick-quoted first-column name, keyed by the nearest
    preceding `### OwnerName` heading.

    Classification is scoped PER OWNER, not globally and not merely per
    `## N.` section: several distinct owners can share one numbered
    section (WorldManager/WorldState both live under "## 3.", all four
    save-envelope records under "## 4.") so a name is only "classified"
    for the specific `###`-headed owner it's documented under -- a
    different owner (a sibling record under the same section, or the
    Lua registry) happening to share that name can't mask a missing
    decision.
    """
    by_owner: dict[str, set[str]] = {}
    current_owner: str | None = None
    for line in inventory_text.splitlines():
        heading = OWNER_HEADING_RE.match(line)
        if heading:
            current_owner = heading.group(1)
            continue
        m = ROW_FIRST_CELL_RE.match(line)
        if not m or current_owner is None:
            continue
        for bt in BACKTICK_RE.findall(m.group(1)):
            by_owner.setdefault(current_owner, set()).add(bt)
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
        classified_here = classified.get(label, set())
        for field in fields:
            if field not in classified_here:
                violations.append(
                    f"{label}.{field} ({relpath}) has no classification under "
                    f"the '### {label}' heading in {INVENTORY_PATH.name}")

    classified_lua = classified.get(LUA_OWNER_HEADING, set())
    for name, relpath in extract_lua_registered_modules(scripts_text_by_file):
        if name not in classified_lua:
            violations.append(
                f'Lua save module "{name}" (registered in {relpath}) has no '
                f"classification under the '### {LUA_OWNER_HEADING}' heading "
                f"in {INVENTORY_PATH.name}")

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
