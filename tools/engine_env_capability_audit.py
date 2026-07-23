#!/usr/bin/env python3
"""EngineEnv capability-inventory audit (issue #876, EngineEnv capability
epic #537 requirement 8).

Guards docs/engineenv_capability_inventory.md against silent drift, the
same way tools/persistence_inventory_audit.py guards the persistence
inventory (issue #756) it's deliberately modeled on. Every field
currently declared on `EngineEnv` (src/Engine/Core/State.hs) must have
exactly one row in the capability inventory doc, under a `### `
heading naming one of the eight known capability identifiers, with a
valid Lifecycle category, a Readers and a Writers cell each naming a
known thread/execution role (or an explicitly justified `None`), a
non-placeholder Sync/Init/Shutdown cell, a non-BLANK Notes cell (a
deliberate "nothing further to add" -- this document uses a bare
em-dash for that throughout -- is accepted there, since Notes is the
one column where that is itself a legitimate answer), and at least one
source-location citation somewhere in the row.

This anchors on the EXACT SAME live-declaration parser
tools/persistence_inventory_audit.py already uses
(`extract_record_fields`, imported directly rather than
reimplemented) against the SAME anchor tuple
(EngineEnv / src/Engine/Core/State.hs / the `data EngineEnv = EngineEnv`
pattern) -- so this audit and the persistence-inventory audit can never
independently drift onto two different notions of "the live EngineEnv
field set."

This is a static presence/well-formedness check, not a semantic proof:
it cannot verify that a documented reader/writer/sync/lifecycle
decision is actually TRUE of the code, only that a decision -- using
recognized vocabulary -- has been recorded and stays in sync with the
current field set. It also does not enforce any capability-scoped
import boundary yet; #876 documents the intended boundary (see
docs/engineenv_capability_inventory.md SS6) but this audit does not
check that any module actually respects it. See that document's own
introduction for the full scope statement.

Usage:
  python3 tools/engine_env_capability_audit.py
Exit codes: 0 = every live EngineEnv field is validly classified,
1 = one or more violations found.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from persistence_inventory_audit import extract_record_fields  # type: ignore

INVENTORY_PATH = REPO_ROOT / "docs" / "engineenv_capability_inventory.md"
ENGINE_ENV_FILE = "src/Engine/Core/State.hs"
ENGINE_ENV_PATTERN = r"^data EngineEnv = EngineEnv\b"

# docs/engineenv_capability_inventory.md SS2.1 -- the eight capability
# identifiers a field may be classified under. A generic bucket
# ("misc"/"shared"/"other"), a blank heading, or anything else is not
# in this set and is therefore rejected.
CAPABILITIES = (
    "core-init",
    "render-gpu-asset",
    "input-lua-transport",
    "world-sim-render-handoff",
    "units-buildings-combat",
    "content-registries",
    "ui-hud-events",
    "save-load-coordination",
)

# docs/engineenv_capability_inventory.md SS2.2 -- the thread/execution-
# role vocabulary a Readers/Writers cell must draw from (or the literal
# justified "None").
THREAD_ROLES = (
    "Boot",
    "MainRender",
    "InputThread",
    "LuaThread",
    "WorldThread",
    "UnitThread",
    "CombatThread",
    "SimThread",
    "AnyThread",
)

# docs/engineenv_capability_inventory.md SS2.3.
LIFECYCLE_CATEGORIES = (
    "boot-process",
    "boot-shutdown",
    "session-replaced",
    "transient-handoff",
)

# A free-text cell (Sync/Init/Shutdown/Notes) that is present but
# carries no real content -- name-presence without an actual decision.
_PLACEHOLDER_CELLS = {"", "-", "--", "—", "?", "tbd", "n/a", "na"}

HEADING_RE = re.compile(r"^###\s+`?([a-zA-Z0-9_-]+)`?\s*$")
BACKTICK_RE = re.compile(r"`([^`]+)`")
# A citation worth calling "grounding evidence": a backtick-quoted span
# that names a Haskell or Lua source file (by extension), anywhere in
# the row -- e.g. `` `src/Engine/Core/Init.hs:157` `` or
# `` `scripts/init.lua` ``.
EVIDENCE_RE = re.compile(r"`[^`]*\.(?:hs|lua)[^`]*`")
# STRICT role-cell grammar (fifth and FINAL iteration on this check --
# rounds 2/3/7/8 each closed one bypass while leaving another open via
# a slightly different joiner or token shape: a bare/unquoted role, a
# lower-camel-cased one, one joined by the word "and" instead of "/",
# a wrong-shaped-but-quoted one, and (round 9) one joined by ";" or
# "plus" instead. Rather than keep enumerating joiners -- an unbounded
# list -- this enforces an EXPLICIT, narrow grammar on every
# Readers/Writers cell, checked exactly, with no heuristic guessing
# about what "looks like" a declared role:
#
#   cell     := "None" (`?) WS "(" ... ")"
#             | segment ("," segment)*
#   segment  := role ("/" role)* (WS "(" ... ")")?
#   role     := "`" LETTERS "`"
#
# i.e. every top-level comma-separated segment must EITHER be exactly
# one or more backtick-quoted, slash-joined role names, optionally
# followed by nothing but a single trailing "(...)" parenthetical -- or
# the cell is the special justified-`None` form. There is no
# "and"/";"/"plus"-joined alternative to special-case and no way to
# miss "the next one": anything that isn't backtick-quoted-role(s)-
# then-optional-paren is a GRAMMAR VIOLATION on its own, reported
# directly, never a candidate for further heuristic parsing. All
# explanatory prose (what a role does, why, supporting citations)
# belongs INSIDE that one trailing parenthetical, never bare between
# the role and the paren and never after the paren closes -- e.g.
# "`InputThread` (drains; `Engine.Input.Thread`)" is well-formed,
# "`InputThread` drains (`Engine.Input.Thread`)" is not (the bare word
# "drains" sits outside the parenthetical). Verified against the real
# inventory doc: every one of its ~160 Readers/Writers cells was
# rewritten to conform before this grammar was adopted, with zero
# remaining violations.
_STRICT_SEGMENT_RE = re.compile(
    r"^(?:`[A-Za-z]+`(?:/`[A-Za-z]+`)*)(?:\s*\(.*\))?$")
_ROLE_TOKEN_RE = re.compile(r"`([A-Za-z]+)`")


def _is_placeholder(cell: str) -> bool:
    return cell.strip().lower() in _PLACEHOLDER_CELLS


def _split_top_level_commas(cell: str) -> list[str]:
    """Split `cell` on commas at parenthesis-depth 0."""
    segments: list[str] = []
    depth = 0
    current: list[str] = []
    for ch in cell:
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        if ch == "," and depth == 0:
            segments.append("".join(current))
            current = []
        else:
            current.append(ch)
    segments.append("".join(current))
    return segments


def _attempted_roles(cell: str) -> tuple[list[str], list[str]]:
    """Every DECLARED role token in `cell` (see `_STRICT_SEGMENT_RE`),
    plus every top-level segment that does not conform to the required
    grammar at all. A cell with any malformed segment is invalid
    regardless of what roles it also names -- a malformed segment
    could be hiding an unrecognized role in a form this checker has
    never seen before, so "the rest of the cell looks fine" is not a
    reason to let it through."""
    attempted: list[str] = []
    malformed: list[str] = []
    for segment in _split_top_level_commas(cell):
        stripped = segment.strip()
        if not stripped:
            continue
        if _STRICT_SEGMENT_RE.match(stripped):
            paren_idx = stripped.find("(")
            role_part = stripped if paren_idx == -1 else stripped[:paren_idx]
            attempted.extend(_ROLE_TOKEN_RE.findall(role_part))
        else:
            malformed.append(stripped)
    return attempted, malformed


def _validate_role_cell(cell: str) -> tuple[bool, list[str], list[str]]:
    """Validate a Readers/Writers cell. Returns (is_valid,
    unknown_roles, malformed_segments).

    A cell is valid iff it is `None` immediately followed by a
    non-empty parenthetical justification, OR every top-level segment
    conforms to `_STRICT_SEGMENT_RE` AND every role token it declares
    is a recognized `THREAD_ROLES` identifier, AND at least one role is
    declared somewhere in the cell. A malformed segment is reported on
    its own and short-circuits the unknown-role check -- it's a
    grammar violation, not a "maybe it's fine" situation."""
    stripped = cell.strip()
    bare_start = stripped.lstrip("`")
    if bare_start.lower().startswith("none"):
        rest = bare_start[4:].lstrip("`").strip()
        justified = (rest.startswith("(") and rest.rstrip().endswith(")")
                     and len(rest) > 2)
        return justified, [], []
    attempted, malformed = _attempted_roles(stripped)
    if malformed:
        return False, [], malformed
    unknown = [t for t in attempted if t not in THREAD_ROLES]
    if unknown:
        return False, unknown, []
    return bool(attempted), [], []


class ParsedRow:
    __slots__ = ("field", "capability", "cells", "raw", "line_no")

    def __init__(self, field: str, capability: str, cells: dict[str, str],
                 raw: str, line_no: int):
        self.field = field
        self.capability = capability
        self.cells = cells
        self.raw = raw
        self.line_no = line_no


def parse_inventory(text: str) -> tuple[list[ParsedRow], list[str]]:
    """Every `### <capability>` -> table-row pairing in SS5.

    Returns (rows, parse_violations). A parse violation is something
    that prevented a row from being interpreted at all (e.g. a data row
    with no recognizable backtick-quoted field name in its first
    column) -- distinct from a semantic violation (e.g. an unknown
    lifecycle value), which `audit()` reports against a successfully
    parsed row.
    """
    rows: list[ParsedRow] = []
    violations: list[str] = []
    current_capability: str | None = None
    header_idx: dict[str, int] | None = None
    in_section5 = False

    for line_no, line in enumerate(text.splitlines(), start=1):
        if line.strip() == "## 5. Field inventory":
            in_section5 = True
            continue
        if in_section5 and line.startswith("## "):
            in_section5 = False
        if not in_section5:
            continue

        heading = HEADING_RE.match(line)
        if heading:
            current_capability = heading.group(1)
            header_idx = None
            continue
        if line.strip().startswith("###"):
            # A line that starts a `###` heading but doesn't match
            # HEADING_RE at all (blank, malformed, stray punctuation) --
            # round-10 review: silently falling through here left
            # `current_capability` holding the PRECEDING valid section's
            # value, so rows after a malformed heading wrongly inherited
            # it instead of being flagged. Reset scope explicitly and
            # report the malformed heading itself, exactly like a table
            # row with no enclosing heading is already reported below.
            violations.append(
                f"{INVENTORY_PATH.name}:{line_no}: malformed '### ' "
                f"capability heading (matches no known capability "
                f"pattern): {line.strip()!r}")
            current_capability = None
            header_idx = None
            continue

        if not line.startswith("|"):
            continue

        cells_raw = [c.strip() for c in line.strip().strip("|").split("|")]
        if header_idx is None:
            # This is the header row for the table that just started.
            header_idx = {name: i for i, name in enumerate(cells_raw)}
            continue
        if all(re.fullmatch(r":?-{2,}:?", c) for c in cells_raw if c):
            continue  # the `|---|---|` separator row

        if current_capability is None:
            violations.append(
                f"{INVENTORY_PATH.name}:{line_no}: table row with no "
                f"enclosing '### <capability>' heading in scope")
            continue

        field_cell = cells_raw[0] if cells_raw else ""
        names = BACKTICK_RE.findall(field_cell)
        if len(names) != 1:
            violations.append(
                f"{INVENTORY_PATH.name}:{line_no}: expected exactly one "
                f"backtick-quoted field name in the first column, found "
                f"{len(names)} ({field_cell!r})")
            continue

        cells: dict[str, str] = {}
        for col_name in ("Lifecycle", "Readers", "Writers", "Sync", "Init",
                         "Shutdown", "Notes"):
            idx = header_idx.get(col_name)
            cells[col_name] = cells_raw[idx] if idx is not None and idx < len(cells_raw) else ""

        rows.append(ParsedRow(names[0], current_capability, cells, line, line_no))

    return rows, violations


def audit(engine_env_source: str, inventory_text: str) -> list[str]:
    """Pure audit core. Returns a list of human-readable violations."""
    violations: list[str] = []

    try:
        live_fields = extract_record_fields(engine_env_source, ENGINE_ENV_PATTERN)
    except ValueError as exc:
        return [f"EngineEnv: {exc}"]
    if not live_fields:
        return [f"EngineEnv: no fields extracted from {ENGINE_ENV_FILE} -- "
                f"the parser may be out of sync with this record's layout"]
    live_set = set(live_fields)

    rows, parse_violations = parse_inventory(inventory_text)
    violations.extend(parse_violations)

    seen: dict[str, ParsedRow] = {}
    for row in rows:
        if row.field in seen:
            violations.append(
                f"`{row.field}` has more than one inventory row "
                f"(under '### {seen[row.field].capability}' at line "
                f"{seen[row.field].line_no} and '### {row.capability}' at "
                f"line {row.line_no}) -- every field must have exactly one row")
            continue
        seen[row.field] = row

        if row.field not in live_set:
            violations.append(
                f"`{row.field}` (inventory row at line {row.line_no}, under "
                f"'### {row.capability}') no longer exists on the live "
                f"EngineEnv declaration in {ENGINE_ENV_FILE} -- remove the "
                f"stale row")
            continue

        if row.capability not in CAPABILITIES:
            violations.append(
                f"`{row.field}`'s capability heading '### {row.capability}' "
                f"is not one of {CAPABILITIES} -- a generic bucket "
                f"(misc/shared/other) or an unrecognized identifier is not "
                f"a valid capability owner")

        lifecycle = row.cells["Lifecycle"].strip()
        if lifecycle not in LIFECYCLE_CATEGORIES:
            violations.append(
                f"`{row.field}`'s Lifecycle cell {lifecycle!r} is not one "
                f"of {LIFECYCLE_CATEGORIES}")

        for role_col in ("Readers", "Writers"):
            cell = row.cells[role_col]
            if not cell.strip():
                violations.append(
                    f"`{row.field}` has no {role_col} decision recorded")
                continue
            ok, unknown, malformed = _validate_role_cell(cell)
            if malformed:
                violations.append(
                    f"`{row.field}`'s {role_col} cell has a segment that "
                    f"does not conform to the required role-list grammar "
                    f"(one or more backtick-quoted, slash-joined role names "
                    f"optionally followed by a single trailing parenthetical "
                    f"-- see the module docstring): {malformed!r} "
                    f"(cell: {cell!r})")
            elif unknown:
                violations.append(
                    f"`{row.field}`'s {role_col} cell declares unrecognized "
                    f"thread/execution role(s) {unknown} not in "
                    f"{THREAD_ROLES} (cell: {cell!r}) -- every declared role "
                    f"must be recognized, not merely one of several")
            elif not ok:
                violations.append(
                    f"`{row.field}`'s {role_col} cell {cell!r} names no "
                    f"recognized thread/execution role from {THREAD_ROLES} "
                    f"and is not a justified 'None (...)'")

        # Requirement 2 requires Sync/Init/Shutdown to be recorded facts
        # for every field, same as Readers/Writers/Lifecycle/Capability --
        # a blank or bare-punctuation placeholder in any of them is a
        # missing decision, not a real answer (round-4 review: this used
        # to only check Sync, silently accepting an all-blank row on the
        # other two).
        for required_col in ("Sync", "Init", "Shutdown"):
            if _is_placeholder(row.cells[required_col]):
                violations.append(
                    f"`{row.field}`'s {required_col} cell is blank or a "
                    f"bare placeholder -- record the actual decision")
        # Notes is the one column where "nothing further to add" is
        # itself a legitimate, deliberate answer -- this document uses a
        # bare em-dash for that throughout, so only a genuinely EMPTY
        # cell (an oversight, not a decision) is rejected here.
        if not row.cells["Notes"].strip():
            violations.append(f"`{row.field}`'s Notes cell is blank")

        if not EVIDENCE_RE.search(row.raw):
            violations.append(
                f"`{row.field}`'s row cites no source-location evidence "
                f"(no backtick-quoted `.hs`/`.lua` reference found anywhere "
                f"in the row)")

    missing = sorted(live_set - set(seen))
    for field in missing:
        violations.append(
            f"`{field}` ({ENGINE_ENV_FILE}) has no row in "
            f"{INVENTORY_PATH.name} SS5")

    return violations


def main() -> int:
    engine_env_source = (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8")
    inventory_text = INVENTORY_PATH.read_text(encoding="utf-8")
    violations = audit(engine_env_source, inventory_text)
    if violations:
        print(f"{len(violations)} EngineEnv capability-inventory violation(s):")
        for v in violations:
            print(f"  - {v}")
        print(f"\nAdd or fix a classification row for each item above in "
              f"{INVENTORY_PATH.relative_to(REPO_ROOT)} SS5 (see SS2 for the "
              f"capability/thread-role/lifecycle vocabulary).")
        return 1

    total_fields = len(extract_record_fields(engine_env_source, ENGINE_ENV_PATTERN))
    print(f"engine-env capability-inventory audit: {total_fields} EngineEnv "
          f"field(s) all classified")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
