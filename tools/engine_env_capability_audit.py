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

Since issue #1669 it also checks the prose SS1 uses to describe that
same field set. SS1 states the record's field TOTAL and its first- and
last-field SPAN; before #1669 both were hand-maintained, and both had
drifted: the doc still claimed 83 fields, with three equally stale
source line anchors, against a live record several fields larger, and
every gate stayed green -- the row comparison above only ever compared
field NAMES, never their count and never the prose. `audit_field_total`
re-derives the count and the two span field names from the live
declaration and rejects the marked SS1 block when either disagrees;
SS6.2's assignment-method sentence, which used to repeat the same
total, is marked as prose that must carry no number at all. Each
marker pair is bound to the section it governs, so it cannot be parked
somewhere inert while the real sentence regains a hand-maintained
number. See that function's own section comment for the marker
contract.

This is a static presence/well-formedness check, not a semantic proof:
it cannot verify that a documented reader/writer/sync/lifecycle
decision is actually TRUE of the code, only that a decision -- using
recognized vocabulary -- has been recorded and stays in sync with the
current field set.

Since issue #889 (EngineEnv capability split E1), this module ALSO
enforces the SS6 full-access ratchet: every production (`src/`/`app/`)
Haskell module that imports `Engine.Core.State` either with an
explicit `EngineEnv(..)` or as a bare import (no import list -- both
grant unrestricted field-level access, see SS6's own methodology) must
be either one of SS6.1's permanent modules (a hard, checked-in
allowlist) or one of SS6.2's individually-assigned temporary modules
(a checked-in, strict, shrink-only ceiling established by #889 and
cross-checked against SS6.2's own documented accounting). A module
newly gaining unrestricted access fails this ratchet even if SS6.2 is
ALSO edited to document it -- growing the checked-in ceiling itself
(in this file) is the only way to admit a new temporary full-access
module, and doing so without a matching SS6.2 update fails the
doc/ceiling consistency check below. Both the temporary ceiling AND the
permanent allowlist are checked in BOTH directions: a module also fails
the ratchet if it is listed in the checked-in ceiling (and/or SS6.2) or
in PERMANENT_IMPORTERS (SS6.1) but no longer has live unrestricted
access -- a stale entry left behind by a migration that narrowed the
module without also updating its allowlist/ceiling entry -- since both
SS6.1 and SS6.2 must stay an exact, exhaustive mirror of the live
full-access set, never merely an upper bound on it. `test/` sources
remain outside this ratchet entirely (SS6.3's test-only exception).

Since issue #899 (E8, the epic's final child) that ceiling is __EMPTY__
and the boundary is PERMANENT-ONLY: the live unrestricted production
importer set equals PERMANENT_IMPORTERS exactly, so there is no legal
path left for a module to take unrestricted access -- "add the field
now, narrow it later" no longer exists. Two checks make that flip
unforgiving:

  * `audit_permanent_boundary` parses SS6.1's DOCUMENTED module set
    (its first column only -- Reason cells cite other module names as
    context) and requires it to equal PERMANENT_DEFINER +
    PERMANENT_IMPORTERS, with every row carrying a real, non-placeholder
    Category AND Reason. Documentation alone cannot admit a permanent
    importer, and neither can a constant change with no written
    justification.
  * `audit_save_load_projection` pins E8's own record: the module
    exists, is listed in synarchy.cabal's explicit library module list,
    and its projection binds exactly the five documented
    `save-load-coordination` handles from their matching `EngineEnv`
    accessors.

SS6.4 documents the procedure for what to do instead (most new state
does not belong on `EngineEnv` at all).

Since issue #1892 (capability mutation-authority epic #1890, CMA-1) it
also pins each field's direct WRITING MODULES. SS5's Writers cells are
prose validated for grammar and citation presence only, so until this
check the doc could claim a field has no writers at all and a change
falsifying it passed every gate -- the drift class #1669 closed for the
field COUNT, still open for the ownership CLAIMS.
`CAPABILITY_WRITER_MODULES` is a checked-in, both-directions map of the
same shape as `RENDER_MAIN_ONLY_MODULES`: an undeclared write fails, a
stale entry fails, and the map's KEYS must equal the live field set.
It scans DIRECT `IORef` mutation only, through the raw `EngineEnv`
accessor and through any capability-record accessor projecting it
alike; SS6.1's permanent cohort is exempt (design decision D-4); and
every capability-accessor use the scan cannot attribute -- a handle
passed to a helper, stored in a context record, or handed to a
queue/`TVar`/`MVar` -- is printed as the non-blocking pass-on residue
(D-5), ahead of every blocking check so a failure elsewhere never costs
the measurement. See SS6.5 of the inventory doc and
docs/capability_mutation_authority_design.md.

Usage:
  python3 tools/engine_env_capability_audit.py
Exit codes: 0 = every live EngineEnv field is validly classified and
the SS6 ratchet holds, 1 = one or more violations found.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import NamedTuple

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
        inner = rest[1:-1] if (rest.startswith("(") and rest.endswith(")")) else ""
        justified = bool(inner.strip())
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


# ===========================================================================
# SS1's audited field total and field span (issue #1669)
# ===========================================================================
#
# SS1 used to state a hand-maintained field total ("exactly 83 fields")
# and three hand-written `src/Engine/Core/State.hs` line anchors. All
# four had drifted from the live record while every gate stayed green:
# `audit` above compares the live field SET with SS5's row set, so it
# never looked at the prose total at all, and SS6.2's assignment-method
# procedure repeated the same stale number a second time.
#
# The fix follows the rule this repository settled on in issue #1584:
# a total a document displays is obtained mechanically or is not
# displayed. Here it is displayed and mechanically checked, which needs
# the governed prose to be delimited rather than pattern-matched out of
# free text -- a prose validator that has to GUESS which sentence is
# the total is the shape that burned review rounds in #704/#1128/#1309.
# Two explicit HTML-comment markers do that instead:
#
#   `<!-- engineenv-field-total -->` ... `<!-- /engineenv-field-total -->`
#       SS1's scope paragraph. Exactly one block, and inside it:
#       exactly one number, equal to the live field count; the live
#       record's FIRST and LAST field named in backticks and no other
#       live field name; and the one-row-per-field contract sentence.
#       "Exactly one number" is what keeps a hand-written
#       `State.hs:NNN` anchor out, so the anchors #1669 removed cannot
#       come back either.
#
# SS6.2's assignment-method sentence -- the second copy's old home --
# is governed WITHOUT markers, by structural position plus a content
# anchor: SS6.2's first numbered item must still be that sentence
# (`PROCEDURE_ITEM_ANCHOR`) and must state no number at all. Markers
# there would have to open the list item's content, which starts a
# Markdown HTML block and splits the item's rendering.
#
# Three things stop the governed prose from being hollowed out rather
# than edited: the marker pair is REQUIRED (deleting it is a
# violation), it must be SS1's first content and SS1's only number
# (so an unaudited paragraph cannot stand in front of the audited one,
# and no second count can hide behind it), and the whole document is
# swept for the one unambiguous reintroduction shape,
# `<n> EngineEnv field(s)`.
#
# Section references (`SS5`, `SS7.3` -- written with the section sign in
# the document) are excluded from both number scans. They are
# navigation, not counts, and SS1's contract sentence necessarily names
# SS5.
# The two sections whose prose these markers govern. A block found
# outside its own section is not governing that section's prose, which
# is the whole point of the check.
SECTION_1_HEADING = "## 1. Scope"
# SECTION_6_2_HEADING is defined with the SS6 parsers further down; the
# section-bounds helper takes the heading as an argument so the two
# uses stay on one definition.

FIELD_TOTAL_OPEN = "<!-- engineenv-field-total -->"
FIELD_TOTAL_CLOSE = "<!-- /engineenv-field-total -->"
# SS6.2's assignment-method item 1 -- the sentence that used to carry
# the second copy of the total -- is bound by CONTENT, not by markers:
# it is identified by the phrase below and then required to state no
# number. Markers were tried first and rejected: an HTML comment cannot
# open a Markdown list item's content without starting an HTML block
# and splitting the item's rendering, and marking only part of the
# sentence would leave the rest free to state a total.
PROCEDURE_ITEM_ANCHOR = "`EngineEnv` field names from §5"

# Requirement 2 of issue #1669: the one-row-per-field contract is the
# useful half of SS1's sentence and is independent of the number, so it
# survives explicitly rather than by accident.
ONE_ROW_PHRASE = "exactly one row in §5"

_SECTION_REF_RE = re.compile(r"§\d+(?:\.\d+)*")
_INTEGER_RE = re.compile(r"\d+")


class MarkedSpan(NamedTuple):
    """One marker pair: its inner prose and where the whole block sits.

    `start`/`end` bracket the block INCLUDING both markers, which is
    what the section-containment check needs -- a pair whose opening
    marker is inside its section but whose closing marker is not has
    not kept its prose there.
    """
    body: str
    start: int
    end: int


def extract_marked_spans(text: str, open_marker: str, close_marker: str
                         ) -> tuple[list[MarkedSpan], list[str]]:
    """Return `(spans, violations)` for one marker pair.

    Deliberately literal string scanning, not a regex: the markers are
    fixed literals, and an unbalanced or nested pair must be reported
    as the malformed markup it is rather than silently matching some
    other pair's text.

    The inventory document is the only text this is applied to, so the
    diagnostics name it directly.
    """
    spans: list[MarkedSpan] = []
    violations: list[str] = []
    cursor = 0
    while True:
        start = text.find(open_marker, cursor)
        if start < 0:
            break
        body_start = start + len(open_marker)
        end = text.find(close_marker, body_start)
        if end < 0:
            violations.append(
                f"`{open_marker}` at offset {start} in "
                f"docs/{INVENTORY_PATH.name} is never closed by "
                f"`{close_marker}`")
            break
        nested = text.find(open_marker, body_start, end)
        if nested >= 0:
            violations.append(
                f"`{open_marker}` blocks are nested in "
                f"docs/{INVENTORY_PATH.name} (a second one opens at "
                f"offset {nested} before the first closes) -- the "
                f"governed prose must be one flat block")
        spans.append(MarkedSpan(text[body_start:end], start,
                                end + len(close_marker)))
        cursor = end + len(close_marker)
    return spans, violations


def _countable_numbers(body: str) -> list[str]:
    """Every decimal integer in `body` that is not a section reference."""
    return _INTEGER_RE.findall(_SECTION_REF_RE.sub("", body))


_FENCE_RE = re.compile(r"^(`{3,}|~{3,})")


def _fence_states(text: str) -> "list[bool]":
    """Per line, whether that line is INSIDE a fenced code block.

    A fence opens on a line of three or more backticks or tildes and
    closes on a later line of at least as many of the SAME character.
    The opening and closing fence lines themselves count as inside, so
    a heading can never be read out of fenced content.

    This exists because `section_bounds` decides where a section ends,
    and a fenced block containing a line like `## example` would
    otherwise end the section early -- while Markdown still renders
    everything after it inside the real section. That gap was a live
    bypass of the scope rules below: prose past the fake heading was
    outside the audit and inside the document.
    """
    inside: list[bool] = []
    open_char = ""
    open_len = 0
    for line in text.splitlines():
        stripped = line.strip()
        match = _FENCE_RE.match(stripped)
        if not open_char:
            if match:
                open_char = match.group(1)[0]
                open_len = len(match.group(1))
                inside.append(True)
                continue
            inside.append(False)
            continue
        inside.append(True)
        if (match and match.group(1)[0] == open_char
                and len(match.group(1)) >= open_len
                and not stripped[len(match.group(1)):].strip()):
            open_char = ""
            open_len = 0
    return inside


def section_bounds(text: str, heading: str,
                   stop_prefixes: tuple[str, ...]) -> tuple[int, int] | None:
    """Character bounds of one Markdown section's body, or `None` when
    the heading is absent.

    The body runs from just after the heading line to just before the
    next line whose stripped form starts with one of `stop_prefixes`
    (or to end of document). `"## "` does NOT match `"### "` -- the
    third character is a `#`, not the required space -- so a top-level
    section legitimately contains its own subsections.

    Lines inside a fenced code block are not headings, in either role:
    a fenced `## 1. Scope` does not start the section, and a fenced
    `## anything` does not end it.
    """
    fenced = _fence_states(text)
    start: int | None = None
    offset = 0
    for index, line in enumerate(text.splitlines(keepends=True)):
        stripped = line.strip()
        in_fence = fenced[index] if index < len(fenced) else False
        if start is None:
            if stripped == heading and not in_fence:
                start = offset + len(line)
        elif not in_fence and any(stripped.startswith(prefix)
                                  for prefix in stop_prefixes):
            return start, offset
        offset += len(line)
    if start is None:
        return None
    return start, offset


def _audit_scope_block_placement(inventory_text: str,
                                 spans: list[MarkedSpan]) -> list[str]:
    """SS1's marked block must BE the scope statement, not merely live
    in the same section as one.

    Section containment alone is not enough: the pair could be moved to
    a later paragraph of SS1 -- or into a fenced block -- while SS1's
    opening sentence went back to a hand-maintained total that nothing
    reads. Two rules pin it, and they are deliberately positional
    rather than semantic:

    (a) The pair is SS1's first content. Whatever a reader sees first
        under `## 1. Scope` is the audited paragraph.
    (b) The rest of SS1 states no number at all. Digits are permitted
        only in a SOURCE-LOCATION code span (`` `src/Engine/Core/
        State.hs:446` `` in the out-of-scope list), in a section
        reference (`SS5`), and in an issue reference (`#1669`). A bare
        `` `83` `` is none of those -- code font does not make a field
        total a citation -- so a second copy has nowhere in SS1 to
        live.
    """
    violations: list[str] = []
    doc = f"docs/{INVENTORY_PATH.name}"
    bounds = section_bounds(inventory_text, SECTION_1_HEADING, ("## ",))
    if bounds is None:
        return [f"{doc} has no `{SECTION_1_HEADING}` heading -- "
                f"`{FIELD_TOTAL_OPEN}` is anchored to that section, so it "
                f"cannot be renamed or removed without moving the marker "
                f"contract with it"]
    start, end = bounds
    for span in spans:
        if span.start < start or span.end > end:
            violations.append(
                f"{doc}'s `{FIELD_TOTAL_OPEN}` block (offsets "
                f"{span.start}-{span.end}) is not inside "
                f"`{SECTION_1_HEADING}` (offsets {start}-{end}) -- a "
                f"marker pair moved out of its section governs nothing, "
                f"leaving the scope statement free to drift again")
    if len(spans) == 1 and start <= spans[0].start <= end:
        leading = inventory_text[start:spans[0].start]
        if leading.strip():
            violations.append(
                f"{doc}'s `{FIELD_TOTAL_OPEN}` block is not the first "
                f"content of `{SECTION_1_HEADING}` -- {leading.strip()[:60]!r} "
                f"precedes it. The audited paragraph must be the scope "
                f"statement a reader meets first, or an unaudited one can "
                f"stand in front of it")

    remainder = inventory_text[start:end]
    for span in spans:
        if start <= span.start and span.end <= end:
            remainder = remainder.replace(
                inventory_text[span.start:span.end], "", 1)
    stray = _stray_numbers_outside_code(remainder)
    if stray:
        violations.append(
            f"{doc}'s `{SECTION_1_HEADING}` states number(s) "
            f"{', '.join(stray)} outside its `{FIELD_TOTAL_OPEN}` block. "
            f"SS1 states exactly one number, the field total, inside that "
            f"block; anything else there is a second hand-maintained count "
            f"waiting to drift. Section (SS5) and issue (#1669) references "
            f"are fine, and so is a source-location code span such as "
            f"`src/Engine/Core/State.hs:446` -- but a bare `83` is a field "
            f"total in code font, not a citation")
    return violations


# A backtick span whose digits are a SOURCE LOCATION -- a repository
# path, optionally with a line or line-range anchor. This is the only
# code span whose numbers are exempt from the no-stray-count rule.
#
# Exempting code spans wholesale was the third rereview's finding: it
# let `` `83` `` stand in the governed prose, which reads to a human as
# exactly the stale total this audit exists to remove. A span has to
# LOOK like a source reference to be excused, and a bare number does
# not.
_SOURCE_SPAN_RE = re.compile(
    r"^[A-Za-z0-9_./+\-]+\.(?:hs|lua|py|md|json|yaml|yml|cabal|sh)"
    r"(?::\d+(?:-\d+)?)?$")


def _stray_numbers_outside_code(text: str) -> list[str]:
    """Decimal integers in `text` that are neither a section reference,
    an issue reference, nor part of a source-location code span.

    A code span that is NOT a source location keeps its digits in the
    scan: `` `83` `` is a field total wearing a code font, not a
    citation.
    """
    def _strip_span(match: re.Match[str]) -> str:
        inner = match.group(1).strip()
        return "" if _SOURCE_SPAN_RE.match(inner) else match.group(1)

    without_code = re.sub(r"`([^`]*)`", _strip_span, text)
    without_refs = re.sub(r"#\d+", "", _SECTION_REF_RE.sub("", without_code))
    return _INTEGER_RE.findall(without_refs)


_NUMBERED_ITEM_RE = re.compile(r"^(\d+)\.[ \t]", re.MULTILINE)


def _first_numbered_item(body: str) -> tuple[int, int] | None:
    """`(content start, content end)` of the first `1. ` list item in
    `body`, the item running to the next top-level numbered marker or
    to the end of the text."""
    first = None
    for match in _NUMBERED_ITEM_RE.finditer(body):
        if first is None:
            if match.group(1) != "1":
                continue
            first = match
            continue
        return first.end(), match.start()
    if first is None:
        return None
    return first.end(), len(body)


def _audit_procedure_item_binding(inventory_text: str) -> list[str]:
    """SS6.2's assignment-method item 1 must still be that sentence,
    and must state no number.

    That item is where the second, drifted copy of the total lived.
    Section containment would not be enough even with markers: SS6.2
    legitimately carries many numbers (module tallies, epic ids, its
    own table), so the rule is pinned to the sentence itself -- found
    by structural position AND by its own wording, so that inserting a
    new item ahead of it does not quietly move the governed sentence
    out from under the check.
    """
    doc = f"docs/{INVENTORY_PATH.name}"
    bounds = section_bounds(inventory_text, SECTION_6_2_HEADING,
                            ("## ", "### "))
    if bounds is None:
        return [f"{doc} has no `{SECTION_6_2_HEADING}` heading -- its "
                f"assignment-method item 1 is audited for the field total "
                f"it used to repeat, so the section cannot be renamed or "
                f"removed without moving that rule with it"]
    start, end = bounds
    body = inventory_text[start:end]
    item = _first_numbered_item(body)
    if item is None:
        return [f"{doc}'s `{SECTION_6_2_HEADING}` has no numbered "
                f"assignment-method item -- item 1 is the audited "
                f"sentence, so it must still exist"]
    content = body[item[0]:item[1]]
    violations: list[str] = []
    if PROCEDURE_ITEM_ANCHOR not in content:
        violations.append(
            f"{doc}'s `{SECTION_6_2_HEADING}` item 1 no longer contains "
            f"{PROCEDURE_ITEM_ANCHOR!r} -- that sentence is the one "
            f"audited for the field total it used to repeat, so it may "
            f"not be reworded past recognition or displaced by a new "
            f"item 1. Item 1 currently reads {content.strip()[:80]!r}...")
    stray = _stray_numbers_outside_code(content)
    if stray:
        violations.append(
            f"{doc}'s `{SECTION_6_2_HEADING}` item 1 states number(s) "
            f"{', '.join(stray)} -- that sentence must state no field "
            f"total at all. SS1's marked block is the document's only "
            f"field count; a second copy here is the drift issue #1669 "
            f"removed")
    return violations


# The one reintroduction shape that is unambiguous enough to police
# across the WHOLE document rather than inside a marked block: a number
# directly qualifying `EngineEnv` fields, which is how both drifted
# copies were written ("exactly 83 fields" was SS1's, "one of the 83
# `EngineEnv` field names" was SS6.2's). Deliberately NOT the bare
# "<n> fields" shape -- SS5's capability groups and SS7's roadmap
# legitimately state their own record sizes ("21 fields", "a 14-field
# record"), and a rule that flagged those would be a rule maintainers
# route around.
_ENGINEENV_TOTAL_RE = re.compile(
    r"[`*]{0,2}\d+[`*]{0,2}\s*[`*]{0,2}EngineEnv[`*]{0,2}\s+fields?\b",
    re.IGNORECASE)


def _audit_no_stray_engineenv_total(inventory_text: str,
                                    spans: list[MarkedSpan]) -> list[str]:
    """No `<n> EngineEnv field(s)` phrase anywhere outside the marked
    block, whichever section it appears in."""
    outside = inventory_text
    for span in spans:
        outside = outside.replace(
            inventory_text[span.start:span.end], "", 1)
    found = [m.group(0) for m in _ENGINEENV_TOTAL_RE.finditer(outside)]
    if not found:
        return []
    return [f"docs/{INVENTORY_PATH.name} states {found} outside its "
            f"`{FIELD_TOTAL_OPEN}` block -- the field total is stated "
            f"once, in that block, and nowhere else in this document"]


def audit_field_total(live_fields: list[str], inventory_text: str
                      ) -> list[str]:
    """SS1's stated field total and field span must be the live ones,
    and SS6.2's procedure sentence must state no total at all.

    `live_fields` is the ORDERED field list `extract_record_fields`
    returns, so `[0]`/`[-1]` are the record's real first and last
    field -- which is what SS1's "`engineConfig` through
    `popupQueueRef`" span claim asserts.
    """
    violations: list[str] = []
    doc = f"docs/{INVENTORY_PATH.name}"

    bodies, marker_violations = extract_marked_spans(
        inventory_text, FIELD_TOTAL_OPEN, FIELD_TOTAL_CLOSE)
    violations.extend(marker_violations)
    violations.extend(_audit_scope_block_placement(inventory_text, bodies))
    violations.extend(_audit_no_stray_engineenv_total(
        inventory_text, bodies))
    if not bodies:
        violations.append(
            f"{doc} SS1 has no `{FIELD_TOTAL_OPEN}` block -- the audited "
            f"field-total paragraph is missing, so nothing states the "
            f"live count and nothing can be checked against it")
    elif len(bodies) > 1:
        violations.append(
            f"{doc} has {len(bodies)} `{FIELD_TOTAL_OPEN}` blocks -- "
            f"exactly one paragraph may state the field total, or the "
            f"copies can disagree with each other again")
    else:
        body = bodies[0].body
        numbers = _countable_numbers(body)
        expected = str(len(live_fields))
        if not numbers:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block states no field "
                f"total -- it must state exactly one number, the live "
                f"count ({expected})")
        elif len(numbers) > 1:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block contains "
                f"{len(numbers)} numbers ({', '.join(numbers)}) -- it "
                f"may contain exactly one, the field total. A source "
                f"line number belongs nowhere in it: hand-written "
                f"anchors are what drifted before issue #1669")
        elif numbers[0] != expected:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block states "
                f"{numbers[0]} fields, but {ENGINE_ENV_FILE} declares "
                f"{expected} -- update the block (the total is stated "
                f"once, there, and nowhere else in the document)")

        live_set = set(live_fields)
        named = [t for t in BACKTICK_RE.findall(body) if t in live_set]
        span = [live_fields[0], live_fields[-1]]
        if named != span:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block names live "
                f"EngineEnv field(s) {named} -- it must name exactly the "
                f"record's first and last field, in order "
                f"(`{span[0]}` through `{span[1]}`), and no others")

        if ONE_ROW_PHRASE not in body:
            violations.append(
                f"{doc} SS1's `{FIELD_TOTAL_OPEN}` block no longer states "
                f"the one-row-per-field contract ({ONE_ROW_PHRASE!r}) -- "
                f"that contract is the useful half of the sentence and is "
                f"independent of the number")

    violations.extend(_audit_procedure_item_binding(inventory_text))
    return violations


# ===========================================================================
# SS6 full-access ratchet (issue #889, EngineEnv capability split E1)
# ===========================================================================
#
# docs/engineenv_capability_inventory.md SS6.1's permanent modules -- a
# hard, checked-in allowlist. `Engine.Core.State` itself (the definer,
# which imports nothing and so can never appear in a live importer
# scan) is the 24th permanent module; PERMANENT_IMPORTERS below holds
# only the 23 modules that actually IMPORT it.
PERMANENT_DEFINER = "Engine.Core.State"

PERMANENT_IMPORTERS = frozenset({
    "Engine.Core.Monad",
    "Engine.Core.Init",
    "Engine.Core.Defaults",
    # `Engine.Loop.Headless` left this list in issue #1022: its whole
    # body is now one `Engine.Loop.Mode.LoopMode` value, and the shared
    # driver that reads `lifecycleRef`/`inputQueue`/`saveBarrierRef`
    # (`Engine.Loop.Mode`) names those three fields in a narrow import.
    "Engine.Loop", "Engine.Loop.Frame",
    "Engine.Loop.Shutdown", "Engine.Loop.Camera", "Engine.Loop.Timing",
    "Engine.Loop.Resource",
    "app/App/Graphical.hs", "app/App/Offscreen.hs", "app/App/Preview.hs",
    "app/App/Headless.hs", "app/App/Dump.hs",
    "Engine.Scripting.Lua.Thread", "Engine.Scripting.Lua.Thread.Dispatch",
    "Engine.Scripting.Lua.Thread.Console",
    "Engine.Scripting.Lua.Message",
    "World.Thread.Command.Save", "World.Thread.Command.Save.WriteWorld",
    "World.Load.Stage", "World.Load.Publish", "Engine.Scripting.Lua.API.Save",
})

# docs/engineenv_capability_inventory.md SS6.2 -- the checked-in,
# strict, shrink-only post-E1 ceiling (issue #889 requirement 3): the
# live temporary full-access production importer set as of this
# migration, individually assigned to the same eight capabilities SS2.1
# defines. A module may only be REMOVED from a capability's set here
# (as later migration issues narrow it) -- adding one back, or adding a
# new one, requires this file to change; merely documenting an addition
# in SS6.2 without growing the matching set below still fails the
# ratchet (see `audit_ratchet`).
#
# __EMPTY since issue #899 (E8) -- the epic's end state.__ Every key
# below is retained deliberately: `audit_ratchet`'s doc/ceiling
# cross-check iterates `set(ceiling) | set(doc_temporary)`, so dropping
# a key would silently stop cross-checking that capability's SS6.2 row,
# and the end-state self-test's "every value is empty / SS6.2 has
# exactly the eight CAPABILITIES keys" assertions would go vacuous. The
# ceiling stays shrink-only: with every set empty there is no longer
# ANY legal path for a production module to take unrestricted access --
# it must be narrowed, or (exceptionally, with maintainer approval)
# join SS6.1's permanent set with a documented justification. See
# SS6.4's post-flip procedure.
TEMPORARY_CEILING: dict[str, frozenset[str]] = {
    # Emptied by issue #899 (E8): `Engine.Graphics.Vulkan.Command.Record`
    # now reads its one `engineConfig` hit through
    # Engine.Core.Capability.Core plus a narrow `EngineState(..)`/
    # `GraphicsState(..)` import (the CPS state, not `EngineEnv`), and
    # `Engine.Scripting.Lua.API.Log`'s four `log*Fn` entry points take
    # `CoreCapability` directly and no longer import
    # `Engine.Core.State` at all -- see SS7.1.
    "core-init": frozenset(),
    # Emptied by issue #891 (E3): all 45 modules now reach their render
    # fields through Engine.Core.Capability.Render (MainRender) or
    # Engine.Core.Capability.RenderView (worker threads) -- see the SS3
    # boundary enforcement below.
    "render-gpu-asset": frozenset(),
    # Emptied by issue #892 (E4): all 11 modules now reach their input
    # fields through Engine.Core.Capability.Input (the LuaThread-only
    # eight-field record) or Engine.Core.Capability.InputView (the
    # worker-safe view that carries neither `inputBarrierNextRef` nor
    # `currentKeyDownRef`) -- see the SS7.3 boundary enforcement below.
    # `Engine.Input.Callback` needed no record at all: its API already
    # took the two live handles explicitly, so it merely narrowed its
    # bare import to the `EngineLifecycle` type.
    "input-lua-transport": frozenset(),
    # Emptied by issues #893 (E5a) and #894 (E5b): E5a shrank this row
    # 54 -> 4 by moving every module whose `EngineEnv` use was covered by
    # the nine world/sim fields onto Engine.Core.Capability.WorldSim, and
    # E5b moved the last four (Engine.Scripting.Lua.API.Structure,
    # World.Thread, World.Thread.Command.Basic, World.Thread.Command.Init)
    # onto Engine.Core.Capability.RenderHandoff for the SEVEN coupled
    # render-handoff fields (worldPreviewRef, worldPreviewGenerationRef,
    # zoomAtlasDataRef, worldQuadsRef, bloodDisposeQueue, texPaletteRef,
    # texPaletteHandlesRef), composed with the already-landed WorldSim/
    # RenderView/ContentRegistries/InputView/UnitCombat/Building/Core
    # records plus the one narrow `saveBarrierRef` accessor World.Thread
    # kept until #899 (E8), which moved it onto
    # Engine.Core.Capability.SaveLoad -- see SS7.4 and SS7.8.
    "world-sim-render-handoff": frozenset(),
    # Emptied by issues #895 (E6a) and #896 (E6b): E6a moved the ten
    # unit/combat fields onto Engine.Core.Capability.UnitCombat (49 -> 14,
    # or -- for World.Thread.Command.Edit.Dig -- onto the explicit narrow
    # `statRNGRef`/`unitQueue` parameters its caller supplies), and E6b
    # moved the three building fields (buildingManagerRef, buildingQueue,
    # buildingGhostRef) onto Engine.Core.Capability.Building (14 -> 0).
    # `Unit.Thread` was on E6a's list without naming a building field
    # itself, because it handed its whole environment to
    # `Building.Thread.Command.processAllBuildingCommands`, which it
    # drains on the unit thread (there is no building thread, SS2.2);
    # that drain now takes the building capability plus the logger and
    # world/sim view explicitly instead -- see SS7.5.
    "units-buildings-combat": frozenset(),
    # Emptied by issue #890 (E2): all nine modules now reach their
    # registries through Engine.Core.Capability.ContentRegistries.
    "content-registries": frozenset(),
    # Shrunk from 13 to 2 by issue #897 (E7a), then emptied by issue
    # #898 (E7b): every module whose `EngineEnv` use was covered by the
    # four UI/focus/HUD fields (uiManagerRef, focusManagerRef,
    # hudActivePageRef, textBuffersRef) reaches them through
    # Engine.Core.Capability.Ui, and the two event-dominant modules
    # left (Engine.PlayerEvent.Emit,
    # Engine.Scripting.Lua.API.PlayerEvent) now reach the four
    # event/notification/popup fields (eventStoreRef,
    # notificationCfgRef, notificationOrder, popupQueueRef) through
    # Engine.Core.Capability.Events -- see SS7.7. Neither half has an
    # unrestricted consumer left.
    "ui-hud-events": frozenset(),
    # Never had a temporary consumer: every module whose dominant field
    # usage is save/load coordination is a permanent SS6.1 whole-session
    # orchestration boundary (SS7.8). #899 (E8) added
    # Engine.Core.Capability.SaveLoad for the NON-permanent touchpoints
    # -- the per-tick `captureLocked`/`acknowledgeCurrent` sites -- and
    # narrowed `World.Thread` onto it.
    "save-load-coordination": frozenset(),
}

PRODUCTION_DIRS = ("src", "app")
STATE_MODULE = "Engine.Core.State"
# The record whose selectors `EngineEnv(..)` brings into scope --
# `WindowState(..)` in the same import list brings its own, and not
# these.
ENGINE_ENV_TYPE = "EngineEnv"

_IMPORT_LINE_RE = re.compile(r"^import\b")
_IMPORT_HEAD_RE = re.compile(r"^import\s+(?:qualified\s+)?([A-Za-z][A-Za-z0-9_.']*)")
_EXPLICIT_ENGINEENV_RE = re.compile(r"EngineEnv\s*\(\s*\.\.\s*\)")
# A character literal, including an escape. Used both to step over one
# while stripping comments and to step over one while tokenizing.
_CHAR_LITERAL_RE = re.compile(r"'(?:\\.|[^\\'])'")
# The symbol characters a dash run may continue into. Per the Haskell
# report `--` opens a comment only when the run of dashes is NOT
# followed by one of these -- otherwise it is an operator such as
# `-->`, and the code after it is code.
_SYMBOL_CHARS = frozenset("!#$%&*+./<=>?@\\^|~:-")


def _strip_haskell_comments(text: str) -> str:
    """Blank `{- -}` and `--` comments, preserving every character
    position: comment characters become spaces and newlines are kept,
    so line numbers and the column-0 tests downstream are unaffected.

    __Literal-aware, because a comment marker inside a string is
    text.__ `let marker = "--" in writeIORef (fieldOne env) 1` is a
    real write, and a scanner that stopped at that `--` would drop it
    silently -- the exact failure mode this audit exists to prevent.
    String and character literals are therefore stepped over, block
    comments nest the way Haskell's do, and a dash run continuing into
    a symbol character is an operator rather than a comment."""
    out = list(text)
    i, n = 0, len(text)
    while i < n:
        ch = text[i]
        if ch == '"':
            i += 1
            while i < n and text[i] != '"':
                i += 2 if text[i] == "\\" else 1
            i += 1
            continue
        if ch == "'":
            # A prime continues an identifier; only a `'` that does not
            # follow one can open a character literal.
            previous = text[i - 1] if i else ""
            literal = (None if (previous.isalnum() or previous in "_'")
                       else _CHAR_LITERAL_RE.match(text, i))
            i = literal.end() if literal else i + 1
            continue
        if text.startswith("{-", i):
            depth, j = 0, i
            while j < n:
                if text.startswith("{-", j):
                    depth += 1
                    j += 2
                    continue
                if text.startswith("-}", j):
                    depth -= 1
                    j += 2
                    if depth == 0:
                        break
                    continue
                j += 1
            for k in range(i, min(j, n)):
                if out[k] != "\n":
                    out[k] = " "
            i = j
            continue
        if text.startswith("--", i):
            run = i
            while run < n and text[run] == "-":
                run += 1
            if run < n and text[run] in _SYMBOL_CHARS:
                i = run
                continue
            end = text.find("\n", i)
            end = n if end == -1 else end
            for k in range(i, end):
                out[k] = " "
            i = end
            continue
        i += 1
    return "".join(out)


def _import_chunks(text: str) -> list[str]:
    """Every top-level `import` declaration's FULL text (covering
    multiline module names/import lists), bounded by Haskell's layout
    rule: a continuation line is blank or indented; the declaration
    ends at the first non-blank, column-0 line (the next import, or
    the first non-import top-level declaration -- e.g. a bare import
    that is the file's LAST import is bounded correctly either way)."""
    lines = text.split("\n")
    starts = [i for i, line in enumerate(lines) if _IMPORT_LINE_RE.match(line)]
    chunks = []
    for start in starts:
        end = len(lines)
        for j in range(start + 1, len(lines)):
            line = lines[j]
            if line.strip() == "":
                continue
            if line[0] not in (" ", "\t"):
                end = j
                break
        chunks.append("\n".join(lines[start:end]))
    return chunks


def _classify_state_import_chunk(chunk: str) -> str:
    """`chunk` is already confirmed to import `Engine.Core.State`.
    Returns "explicit" (`EngineEnv(..)`, any combination of qualified/
    aliased/multiline), "bare" (no import list at all -- grants full
    access to every export, qualified/aliased/multiline alike), or
    "narrow" (an explicit list that names neither shape -- e.g. the
    bare `EngineEnv` type, or individual field accessors)."""
    if _EXPLICIT_ENGINEENV_RE.search(chunk):
        return "explicit"
    if "(" not in chunk:
        return "bare"
    return "narrow"


def classify_state_import(source_text: str) -> str | None:
    """The most permissive classification of every `Engine.Core.State`
    import found in `source_text` ("explicit" > "bare" > "narrow"), or
    `None` if the module doesn't import it at all."""
    best: str | None = None
    rank = {"narrow": 0, "bare": 1, "explicit": 2}
    for chunk in _import_chunks(_strip_haskell_comments(source_text)):
        head = _IMPORT_HEAD_RE.match(chunk)
        if not head or head.group(1) != STATE_MODULE:
            continue
        cls = _classify_state_import_chunk(chunk)
        if best is None or rank[cls] > rank[best]:
            best = cls
    return best


def module_identifier(relpath: str) -> str:
    """`src/Engine/Core/Log/Monad.hs` -> `Engine.Core.Log.Monad`
    (matching SS6.2's dotted-name citations); an `app/*.hs` boot module
    keeps its literal relative path (matching SS6.1's own citations --
    every one of them is `module Main where`, so a dotted name would
    collide)."""
    parts = Path(relpath).parts
    if parts[0] == "src":
        return ".".join(parts[1:])[:-len(".hs")]
    return relpath


def classify_production_sources(sources: dict[str, str]) -> set[str]:
    """Pure core of the ratchet scan: given `{relative_path: source_text}`
    for every production Haskell file, the set of module identifiers
    with unrestricted (`explicit`/`bare`) `Engine.Core.State` access."""
    unrestricted = set()
    for relpath, text in sources.items():
        cls = classify_state_import(text)
        if cls in ("explicit", "bare"):
            unrestricted.add(module_identifier(relpath))
    return unrestricted


def scan_production_unrestricted_importers(repo_root: Path) -> set[str]:
    """IO wrapper: walk every `src/**/*.hs` and `app/**/*.hs` file
    under `repo_root` and classify it."""
    return classify_production_sources(scan_production_sources(repo_root))


# ===========================================================================
# SS3 main-render ownership boundary (issue #891, capability split E3)
# ===========================================================================
#
# docs/engineenv_capability_inventory.md SS3 makes `EngineState`
# main-render-thread-private, and SS5 lists `MainRender` as
# `engineStateRef`'s ONLY reader and writer. E1's capability convention
# exports each record as `Capability(..)` -- constructor AND accessors
# -- so a worker-visible record carrying `engineStateRef` would hand
# worker-thread code a way to inspect that pointer no matter what its
# Haddock claimed. #891 therefore splits `render-gpu-asset` into two
# interfaces, and these three checks are what make the split a boundary
# rather than a convention:
#
#   1. Only a module classified `MainRender` may import the full
#      `Engine.Core.Capability.Render`.
#   2. Only the pointer's genuine owners may name `engineStateRef` (or
#      its `rcEngineStateRef` accessor) at all.
#   3. The worker-visible view must not so much as MENTION the field --
#      no field, no accessor, no re-export, hence no path to dereference
#      it.
#
# Like the SS6 ratchet, sets 1 and 2 are checked in BOTH directions: a
# stale entry (a module listed here that no longer does the thing) fails
# too, so neither set can silently decay into a mere upper bound.
RENDER_CAPABILITY_MODULE = "Engine.Core.Capability.Render"
RENDER_VIEW_MODULE = "Engine.Core.Capability.RenderView"

# Production modules that legitimately run on `MainRender` and may hold
# the full 21-field record. Every one of these is a SS6.2
# `render-gpu-asset` module #891 migrated whose execution domain SS5
# records as `MainRender` (the Vulkan device/pipeline/swapchain/texture
# family, font rasterization and upload, the GLFW window, UI/text
# rendering, and the `processLuaMessages`-dispatched Message handlers).
#
# A module reached from a worker thread does NOT belong here even if it
# also has a `MainRender` caller: a dual-domain module must satisfy the
# boundary with the worker-safe view alone (e.g. `World.Render.BloodQuads`,
# whose `renderBloodDecalQuads` runs on `WorldThread` while
# `uploadBloodTextures` runs on `MainRender` -- neither path needs
# `engineStateRef`, so the view serves both).
RENDER_MAIN_ONLY_MODULES = frozenset({
    "Engine.Graphics.Font.Load", "Engine.Graphics.Font.Upload",
    "Engine.Graphics.Vulkan.Command.Sprite", "Engine.Graphics.Vulkan.Command.Text",
    "Engine.Graphics.Vulkan.Init", "Engine.Graphics.Vulkan.Recreate",
    "Engine.Graphics.Vulkan.Texture.Bindless",
    "Engine.Graphics.Vulkan.Texture.DefaultFaceMap",
    "Engine.Graphics.Window.GLFW", "Engine.Scene.Batch.Text",
    "Engine.Scripting.Lua.Message.Texture", "Engine.Scripting.Lua.Message.Video",
    "Engine.Scripting.Lua.Message.WorldTexture", "UI.Render",
})

# The only production modules that may name the main-render-private
# pointer: `Engine.Core.State` declares it, `Engine.Core.Init` seeds it,
# `Engine.Core.Monad` carries it through the CPS Reader environment (the
# "carrying mechanism, not an ownership signal" SS3 describes), and
# `Engine.Core.Capability.Render` projects it into the MainRender-only
# record.
ENGINE_STATE_REF_OWNERS = frozenset({
    "Engine.Core.State", "Engine.Core.Init", "Engine.Core.Monad",
    RENDER_CAPABILITY_MODULE,
})

_ENGINE_STATE_REF_RE = re.compile(r"(?<![A-Za-z0-9_'])(?:rcE|e)ngineStateRef(?![A-Za-z0-9_'])")


def imports_module(source_text: str, module: str) -> bool:
    """True iff `source_text` imports `module` (comments stripped, so a
    Haddock reference to a module name never counts as an import)."""
    for chunk in _import_chunks(_strip_haskell_comments(source_text)):
        head = _IMPORT_HEAD_RE.match(chunk)
        if head and head.group(1) == module:
            return True
    return False


def audit_render_boundary(
    sources: dict[str, str], *,
    main_only: frozenset[str] = RENDER_MAIN_ONLY_MODULES,
    state_ref_owners: frozenset[str] = ENGINE_STATE_REF_OWNERS,
) -> list[str]:
    """Pure core of the SS3 boundary check. `sources` is
    `{relative_path: source_text}` for every production Haskell file
    (the same input `classify_production_sources` takes)."""
    violations: list[str] = []
    live_render_importers: set[str] = set()
    live_state_ref_users: set[str] = set()
    view_source: str | None = None

    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        code = _strip_haskell_comments(text)
        if module == RENDER_VIEW_MODULE:
            view_source = code
        if imports_module(text, RENDER_CAPABILITY_MODULE):
            live_render_importers.add(module)
        if _ENGINE_STATE_REF_RE.search(code):
            live_state_ref_users.add(module)

    for module in sorted(live_render_importers - main_only - {RENDER_CAPABILITY_MODULE}):
        violations.append(
            f"`{module}` imports `{RENDER_CAPABILITY_MODULE}` but is not a "
            f"`MainRender` module (RENDER_MAIN_ONLY_MODULES in "
            f"tools/engine_env_capability_audit.py) -- the full render "
            f"capability carries `engineStateRef`, which "
            f"docs/engineenv_capability_inventory.md SS3 makes main-render "
            f"private. Use `{RENDER_VIEW_MODULE}`'s worker-safe view "
            f"instead; a dual-domain module must satisfy the boundary with "
            f"the view alone")

    for module in sorted(main_only - live_render_importers):
        violations.append(
            f"`{module}` is listed in RENDER_MAIN_ONLY_MODULES but no longer "
            f"imports `{RENDER_CAPABILITY_MODULE}` -- remove the stale entry "
            f"so the checked-in MainRender set stays an exact mirror of the "
            f"live one, not merely an upper bound")

    for module in sorted(live_state_ref_users - state_ref_owners):
        violations.append(
            f"`{module}` names `engineStateRef`/`rcEngineStateRef` but is not "
            f"one of its owners (ENGINE_STATE_REF_OWNERS in "
            f"tools/engine_env_capability_audit.py) -- "
            f"docs/engineenv_capability_inventory.md SS3 confines the "
            f"main-render-private `EngineState` pointer to `MainRender`")

    for module in sorted(state_ref_owners - live_state_ref_users):
        violations.append(
            f"`{module}` is listed in ENGINE_STATE_REF_OWNERS but no longer "
            f"names `engineStateRef` -- remove the stale entry")

    if view_source is None:
        violations.append(
            f"`{RENDER_VIEW_MODULE}` is missing from the production sources "
            f"-- the worker-safe render view is what keeps non-`MainRender` "
            f"consumers off `engineStateRef`; SS3's boundary has no "
            f"enforcement without it")
    elif _ENGINE_STATE_REF_RE.search(view_source):
        violations.append(
            f"`{RENDER_VIEW_MODULE}` mentions `engineStateRef` -- the "
            f"worker-visible render view must provide NO path to the "
            f"main-render-private pointer (no field, no accessor, no "
            f"re-export); see docs/engineenv_capability_inventory.md SS3")

    return violations


# ===========================================================================
# SS7.3 LuaThread ownership boundary (issue #892, capability split E4)
# ===========================================================================
#
# The exact same shape as the SS3 render boundary above, for the exact
# same reason, applied to `input-lua-transport`'s two LuaThread-PRIVATE
# fields: `inputBarrierNextRef` (SS5: "`LuaThread` (only)" -- the
# synthetic-injection barrier-token allocator) and `currentKeyDownRef`
# (SS5: "`LuaThread` (only)" -- the transient `onKeyDown` current-key
# handoff). E1's convention exports each record as `Capability(..)`, so
# a single eight-field record visible to the input/world threads would
# hand them a way to allocate barrier tokens and to inspect or clobber
# the Lua thread's in-flight key. #892 therefore splits the capability
# into two interfaces, enforced by the same three checks:
#
#   1. Only a module that runs on `LuaThread` may import the full
#      `Engine.Core.Capability.Input`.
#   2. Only the two fields' genuine owners may name either one (or its
#      `ic`-prefixed accessor) at all.
#   3. The worker-visible view must not so much as MENTION either field
#      -- no field, no accessor, no re-export, hence no path to reach it.
#
# Sets 1 and 2 are checked in BOTH directions, like SS3's and SS6's.
INPUT_CAPABILITY_MODULE = "Engine.Core.Capability.Input"
INPUT_VIEW_MODULE = "Engine.Core.Capability.InputView"

# Production modules that legitimately run on `LuaThread` and may hold
# the full eight-field record. Both are SS6.2 `input-lua-transport`
# modules #892 migrated whose execution domain SS5 records as
# `LuaThread`: `API.InputInject` is the barrier allocator's only
# non-boot owner, `API.Keybinds` the current-key handoff's.
#
# A module reached from the input or world thread does NOT belong here.
# In particular `Engine.Input.Thread.Dispatch` publishes the barrier
# WATERMARK (`inputBarrierRef`) and must satisfy the boundary with the
# worker-safe view alone -- the view carries the watermark precisely so
# it never needs the allocator.
INPUT_LUA_ONLY_MODULES = frozenset({
    "Engine.Scripting.Lua.API.InputInject",
    "Engine.Scripting.Lua.API.Keybinds",
})

# The only production modules that may name either LuaThread-private
# field: `Engine.Core.State` declares them, `Engine.Core.Init` seeds
# them, `Engine.Core.Capability.Input` projects them into the
# LuaThread-only record, and the three LuaThread consumers actually use
# them -- `Engine.Scripting.Lua.Thread.Dispatch` (a permanent SS6.1
# full-access orchestration module: it is what WRITES
# `currentKeyDownRef` around each `onKeyDown` broadcast) plus the two
# SS6.2 modules above.
#
# `Engine.Core.Monad` is deliberately absent: unlike `engineStateRef`
# it never names either field -- the CPS Reader environment carries
# them structurally, as part of `EngineEnv`, without mentioning them.
INPUT_LUA_ONLY_FIELD_OWNERS = frozenset({
    "Engine.Core.State", "Engine.Core.Init", INPUT_CAPABILITY_MODULE,
    "Engine.Scripting.Lua.Thread.Dispatch",
    "Engine.Scripting.Lua.API.InputInject",
    "Engine.Scripting.Lua.API.Keybinds",
})

_INPUT_LUA_ONLY_FIELD_RE = re.compile(
    r"(?<![A-Za-z0-9_'])"
    r"(?:(?:icI|i)nputBarrierNextRef|(?:icC|c)urrentKeyDownRef)"
    r"(?![A-Za-z0-9_'])")


def audit_input_boundary(
    sources: dict[str, str], *,
    lua_only: frozenset[str] = INPUT_LUA_ONLY_MODULES,
    field_owners: frozenset[str] = INPUT_LUA_ONLY_FIELD_OWNERS,
) -> list[str]:
    """Pure core of the SS7.3 LuaThread boundary check. `sources` is
    `{relative_path: source_text}` for every production Haskell file
    (the same input `classify_production_sources` takes)."""
    violations: list[str] = []
    live_input_importers: set[str] = set()
    live_field_users: set[str] = set()
    view_source: str | None = None

    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        code = _strip_haskell_comments(text)
        if module == INPUT_VIEW_MODULE:
            view_source = code
        if imports_module(text, INPUT_CAPABILITY_MODULE):
            live_input_importers.add(module)
        if _INPUT_LUA_ONLY_FIELD_RE.search(code):
            live_field_users.add(module)

    for module in sorted(live_input_importers - lua_only - {INPUT_CAPABILITY_MODULE}):
        violations.append(
            f"`{module}` imports `{INPUT_CAPABILITY_MODULE}` but is not a "
            f"`LuaThread` module (INPUT_LUA_ONLY_MODULES in "
            f"tools/engine_env_capability_audit.py) -- the full input "
            f"capability carries `inputBarrierNextRef` and "
            f"`currentKeyDownRef`, which "
            f"docs/engineenv_capability_inventory.md SS5 makes `LuaThread` "
            f"private. Use `{INPUT_VIEW_MODULE}`'s worker-safe view "
            f"instead; a dual-domain module must satisfy the boundary with "
            f"the view alone")

    for module in sorted(lua_only - live_input_importers):
        violations.append(
            f"`{module}` is listed in INPUT_LUA_ONLY_MODULES but no longer "
            f"imports `{INPUT_CAPABILITY_MODULE}` -- remove the stale entry "
            f"so the checked-in LuaThread set stays an exact mirror of the "
            f"live one, not merely an upper bound")

    for module in sorted(live_field_users - field_owners):
        violations.append(
            f"`{module}` names `inputBarrierNextRef`/`currentKeyDownRef` "
            f"(or an `ic`-prefixed accessor) but is not one of their owners "
            f"(INPUT_LUA_ONLY_FIELD_OWNERS in "
            f"tools/engine_env_capability_audit.py) -- "
            f"docs/engineenv_capability_inventory.md SS5 confines the "
            f"barrier-token allocator and the `onKeyDown` current-key "
            f"handoff to `LuaThread`")

    for module in sorted(field_owners - live_field_users):
        violations.append(
            f"`{module}` is listed in INPUT_LUA_ONLY_FIELD_OWNERS but no "
            f"longer names `inputBarrierNextRef`/`currentKeyDownRef` -- "
            f"remove the stale entry")

    if view_source is None:
        violations.append(
            f"`{INPUT_VIEW_MODULE}` is missing from the production sources "
            f"-- the worker-safe input view is what keeps non-`LuaThread` "
            f"consumers off the barrier allocator and the current-key "
            f"handoff; SS7.3's boundary has no enforcement without it")
    elif _INPUT_LUA_ONLY_FIELD_RE.search(view_source):
        violations.append(
            f"`{INPUT_VIEW_MODULE}` mentions `inputBarrierNextRef`/"
            f"`currentKeyDownRef` -- the worker-visible input view must "
            f"provide NO path to either `LuaThread`-private field (no "
            f"field, no accessor, no re-export); see "
            f"docs/engineenv_capability_inventory.md SS7.3")

    return violations


def scan_production_sources(repo_root: Path) -> dict[str, str]:
    """IO wrapper: `{relative_path: source_text}` for every production
    Haskell file under `repo_root`."""
    sources: dict[str, str] = {}
    for base in PRODUCTION_DIRS:
        for path in sorted((repo_root / base).rglob("*.hs")):
            relpath = str(path.relative_to(repo_root))
            sources[relpath] = path.read_text(encoding="utf-8", errors="replace")
    return sources


SECTION_6_1_HEADING = "### 6.1 Permanent (production)"
SECTION_6_2_HEADING = "### 6.2 Temporary compatibility boundary (production)"
# A Modules cell that is ENTIRELY one italicized parenthetical --
# `*(...)*` spanning the whole cell -- is explanatory prose (citing
# other modules/fields for context), never a module assignment, no
# matter what backtick-quoted names it contains; see the real
# `save-load-coordination` row.
_EXPLANATORY_CELL_RE = re.compile(r"^\*\(.*\)\*$", re.DOTALL)
_SEPARATOR_ROW_RE = re.compile(r":?-{2,}:?")


def parse_temporary_boundary(inventory_text: str) -> dict[str, set[str]]:
    """Parse SS6.2's table: `{capability: {module, ...}}`, one entry
    per capability row, individually parsed (never a glob/catch-all).
    A capability whose Modules cell is pure explanatory prose (the
    `save-load-coordination` row) maps to an empty set, not the
    backtick-quoted names that prose happens to cite."""
    lines = inventory_text.splitlines()
    try:
        start = next(i for i, line in enumerate(lines)
                     if line.strip() == SECTION_6_2_HEADING) + 1
    except StopIteration:
        return {}

    result: dict[str, set[str]] = {}
    header_seen = False
    for line in lines[start:]:
        stripped = line.strip()
        if stripped.startswith("#"):
            break  # SS6.3 (or any later heading) ends the table
        if not stripped.startswith("|"):
            continue
        cells = [c.strip() for c in stripped.strip("|").split("|")]
        if not header_seen:
            header_seen = True
            continue
        if all(_SEPARATOR_ROW_RE.fullmatch(c) for c in cells if c):
            continue
        if len(cells) < 2:
            continue
        cap_names = BACKTICK_RE.findall(cells[0])
        if len(cap_names) != 1:
            continue
        capability = cap_names[0]
        modules_cell = cells[1]
        if _EXPLANATORY_CELL_RE.match(modules_cell):
            result[capability] = set()
        else:
            result[capability] = set(BACKTICK_RE.findall(modules_cell))
    return result


def parse_permanent_boundary(inventory_text: str
                             ) -> list[tuple[set[str], str, str]]:
    """Parse SS6.1's table into one `(modules, category, reason)` triple
    per row.

    __First column only.__ SS6.1's Reason cells routinely cite OTHER
    backtick-quoted module names as supporting context
    (`World.Save.Snapshot`, `Engine.Save.Barrier`, `Engine.Loop.Mode`,
    ...) which are explicitly NOT permanent-allowlist entries -- reading
    the whole row would admit every one of them. The Module(s) cell may
    name several modules (the `Engine.Loop.*` and `app/App/*.hs`
    families each occupy one row); the Category/Reason cells are
    returned verbatim so the caller can reject a name-only row that
    provides none of the justification SS6.1/SS6.4 demand.
    """
    lines = inventory_text.splitlines()
    try:
        start = next(i for i, line in enumerate(lines)
                     if line.strip() == SECTION_6_1_HEADING) + 1
    except StopIteration:
        return []

    rows: list[tuple[set[str], str, str]] = []
    header_seen = False
    for line in lines[start:]:
        stripped = line.strip()
        if stripped.startswith("#"):
            break  # SS6.2 (or any later heading) ends the table
        if not stripped.startswith("|"):
            continue
        cells = [c.strip() for c in stripped.strip("|").split("|")]
        if not header_seen:
            header_seen = True
            continue
        if all(_SEPARATOR_ROW_RE.fullmatch(c) for c in cells if c):
            continue
        if len(cells) < 3:
            continue
        modules = set(BACKTICK_RE.findall(cells[0]))
        if not modules:
            continue
        rows.append((modules, cells[1], cells[2]))
    return rows


def audit_permanent_boundary(inventory_text: str, *,
                             permanent: frozenset[str] = PERMANENT_IMPORTERS,
                             definer: str = PERMANENT_DEFINER) -> list[str]:
    """SS6.1's DOCUMENTED permanent set must equal the checked-in
    `PERMANENT_DEFINER` + `PERMANENT_IMPORTERS` constants exactly, and
    every row must actually justify itself.

    `audit_ratchet` already pins the constants to the LIVE source in
    both directions. This closes the remaining gap: without it, growing
    a live importer AND the Python constant together passes, with the
    inventory never recording why the new module is a genuine
    whole-session orchestration boundary. Requirement 3 of issue #899
    ("documentation alone, or a Python constant change without the
    matching inventory justification, must not admit a new permanent
    importer") is exactly this check plus `audit_ratchet`'s.
    """
    violations: list[str] = []
    rows = parse_permanent_boundary(inventory_text)
    if not rows:
        return [f"docs/{INVENTORY_PATH.name} SS6.1's permanent-allowlist "
                f"table could not be parsed (heading "
                f"`{SECTION_6_1_HEADING}` missing, or it has no rows) -- "
                f"the permanent boundary has no documented set to compare "
                f"the checked-in PERMANENT_IMPORTERS/PERMANENT_DEFINER "
                f"constants against"]

    documented: set[str] = set()
    for modules, category, reason in rows:
        names = ", ".join(f"`{m}`" for m in sorted(modules))
        if _is_placeholder(category):
            violations.append(
                f"SS6.1's row for {names} has an empty or placeholder "
                f"Category cell -- a permanent exception must state which "
                f"kind it is (permanent initialization/orchestration "
                f"infrastructure, or the engine-monad carrier itself), not "
                f"merely name the module")
        if _is_placeholder(reason):
            violations.append(
                f"SS6.1's row for {names} has an empty or placeholder "
                f"Reason cell -- a permanent exception must carry an "
                f"explicit written justification for why it is a genuine "
                f"whole-session boundary that cannot be narrowed (see "
                f"SS6.4's post-flip procedure)")
        documented |= modules

    expected = set(permanent) | {definer}
    for module in sorted(documented - expected):
        violations.append(
            f"`{module}` is documented in SS6.1's permanent allowlist but "
            f"is not in the checked-in PERMANENT_IMPORTERS/"
            f"PERMANENT_DEFINER constants (tools/"
            f"engine_env_capability_audit.py) -- documenting a permanent "
            f"exception does not grant it; the constants and the "
            f"inventory must be changed together")
    for module in sorted(expected - documented):
        violations.append(
            f"`{module}` is in the checked-in PERMANENT_IMPORTERS/"
            f"PERMANENT_DEFINER constants but has no row in "
            f"docs/{INVENTORY_PATH.name} SS6.1 -- a Python constant "
            f"change without the matching inventory justification must "
            f"not admit a permanent importer")
    return violations


# ===========================================================================
# SaveLoadCapability projection correspondence (issue #899, E8)
# ===========================================================================
#
# The static half of the aliasing contract. A Python audit cannot
# observe runtime container identity, so it checks the SOURCE-LEVEL
# correspondence -- every capability field bound from the matching
# `EngineEnv` accessor -- in the same shape the SS3/SS7.3 boundary
# checks already use. Genuine aliasing (the same live IORef/TVar) is
# proven separately by the hspec module
# `Test.Headless.Capability.SaveLoad`, using the established
# `sameContainer` pattern. Both are required: the static check catches
# a transposed or renamed binding in review, the runtime one catches a
# projection that copies or reconstructs a container.
SAVE_LOAD_CAPABILITY_MODULE = "Engine.Core.Capability.SaveLoad"
SAVE_LOAD_CAPABILITY_FILE = "src/Engine/Core/Capability/SaveLoad.hs"
SAVE_LOAD_PROJECTION = "toSaveLoadCapability"

# `{capability field: EngineEnv accessor}` -- the exact five handles
# docs/engineenv_capability_inventory.md SS5's `save-load-coordination`
# table lists, and nothing else.
SAVE_LOAD_FIELD_MAP = {
    "slLoadStatusRef": "loadStatusRef",
    "slPendingLoadRef": "pendingLoadRef",
    "slSaveBarrierRef": "saveBarrierRef",
    "slLastSaveTimeRef": "lastSaveTimeRef",
    "slNextItemInstanceIdRef": "nextItemInstanceIdRef",
}

# `field = accessor env`, with the accessor optionally QUALIFIED: a
# capability module may import `Engine.Core.State` under an alias and
# project `fkFieldOne = State.fieldOne env`. Missing that spelling
# would drop the accessor from `capability_accessor_map` entirely, and
# with it every write made through the record.
_PROJECTION_BINDING_RE = re.compile(
    r"(?<![A-Za-z0-9_'])([A-Za-z][A-Za-z0-9_']*)\s*=\s*"
    r"((?:[A-Z][A-Za-z0-9_']*\.)*[A-Za-z][A-Za-z0-9_']*)"
    r"\s+env(?![A-Za-z0-9_'])")

# docs/engineenv_capability_inventory.md SS2.1's abstract-wrapper
# extension (issue #1896): a view field may be
# `field = wrapper (accessor env)` instead of `field = accessor env`.
# The wrapper set is a CLOSED, named list, not "any function", because
# what earns the alias treatment is the guarantee the wrapper carries:
# `Engine.Core.ReadOnlyRef.toReadOnlyRef` is documented to wrap the
# caller's live handle and never to copy it, so the projected field is
# the same container the accessor named. A projection that applied any
# other function would be transforming the state, and inventing an
# alias for it is exactly what this regex must not do -- so an
# unrecognized wrapper is skipped, and the field simply does not
# canonicalize.
#
# Missing this shape is not cosmetic. `capability_accessor_map` is what
# turns a record selector into an `EngineEnv` field for BOTH the write
# scan and the pass-on residue, so a dropped view accessor would make
# every use of it invisible -- including the context-record pass-on in
# `Building.Knowledge.Live` that D-7 exists to demonstrate, which would
# have silently left the residue CMA-3 weighs.
ALIAS_PRESERVING_WRAPPERS = frozenset({"toReadOnlyRef"})

_WRAPPED_PROJECTION_BINDING_RE = re.compile(
    r"(?<![A-Za-z0-9_'])([A-Za-z][A-Za-z0-9_']*)\s*=\s*"
    r"((?:[A-Z][A-Za-z0-9_']*\.)*[A-Za-z][A-Za-z0-9_']*)"
    r"\s*\(\s*"
    r"((?:[A-Z][A-Za-z0-9_']*\.)*[A-Za-z][A-Za-z0-9_']*)"
    r"\s+env\s*\)")


def parse_projection_bindings(source_text: str, projection: str
                              ) -> dict[str, str]:
    """`{capability field: EngineEnv accessor}` for every
    `field = accessor env` binding inside `projection`'s record
    construction, the accessor reported BARE whether it was written
    qualified or not. Comments are stripped first, so a Haddock example
    never counts as a binding. Returns `{}` if the projection is not
    defined in `source_text` at all.

    A view field wrapped by a named alias-preserving wrapper --
    `field = toReadOnlyRef (accessor env)`, SS2.1's abstract-wrapper
    extension -- reports the same accessor as the bare form, because it
    names the same live container (`ALIAS_PRESERVING_WRAPPERS` says
    which wrappers make that promise). Any other function around the
    accessor is not recognized, and the field does not canonicalize."""
    code = _strip_haskell_comments(source_text)
    lines = code.split("\n")
    start = None
    equation = re.compile(rf"^{re.escape(projection)}\s+env\s*=")
    for i, line in enumerate(lines):
        if equation.match(line):
            start = i
            break
    if start is None:
        return {}

    depth = 0
    seen_open = False
    body: list[str] = []
    for line in lines[start:]:
        body.append(line)
        depth += line.count("{") - line.count("}")
        if "{" in line:
            seen_open = True
        if seen_open and depth <= 0:
            break
    # The qualifier says which module the accessor came from, which is
    # already settled by the time we get here; the FIELD name is what
    # every consumer wants, so it is returned bare.
    text = "\n".join(body)
    bindings = {field: accessor.rpartition(".")[2]
                for field, accessor
                in _PROJECTION_BINDING_RE.findall(text)}
    for field, wrapper, accessor in (
            _WRAPPED_PROJECTION_BINDING_RE.findall(text)):
        if wrapper.rpartition(".")[2] in ALIAS_PRESERVING_WRAPPERS:
            bindings[field] = accessor.rpartition(".")[2]
    return bindings


def audit_save_load_projection(
    sources: dict[str, str], cabal_text: str, *,
    field_map: dict[str, str] | None = None,
) -> list[str]:
    """Pure core of the E8 record check: the module exists in the
    production sources, is listed in the library's explicit
    `synarchy.cabal` module list (an unlisted source file compiles
    nowhere and so could satisfy a warning-clean build while being
    dead), and its projection binds exactly the five documented handles
    from their matching `EngineEnv` accessors."""
    expected = dict(SAVE_LOAD_FIELD_MAP if field_map is None else field_map)
    violations: list[str] = []

    source = None
    for relpath, text in sources.items():
        if module_identifier(relpath) == SAVE_LOAD_CAPABILITY_MODULE:
            source = text
            break
    if source is None:
        return [f"`{SAVE_LOAD_CAPABILITY_MODULE}` is missing from the "
                f"production sources ({SAVE_LOAD_CAPABILITY_FILE}) -- the "
                f"`save-load-coordination` capability record is what "
                f"non-permanent barrier/load-status consumers narrow to "
                f"(docs/engineenv_capability_inventory.md SS7.8)"]

    if not re.search(rf"^\s*{re.escape(SAVE_LOAD_CAPABILITY_MODULE)}\s*$",
                     cabal_text, re.MULTILINE):
        violations.append(
            f"`{SAVE_LOAD_CAPABILITY_MODULE}` is not listed in "
            f"synarchy.cabal's explicit library module list -- an "
            f"unlisted source file is never compiled, so a warning-clean "
            f"build would say nothing about it")

    bindings = parse_projection_bindings(source, SAVE_LOAD_PROJECTION)
    if not bindings:
        return violations + [
            f"`{SAVE_LOAD_CAPABILITY_MODULE}` defines no "
            f"`{SAVE_LOAD_PROJECTION} env = ...` record construction -- "
            f"E1's convention requires one total, one-way "
            f"`EngineEnv -> XCapability` projection"]

    for field, accessor in sorted(expected.items()):
        actual = bindings.get(field)
        if actual is None:
            violations.append(
                f"`{SAVE_LOAD_PROJECTION}` does not bind `{field}` -- the "
                f"projection must be TOTAL over the five "
                f"`save-load-coordination` handles")
        elif actual != accessor:
            violations.append(
                f"`{SAVE_LOAD_PROJECTION}` binds `{field}` from "
                f"`{actual} env`, not `{accessor} env` -- a projection "
                f"wired to the wrong same-typed `EngineEnv` handle "
                f"typechecks silently and detaches the capability's view "
                f"from the live state")
    for field in sorted(set(bindings) - set(expected)):
        violations.append(
            f"`{SAVE_LOAD_PROJECTION}` binds `{field}`, which is not one "
            f"of the five documented `save-load-coordination` handles -- "
            f"widening the record needs a SS5/SS6.4 inventory change "
            f"first, not a silent addition")
    return violations


def audit_ratchet(unrestricted: set[str], doc_temporary: dict[str, set[str]],
                   *, permanent: frozenset[str] = PERMANENT_IMPORTERS,
                   ceiling: dict[str, frozenset[str]] = TEMPORARY_CEILING
                   ) -> list[str]:
    """Pure ratchet core. `unrestricted` is a live-scanned production
    importer set (`classify_production_sources`/
    `scan_production_unrestricted_importers`); `doc_temporary` is
    SS6.2 as documented (`parse_temporary_boundary`); `permanent`/
    `ceiling` are the checked-in constants above (overridable so tests
    can exercise this against small synthetic fixtures instead of the
    real ~200-module repo state)."""
    violations: list[str] = []
    ceiling_all: set[str] = set()
    for modules in ceiling.values():
        ceiling_all |= modules
    allowed = set(permanent) | ceiling_all

    for module in sorted(unrestricted - allowed):
        violations.append(
            f"`{module}` has unrestricted `Engine.Core.State` access (a "
            f"bare import or `EngineEnv(..)`) but is neither in the SS6.1 "
            f"permanent allowlist nor the checked-in SS6.2 temporary "
            f"ceiling (PERMANENT_IMPORTERS/TEMPORARY_CEILING in "
            f"tools/engine_env_capability_audit.py) -- a newly full-access "
            f"module must be narrowed, not merely documented; see "
            f"docs/engineenv_capability_inventory.md SS6")

    for module in sorted(ceiling_all - unrestricted):
        violations.append(
            f"`{module}` is listed in the checked-in TEMPORARY_CEILING but "
            f"no longer has unrestricted `Engine.Core.State` access in the "
            f"live source -- it must be REMOVED from TEMPORARY_CEILING and "
            f"its SS6.2 row (docs/engineenv_capability_inventory.md), not "
            f"left as a stale entry: SS6.2's accounting must exactly mirror "
            f"the live temporary set, not merely bound it from above")

    # SS6.1's permanent allowlist must agree with the live scan just as
    # strictly as SS6.2's temporary ceiling does -- a permanent module
    # that has since been narrowed (and so no longer needs its SS6.1
    # exception) must be caught too, not only growth into a NEW
    # full-access module. `Engine.Core.State` itself (the definer) is
    # deliberately excluded from `permanent` above (PERMANENT_IMPORTERS
    # holds only the 24 actual importers, never the definer), so no
    # special-case exclusion is needed here.
    for module in sorted(set(permanent) - unrestricted):
        violations.append(
            f"`{module}` is listed in the checked-in PERMANENT_IMPORTERS "
            f"allowlist (SS6.1) but no longer has unrestricted "
            f"`Engine.Core.State` access in the live source -- remove it "
            f"from PERMANENT_IMPORTERS and its SS6.1 entry "
            f"(docs/engineenv_capability_inventory.md) once it has "
            f"genuinely been narrowed, rather than leaving a stale "
            f"allowlist entry the live scan no longer agrees with")

    for cap in sorted(set(ceiling) | set(doc_temporary)):
        ceiling_set = set(ceiling.get(cap, frozenset()))
        doc_set = doc_temporary.get(cap, set())
        missing_from_doc = ceiling_set - doc_set
        extra_in_doc = doc_set - ceiling_set
        if missing_from_doc or extra_in_doc:
            detail = []
            if missing_from_doc:
                detail.append(
                    f"checked-in ceiling has {sorted(missing_from_doc)} not "
                    f"documented in SS6.2")
            if extra_in_doc:
                detail.append(
                    f"SS6.2 documents {sorted(extra_in_doc)} not present in "
                    f"the checked-in ceiling")
            violations.append(
                f"capability `{cap}`: the checked-in TEMPORARY_CEILING and "
                f"docs/engineenv_capability_inventory.md SS6.2 disagree "
                f"({'; '.join(detail)})")

    return violations


# ===========================================================================
# SS5 writing-module map (issue #1892, capability mutation-authority
# epic #1890 -- CMA-1)
# ===========================================================================
#
# SS5 records a Writers cell for every `EngineEnv` field, and until this
# section nothing checked one against the code: the cells were validated
# for role GRAMMAR and citation PRESENCE only, so SS5 could claim a field
# has no writers at all and a change falsifying that passed every gate.
# This is the drift class #1669 closed for the field COUNT, still open
# for the ownership CLAIMS.
#
# __What this checks, precisely.__ SS5 declares thread ROLES; a source
# scan yields MODULES; the repository carries no mapping between them,
# and (design decision D-2a in docs/capability_mutation_authority_design.md)
# the mapping is not even well-defined at module granularity --
# `World.Render.BloodQuads` is deliberately dual-domain and writes
# `textureSystemRef` from a `MainRender` function while its quad-building
# path runs on `WorldThread`, so the role is a property of the FUNCTION.
# This section therefore maintains its own checked-in field ->
# writing-modules map, independent of SS5's role cells, and verifies the
# weaker, honest property "the set of modules writing this field is what
# we last declared" -- NOT "SS5's role claim is true". SS5's
# Readers/Writers cells stay prose (D-2a).
#
# Checked in BOTH directions, exactly like `RENDER_MAIN_ONLY_MODULES`
# (issue #891) and the SS6 ratchet: an undeclared write fails, and a
# mapped module that no longer writes the field fails just as loudly, so
# the map can never decay into a mere upper bound. The map's KEYS are
# checked both ways too -- they must equal the live `EngineEnv` field
# set, so a newly added field cannot slip in unmapped and a removed one
# cannot leave a stale key behind. `frozenset()` is the legitimate value
# for a field with no detected in-scope direct write.
#
# __Scope: direct IORef mutation only (D-2's consequences, D-5).__ A
# write is detected only where an `IORef` mutation primitive is applied
# DIRECTLY to a known accessor application -- `writeIORef (accessor
# handle) ...`, bare or qualified (`State.fieldOne`, under the module's
# own name or an `as` alias; see `parse_imports`), prefix or
# backticked-infix. Two rules keep a textual match honest, and neither
# models Haskell's binding forms: import scope under the exact spelling
# used, and an APPLIED argument (`_first_argument_head` /
# `_infix_left_operand_head`). `SHADOW_EXEMPTIONS` covers their
# residue, and `audit_mutation_sites` makes the recognized-form list
# CLOSED by failing on a site whose argument the scan cannot read.
# Mutation through a queue, a `TVar`, an `MVar`, an opaque
# internally-synchronized handle (`SaveBarrier`, `LoadStatusRef`), or a
# helper that took the `IORef` as an argument is NOT a write this scan
# can see, and is deliberately out of this slice: full interprocedural
# attribution is Haskell dataflow analysis written in Python, explicitly
# rejected for this arc (D-5). What the scan cannot attribute it
# REPORTS, as the non-blocking residue below.
#
# __The SS6.1 exemption (D-4).__ The 24 permanent full-access modules
# (`PERMANENT_DEFINER` + `PERMANENT_IMPORTERS`) hold whole-session
# orchestration authority by job description and this arc does not
# constrain them, so their writes are neither reported as violations nor
# admitted into the map. The boundary this section draws is the
# capability-narrowed consumer cohort. The residue does NOT share that
# exemption: it measures where a capability HANDLE escapes, which is
# evidence for CMA-2's pilot no matter which module does it.

# The `IORef` mutation primitives a direct write goes through. The
# design measured `writeIORef`/`modifyIORef'`/`atomicModifyIORef'`; the
# whole family is listed so a site that switches to a sibling primitive
# stays visible instead of silently leaving the scan.
IOREF_WRITE_PRIMITIVES = frozenset({
    "writeIORef", "atomicWriteIORef",
    "modifyIORef", "modifyIORef'",
    "atomicModifyIORef", "atomicModifyIORef'",
})

# Reads are not authority-checked, but they DO consume a handle inline,
# so they are what separates an inline use from a passed-onward one in
# the residue classification below.
IOREF_READ_PRIMITIVES = frozenset({"readIORef"})
IOREF_ACCESS_PRIMITIVES = IOREF_WRITE_PRIMITIVES | IOREF_READ_PRIMITIVES

# Issue #1896 (CMA-2) gave `content-registries` a reader-facing view
# whose selected fields are `Engine.Core.ReadOnlyRef.ReadOnlyRef`s.
# Such a field has NO write primitive by construction -- that is the
# whole point of the type -- so nothing joins IOREF_WRITE_PRIMITIVES
# here. What it does have is a read, and the read matters for exactly
# the reason `readIORef` does: it CONSUMES the handle inline, so
# without it every migrated reader's ordinary read would be counted as
# a pass-on and the residue measurement CMA-3 weighs would inflate by
# the size of the migration.
READ_ONLY_REF_MODULE = "Engine.Core.ReadOnlyRef"
READ_ONLY_REF_READ_PRIMITIVES = frozenset({"readReadOnlyRef"})

CAPABILITY_MODULE_PREFIX = "Engine.Core.Capability."

# Where the primitives come from. A name is only the primitive if the
# module actually has THAT one in scope under the spelling used -- the
# same rule the accessors are held to, and for the same reason: a
# module may define its own `writeIORef`, or qualify an unrelated
# module's homonym, and calling it is not an `IORef` mutation. Every
# module in this tree that mutates one imports `Data.IORef` bare.
IOREF_MODULE = "Data.IORef"

# `{primitive: the module it must come from}`. The scan resolves a
# primitive through this table, so a handle-consuming operation defined
# somewhere other than `Data.IORef` is recognized under the identical
# in-scope rule rather than by a second, looser path.
ACCESS_PRIMITIVE_MODULES: dict[str, str] = dict(
    [(name, IOREF_MODULE) for name in sorted(IOREF_ACCESS_PRIMITIVES)]
    + [(name, READ_ONLY_REF_MODULE)
       for name in sorted(READ_ONLY_REF_READ_PRIMITIVES)])

# docs/engineenv_capability_inventory.md SS5's writing-module map: for
# every live `EngineEnv` field, the production modules that DIRECTLY
# mutate it -- through the field's own accessor or through any
# capability-record accessor projecting it. Seeded from the real write
# sites present when issue #1892 landed, and maintained the same way
# `RENDER_MAIN_ONLY_MODULES` is: `audit_writer_modules` rejects an
# undeclared write AND a stale entry, so the map is an exact mirror of
# the detected write set rather than an upper bound on it.
#
# `frozenset()` is a real, common answer -- 35 of the 88 fields have no
# in-scope direct `IORef` write at all, either because nothing writes
# them after `Engine.Core.Init` seeds them, because their only writers
# are SS6.1 permanent modules (D-4), or because they are mutated through
# a queue/`TVar`/opaque handle the scan deliberately does not follow
# (D-5, and the residue report).
#
# Adding an entry is a deliberate act, not a maintenance edit: it
# declares that a capability-narrowed module now holds write authority
# over that field. Removing one is what a narrowing migration owes the
# gate. Either way the audit names the exact module and field, so the
# edit is mechanical once the decision is made -- see
# docs/engineenv_capability_inventory.md SS6.4.
# docs/engineenv_capability_inventory.md SS6.5's shadow exemptions
# (issue #1892 requirement 7): `{(module, EngineEnv field): reason}` for
# the one case the two shape rules cannot separate -- a module that
# locally binds a name matching an accessor AND applies it to a handle.
#
# __Empty, and expected to stay that way.__ The alternative was a
# lexical scope analysis of Haskell's binding forms; measured against
# the live tree it changed the answer at NONE of the mutation sites,
# while costing eight review rounds of findings, because the forms are
# many and the analysis is only ever as complete as the last one
# someone thought of. The one near-miss in the tree,
# `src/Unit/Thread/Movement.hs`'s `utsRef` parameter, needs no entry:
# that module imports `Engine.Core.State` for the `EngineEnv` TYPE
# alone, so the name is not in scope as an accessor there.
#
# An entry suppresses exactly its own module/field pair and nothing
# else, must name a live field, must carry a real reason, and fails
# once it stops suppressing anything -- `audit_shadow_exemptions`
# checks all four.
SHADOW_EXEMPTIONS: dict[tuple[str, str], str] = {}

CAPABILITY_WRITER_MODULES: dict[str, frozenset[str]] = {
    "engineConfig": frozenset(),
    "engineStateRef": frozenset(),
    "videoConfigRef": frozenset({
        "Engine.Scripting.Lua.API.Config",
        "Engine.Scripting.Lua.Message.Video",
    }),
    "windowSizeRef": frozenset({
        "Engine.Graphics.Window.GLFW",
        "Engine.Input.Thread.Dispatch",
        "Engine.Scripting.Lua.Message.Video",
    }),
    "windowPosRef": frozenset({
        "Engine.Graphics.Window.GLFW",
        "Engine.Scripting.Lua.Message.Video",
    }),
    "windowStateRef": frozenset({
        "Engine.Graphics.Window.GLFW",
        "Engine.Scripting.Lua.Message.Video",
    }),
    "framebufferSizeRef": frozenset({
        "Engine.Graphics.Window.GLFW",
        "Engine.Input.Thread.Dispatch",
        "Engine.Scripting.Lua.Message.Video",
    }),
    "framebufferMinimizeGenRef": frozenset({"Engine.Input.Thread.Dispatch"}),
    "fpsRef": frozenset(),
    "brightnessRef": frozenset({"Engine.Scripting.Lua.Message.Video"}),
    "pixelSnapRef": frozenset({
        "Engine.Scripting.Lua.API.Config",
        "Engine.Scripting.Lua.Message.Video",
    }),
    "textureFilterRef": frozenset({
        "Engine.Scripting.Lua.API.Config",
        "Engine.Scripting.Lua.Message.Video",
    }),
    "inputQueue": frozenset(),
    "inputBarrierNextRef": frozenset(),
    "inputBarrierRef": frozenset(),
    "loggerRef": frozenset(),
    "luaToEngineQueue": frozenset(),
    "luaQueue": frozenset(),
    "lifecycleRef": frozenset({
        "Combat.Thread",
        "Engine.Input.Thread",
        "Engine.Loop.Mode",
        "Engine.Scripting.Lua.API.Core",
        "Sim.Thread",
        "Unit.Thread",
        "World.Thread",
    }),
    "assetPoolRef": frozenset(),
    "textureNameRegistryRef": frozenset(),
    "nextObjectIdRef": frozenset(),
    "nextItemInstanceIdRef": frozenset(),
    "fontCacheRef": frozenset(),
    "inputStateRef": frozenset({"Engine.Input.Thread.Dispatch"}),
    "keyBindingsRef": frozenset({"Engine.Scripting.Lua.API.Keybinds"}),
    "currentKeyDownRef": frozenset(),
    "textBuffersRef": frozenset({"Engine.Scripting.Lua.Message.Scene"}),
    "cameraRef": frozenset({
        "Engine.Scripting.Lua.API.Camera",
        "World.Render",
        "World.Thread.Command.Init",
    }),
    "uiCameraRef": frozenset({"Engine.Graphics.Vulkan.Recreate"}),
    "uiManagerRef": frozenset({
        "Engine.Input.Thread.Char",
        "Engine.Input.Thread.Keyboard",
        "Engine.Input.Thread.Mouse",
        "Engine.Scripting.Lua.API.Config",
        "Engine.Scripting.Lua.API.UI.Element",
        "Engine.Scripting.Lua.API.UI.Focus",
        "Engine.Scripting.Lua.API.UI.Hierarchy",
        "Engine.Scripting.Lua.API.UI.Page",
        "Engine.Scripting.Lua.API.UI.Presentation",
        "Engine.Scripting.Lua.API.UI.Property",
        "Engine.Scripting.Lua.API.UI.TextInput",
        "Engine.Scripting.Lua.API.UI.Tooltip",
        "UI.Render",
        "UI.Tooltip.State",
    }),
    "focusManagerRef": frozenset({"Engine.Scripting.Lua.API.ShellFocus"}),
    "worldManagerRef": frozenset({
        "Engine.Scripting.Lua.API.World.Lifecycle",
        "World.Thread.Command.Basic",
        "World.Thread.Command.Init",
        "World.Thread.Command.UI",
    }),
    "hudActivePageRef": frozenset({"World.Thread.Cursor"}),
    "loadStatusRef": frozenset(),
    "pendingLoadRef": frozenset(),
    "worldQueue": frozenset(),
    "sunAngleRef": frozenset({
        "Engine.Scripting.Lua.API.World.Clock",
        "World.Thread.Time",
    }),
    "worldPreviewRef": frozenset({
        "Engine.Scripting.Lua.Message.WorldTexture",
        "World.Thread.Command.Init",
    }),
    "worldPreviewGenerationRef": frozenset({"World.Thread.Command.Init"}),
    "zoomAtlasDataRef": frozenset({
        "Engine.Scripting.Lua.Message.WorldTexture",
        "World.Thread.Command.Init",
    }),
    "screenshotRequestQueue": frozenset(),
    "worldQuadsRef": frozenset({
        "World.Thread",
        "World.Thread.Command.Basic",
    }),
    # #1921. Written only by the world thread, and only through
    # `Engine.Scene.Stats`'s `publishSceneStats`/`clearSceneStats`, which
    # take the ref as a parameter -- so no module writes it DIRECTLY and
    # the empty set is what this direct-write scan can honestly assert.
    # The callers are `World.Render.updateWorldTiles` (one publication
    # per completed pass) and `World.Thread.Command.Basic`'s two
    # teardown handlers (clear), both named in the SS5 row.
    "sceneStatsRef": frozenset(),
    "textureSystemRef": frozenset({
        "Engine.Asset.Manager",
        "Engine.Graphics.Vulkan.Init",
        "Engine.Scripting.Lua.Message.Texture",
        "Engine.Scripting.Lua.Message.Video",
        "Engine.Scripting.Lua.Message.WorldTexture",
        "World.Render.BloodQuads",
    }),
    "samplerCacheRef": frozenset(),
    "textureSizeRef": frozenset({
        "Engine.Scripting.Lua.Message.Texture",
        "World.Render.BloodQuads",
    }),
    "bloodDisposeQueue": frozenset(),
    "defaultFaceMapSlotRef": frozenset({"Engine.Graphics.Vulkan.Init"}),
    "floraCatalogRef": frozenset(),
    "materialRegistryRef": frozenset({
        "Engine.Scripting.Lua.API.YamlTextures",
        "World.Thread.Command.Init",
    }),
    "unitManagerRef": frozenset({
        "Combat.Resolution",
        "Combat.Resolution.Wear",
        "Combat.Wounds.Tick",
        "Engine.Scripting.Lua.API.Craft.Execute",
        "Engine.Scripting.Lua.API.Equipment.Accessory",
        "Engine.Scripting.Lua.API.Equipment.Slot",
        "Engine.Scripting.Lua.API.Items.Ground",
        "Engine.Scripting.Lua.API.Power",
        "Engine.Scripting.Lua.API.Units.Cargo",
        "Engine.Scripting.Lua.API.Units.Combat",
        "Engine.Scripting.Lua.API.Units.Equipment",
        "Engine.Scripting.Lua.API.Units.Inventory",
        "Engine.Scripting.Lua.API.Units.Medical",
        "Engine.Scripting.Lua.API.Units.Selection",
        "Engine.Scripting.Lua.API.Units.Spawn",
        "Engine.Scripting.Lua.API.Units.Stats",
        "Engine.Scripting.Lua.API.Units.Survival",
        "Engine.Scripting.Lua.API.Units.Transfer",
        "Engine.Scripting.Lua.API.Units.Yaml",
        "Unit.Selection",
        "Unit.Thread",
        "Unit.Thread.Command.Lifecycle",
        "Unit.Thread.Command.Pose",
        "Unit.Thread.Command.Spawn",
        "Unit.Thread.Movement",
        "World.Thread.ItemTemp",
    }),
    "unitQueue": frozenset(),
    "utsRef": frozenset(),
    "statRNGRef": frozenset({
        "Combat.Resolution",
        "Combat.Wounds.Tick",
        "Engine.Scripting.Lua.API.Forage.Harvest",
        "Engine.Scripting.Lua.API.Units.Medical",
        "Engine.Scripting.Lua.API.Units.Stats",
        "Unit.Thread.Command.Spawn",
        "Unit.Thread.Movement.Climb",
    }),
    "buildingManagerRef": frozenset({
        "Building.Thread.Command",
        "Engine.Scripting.Lua.API.Buildings.Progress",
        "Engine.Scripting.Lua.API.Buildings.Selection",
        "Engine.Scripting.Lua.API.Buildings.Spawn",
        "Engine.Scripting.Lua.API.Buildings.Yaml",
        "Engine.Scripting.Lua.API.Power",
        "Engine.Scripting.Lua.API.Units.Cargo",
        "Engine.Scripting.Lua.API.Units.Transfer",
        "World.Thread.ItemTemp",
    }),
    "texPaletteRef": frozenset({"Engine.Scripting.Lua.API.Structure"}),
    "texPaletteHandlesRef": frozenset({"Engine.Scripting.Lua.API.Structure"}),
    "structureWallCatalogRef": frozenset({"Engine.Scripting.Lua.API.Structure"}),
    "structureArtCatalogRef": frozenset({"Engine.Scripting.Lua.API.StructureArt"}),
    "buildingQueue": frozenset(),
    "combatQueue": frozenset(),
    "combatEventsRef": frozenset({
        "Combat.Resolution.Events",
        "Combat.Wounds.Tick",
        "Engine.Scripting.Lua.API.Combat",
    }),
    "injuryEventsRef": frozenset({"Engine.Scripting.Lua.API.Combat"}),
    "thoughtEventsRef": frozenset({"Engine.Scripting.Lua.API.Combat"}),
    "actionOutcomeRef": frozenset({"Engine.Scripting.Lua.API.ActionOutcome"}),
    "buildingGhostRef": frozenset({"Engine.Scripting.Lua.API.Buildings.Spawn"}),
    "worldGenConfigRef": frozenset({"Engine.Scripting.Lua.API.World.GenConfig"}),
    "pathingConfigRef": frozenset(),
    "simQueue": frozenset(),
    "enginePausedRef": frozenset({"World.Pause"}),
    "playerIntentGenRef": frozenset(),
    "enginePauseGenRef": frozenset({"World.Pause"}),
    "gameTimeRef": frozenset({"Unit.Thread"}),
    "saveBarrierRef": frozenset(),
    "inputThreadActiveRef": frozenset({"Engine.Input.Thread"}),
    "lastSaveTimeRef": frozenset(),
    "itemManagerRef": frozenset(),
    "equipmentClassManagerRef": frozenset({"Engine.Scripting.Lua.API.Equipment.Class"}),
    "substanceManagerRef": frozenset({"Engine.Scripting.Lua.API.Substance"}),
    "infectionManagerRef": frozenset({"Engine.Scripting.Lua.API.Infection"}),
    "recipeManagerRef": frozenset({"Engine.Scripting.Lua.API.Craft.Recipe"}),
    "locationDefsRef": frozenset({"Engine.Scripting.Lua.API.Locations"}),
    "lootTableRegistryRef": frozenset({"Engine.Scripting.Lua.API.LootTables"}),
    "tutorialRegistryRef": frozenset({"Engine.Scripting.Lua.API.Tutorial"}),
    "eventStoreRef": frozenset(),
    "notificationCfgRef": frozenset({"Engine.Scripting.Lua.API.PlayerEvent"}),
    "notificationOrder": frozenset(),
    "popupQueueRef": frozenset(),
}


_HS_IDENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_']*")
_IMPORT_WILDCARD_RE = re.compile(
    r"([A-Z][A-Za-z0-9_']*)\s*\(\s*\.\.\s*\)")
# An equation left-hand side (top-level, `where` or `let` alike),
# anchored at the start of its line and forbidding `{` in the parameter
# text, so a record construction or update -- `env { fooRef = r }`, or a
# continuation line starting with `,` or `{` -- is never mistaken for
# one. Group 1 is the bound name, group 2 its parameter text.
# An import's `qualified` keyword, module name, and optional `as`
# alias, in either the classic or the `ImportQualifiedPost` order. All
# three matter, and each is a language rule rather than a preference:
# `qualified` removes the UNQUALIFIED spelling from scope entirely, and
# an `as` alias REPLACES the module name as the qualifier instead of
# joining it.
# The four binding forms, each anchored at the start of ONE line and
# each paired below with the layout region it actually scopes over.
# `_BINDING_LHS_RE` forbids `{` in the parameter text so a record
# construction or update (`env { fooRef = r }`) is never read as an
# equation; a continuation line opening with `,` or `{` matches
# nothing at all.
_IMPORT_HIDING_RE = re.compile(r"(?<![A-Za-z0-9_'])hiding(?![A-Za-z0-9_'])")
_IMPORT_DECL_RE = re.compile(
    r"^import\s+(?P<pre>qualified\s+)?(?P<module>[A-Z][A-Za-z0-9_.']*)"
    r"(?P<post>\s+qualified\b)?"
    r"(?:\s+as\s+(?P<alias>[A-Z][A-Za-z0-9_.']*))?")


class Token(NamedTuple):
    """One identifier or single-character punctuation token.

    `offset` is the character position, which is what lets ADJACENCY be
    tested: `env.fieldOne` and `env . fieldOne` tokenize identically and
    mean entirely different things, and only the gap between them says
    which."""
    kind: str   # "id" | "punc"
    text: str
    line: int   # 1-based
    offset: int


class ImportDecl(NamedTuple):
    """One `import` declaration, reduced to what name resolution needs.

    `qualifier` is the prefix a QUALIFIED use must carry -- the `as`
    alias when there is one, otherwise the module's own name.
    `qualified` says whether the UNQUALIFIED spelling is in scope at
    all: `import qualified M as N` puts only `N.f` in scope, never `f`,
    which is why the two flags cannot be collapsed into one map.
    `names` is `None` only for an import that enumerates nothing at all
    -- a bare import, or one carrying a `hiding` clause, whose excluded
    names are in `hidden`. Otherwise `names` holds the plainly listed
    symbols and `wildcards` the TYPES imported as `T(..)`: that form
    brings in `T`'s own selectors and nobody else's, so
    `Engine.Core.State (WindowState(..))` does not put an `EngineEnv`
    field in scope."""
    module: str
    qualified: bool
    qualifier: str
    names: frozenset[str] | None
    hidden: frozenset[str]
    wildcards: frozenset[str]


class Occurrence(NamedTuple):
    """One capability-accessor use the direct-write scan cannot
    attribute. Ordered path-first so the report is deterministic."""
    relpath: str
    line: int
    accessor: str
    field: str
    module: str


def tokenize_haskell(text: str) -> list[Token]:
    """Identifier / single-character-punctuation tokens over
    ALREADY-comment-stripped Haskell source, with string and character
    literals consumed whole and dropped.

    Whitespace -- newlines included -- is skipped, which is what makes
    every consumer below scan complete EXPRESSIONS rather than
    individual lines: a mutation whose accessor argument sits on the
    next line (`Engine.Input.Thread.Dispatch`'s `atomicModifyIORef'` of
    `rvFramebufferMinimizeGenRef`, `Engine.Scripting.Lua.API.StructureArt`'s
    `rhStructureArtCatalogRef` write) is one token sequence here.
    Numeric literals degrade into punctuation tokens, which is harmless:
    nothing downstream matches on them."""
    tokens: list[Token] = []
    i = 0
    line = 1
    n = len(text)
    while i < n:
        ch = text[i]
        if ch == "\n":
            line += 1
            i += 1
            continue
        if ch.isspace():
            i += 1
            continue
        ident = _HS_IDENT_RE.match(text, i)
        if ident:
            # Haskell lexes `Mod.name` with NO intervening space as one
            # QUALIFIED name whenever the prefix is a conid, so
            # `State.fieldOne` must not degrade into `State`, `.`,
            # `fieldOne` -- that is how a qualified write would slip the
            # scan. Composition (`f . g`, and `f.g` on a lowercase head)
            # keeps its own tokens.
            end = ident.end()
            component = ident.group(0)
            while component[:1].isupper() and end < n and text[end] == ".":
                nxt = _HS_IDENT_RE.match(text, end + 1)
                if nxt is None:
                    break
                component = nxt.group(0)
                end = nxt.end()
            tokens.append(Token("id", text[i:end], line, i))
            i = end
            continue
        if ch == '"':
            j = i + 1
            while j < n and text[j] != '"':
                if text[j] == "\\":
                    # The ESCAPED character may itself be a newline: a
                    # Haskell string gap is a backslash, whitespace
                    # (newlines included) and another backslash. Missing
                    # it reports every later token a line too early,
                    # and a residue entry or a blocking site names the
                    # wrong source line.
                    j += 1
                    if j < n and text[j] == "\n":
                        line += 1
                elif text[j] == "\n":
                    line += 1
                j += 1
            i = min(j + 1, n)
            continue
        if ch == "'":
            # An identifier's trailing prime is already consumed above,
            # so a `'` reaching here opens a character literal (or is
            # stray punctuation).
            literal = _CHAR_LITERAL_RE.match(text, i)
            if literal:
                i = literal.end()
                continue
        tokens.append(Token("punc", ch, line, i))
        i += 1
    return tokens


def strip_import_declarations(text: str) -> str:
    """`text` with every top-level `import` declaration blanked (line
    count preserved). An import list names accessors -- often the very
    accessor a module writes -- and naming one is not using one."""
    for chunk in _import_chunks(text):
        text = text.replace(chunk, "\n" * chunk.count("\n"), 1)
    return text


def prepared_source(text: str) -> str:
    """Comment-stripped, import-blanked source: what every scan below
    reads. Haddock and `--` commentary can name any accessor without
    counting as a use."""
    return strip_import_declarations(_strip_haskell_comments(text))


def parse_imports(source_text: str) -> list[ImportDecl]:
    """Every `import` declaration in `source_text`, as `ImportDecl`s.

    A LIST, not a map keyed by module: one module is legitimately
    imported twice with different terms (`import Data.Map (Map)` beside
    `import qualified Data.Map as M`), and each declaration carries its
    own answer about which spellings it admits.

    This is what decides whether an identifier at a write site can even
    BE the accessor -- in both directions. `src/Unit/Thread/Movement.hs`
    writes a local `utsRef` parameter while importing
    `Engine.Core.State` for the `EngineEnv` TYPE alone, so the identical
    name there is not the field; and under
    `import qualified Engine.Core.State as State` only `State.fieldOne`
    is the field, while a bare `fieldOne` is necessarily something the
    module defined itself. A `hiding` clause is recorded rather than
    waved through, for the same reason: a module that hides `fieldOne`
    and defines its own is not writing the field."""
    declarations: list[ImportDecl] = []
    for chunk in _import_chunks(_strip_haskell_comments(source_text)):
        head = _IMPORT_DECL_RE.match(chunk)
        if not head:
            continue
        module = head.group("module")
        alias = head.group("alias")
        qualified = bool(head.group("pre") or head.group("post"))
        body = chunk[head.end():]
        hiding = _IMPORT_HIDING_RE.search(body)
        hidden: frozenset[str] = frozenset()
        wildcards = frozenset(match.group(1)
                              for match in _IMPORT_WILDCARD_RE.finditer(body))
        if hiding is not None:
            # Everything EXCEPT the listed names. A `hiding (T(..))`
            # names the type, not its fields, so a field hidden that way
            # is not recorded -- which can only leave a write attributed
            # (a loud violation), never hide one.
            hidden = frozenset(_HS_IDENT_RE.findall(body[hiding.end():]))
            names: frozenset[str] | None = None
        elif "(" not in body:
            names = None
        else:
            # A `T(..)` group is recorded as a wildcard on `T`, never as
            # a plain name, so it can only grant `T`'s own selectors.
            names = frozenset(_HS_IDENT_RE.findall(
                _IMPORT_WILDCARD_RE.sub(" ", body)))
        declarations.append(ImportDecl(module, qualified, alias or module,
                                       names, hidden, wildcards))
    return declarations


def imports_name(declarations: list[ImportDecl], module: str, name: str,
                 qualifier: str, owner_type: str | None = None) -> bool:
    """True iff `declarations` put `module`'s `name` in scope under the
    spelling used -- `qualifier` empty for a bare use, otherwise the
    prefix that was written. A qualified use must match a declaration's
    own qualifier; an unqualified one must find a declaration that is
    not `qualified` at all.

    `owner_type` is the record `name` is a selector of. A `T(..)` group
    grants only `T`'s selectors, so an import list carrying some OTHER
    type's wildcard does not put this one in scope. `None` means the
    owner is unknown, in which case any wildcard is accepted -- the
    direction that keeps a write visible."""
    for declaration in declarations:
        if declaration.module != module:
            continue
        if qualifier:
            if declaration.qualifier != qualifier:
                continue
        elif declaration.qualified:
            continue
        if name in declaration.hidden:
            continue
        if declaration.names is None or name in declaration.names:
            return True
        if declaration.wildcards and (owner_type is None
                                      or owner_type in declaration.wildcards):
            return True
    return False


def capability_accessor_map(sources: dict[str, str], live_fields: list[str]
                            ) -> dict[str, tuple[tuple[str, str, str], ...]]:
    """`{capability accessor: ((field, defining module, record type), ...)}`
    for every `Engine.Core.Capability.*` record, derived from the LIVE
    projections (`parse_projection_bindings`) rather than a second
    checked-in list, so this canonicalization cannot drift from the
    records it describes.

    Each accessor maps to a TUPLE of candidates, not one, because a
    selector name is only unique within its own module: two capability
    records may both export `sharedRef`, and a consumer that imports one
    of them qualified is writing THAT one's field. Collapsing them would
    let the wrong owner win the scope test and drop a real write. The
    candidates are sorted by owner, so resolution is deterministic.

    Duplicate full/view accessors resolve independently: `Render`'s
    `rcVideoConfigRef` and `RenderView`'s `rvVideoConfigRef` are separate
    keys canonicalizing onto the same `videoConfigRef` field. A binding
    whose right-hand side is not a live `EngineEnv` field is skipped
    rather than invented -- `audit_save_load_projection` and the
    boundary checks are where a mis-bound projection is caught."""
    fields = set(live_fields)
    candidates: dict[str, set[tuple[str, str, str]]] = {}
    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        if not module.startswith(CAPABILITY_MODULE_PREFIX):
            continue
        match = re.search(
            r"^(to[A-Za-z0-9_']*)\s*(?:∷|::)\s*"
            r"(?:[A-Z][A-Za-z0-9_']*\.)*EngineEnv\s*(?:→|->)\s*"
            r"([A-Z][A-Za-z0-9_']*)", text, re.MULTILINE)
        if not match:
            continue
        record = match.group(2)
        for capability_field, accessor in parse_projection_bindings(
                text, match.group(1)).items():
            if accessor in fields:
                candidates.setdefault(capability_field, set()).add(
                    (accessor, module, record))
    return {name: tuple(sorted(owners, key=lambda entry: entry[1]))
            for name, owners in candidates.items()}


def resolve_primitive(declarations: list[ImportDecl], name: str) -> str | None:
    """The handle-consuming primitive `name` denotes here, or `None`.

    Bare or qualified, the base name must be one of the recognized
    primitives AND must reach this module from that primitive's OWN
    defining module (`ACCESS_PRIMITIVE_MODULES`) under that exact
    spelling. A module-local `writeIORef`, or `Other.writeIORef` from
    an unrelated module, is a different function; attributing its
    argument would invent a write out of code that mutates no `IORef`
    at all. `Engine.Core.ReadOnlyRef`'s read goes through the identical
    rule, not a looser second path.

    __A TOP-LEVEL homonym is covered by the same rule, because Haskell
    makes it so.__ Defining `writeIORef` beside an unqualified
    `import Data.IORef` is an ambiguous occurrence at every use site --
    that module does not compile -- so the only spellings that reach
    here are the ones this test already decides: the import names the
    primitive, or it does not (`hiding (writeIORef)`, an explicit list
    without it, `qualified`), and a local definition then stands alone.

    A LOCAL binding -- a `let`, a `where`, a lambda parameter -- can
    legally shadow the imported primitive, and that is the mirror of an
    accessor shadowed the same way. Both are `SHADOW_EXEMPTIONS`'
    business, by requirement 7's deliberate choice: the exemption
    suppresses the module/field pair whatever name was shadowed to
    produce it, and no scope analysis is performed for either."""
    qualifier, _, base = name.rpartition(".")
    owner = ACCESS_PRIMITIVE_MODULES.get(base)
    if owner is None:
        return None
    if not imports_name(declarations, owner, base, qualifier):
        return None
    return base


def _applied_head(tokens: list[Token], head: int) -> int | None:
    """`head` if the accessor at that index is APPLIED to something,
    else `None`.

    Parentheses around the accessor ITSELF change nothing --
    `writeIORef ((fieldOne) env) 1` applies exactly what
    `writeIORef (fieldOne env) 1` does -- so the closers balancing the
    openers written directly before it are stepped over before the next
    token is judged. Exactly that many are consumed and no more, so a
    genuinely unapplied `(fieldOne)` still ends at its own closer
    instead of reading whatever follows the group it sits in."""
    peeled = 0
    k = head - 1
    while k >= 0 and tokens[k].kind == "punc" and tokens[k].text == "(":
        peeled += 1
        k -= 1
    j = head + 1
    while (peeled > 0 and j < len(tokens) and tokens[j].kind == "punc"
           and tokens[j].text == ")"):
        peeled -= 1
        j += 1
    if j >= len(tokens):
        return None
    following = tokens[j]
    applied = (following.kind == "id"
               or (following.kind == "punc"
                   and following.text in ("(", "[", "$")))
    return head if applied else None


def _skip_type_atom(tokens: list[Token], index: int) -> int:
    """Index just past the type atom at `index` -- one identifier, or
    one balanced `(`/`[` group. Anything else is left where it is, so a
    shape this does not understand stops the walk instead of consuming
    the value argument."""
    if index >= len(tokens):
        return index
    token = tokens[index]
    if token.kind == "id":
        return index + 1
    if token.kind == "punc" and token.text in ("(", "["):
        depth = 0
        while index < len(tokens):
            current = tokens[index]
            if current.kind == "punc" and current.text in ("(", "["):
                depth += 1
            elif current.kind == "punc" and current.text in (")", "]"):
                depth -= 1
                if depth == 0:
                    return index + 1
            index += 1
    return index


# Keywords lex as identifiers but apply to nothing: `else
# atomicModifyIORef' (...) ...` is a head-position use, and
# `src/Unit/Thread/Movement/Climb.hs:86` is exactly that.
_HASKELL_KEYWORDS = frozenset({
    "case", "do", "else", "if", "in", "let", "of", "then", "where",
})


def after_operator_section(tokens: list[Token], index: int) -> bool:
    """True if `tokens[index]` is directly preceded by an OPERATOR
    SECTION -- a parenthesized group holding nothing but punctuation,
    as in `($) writeIORef (fieldOne env) value` or `(.) f g`.

    Applying an operator prefix that way is ordinary Haskell, and what
    the section does with its arguments is exactly what a textual scan
    cannot know: `($)` applies them, `(.)` composes them, and the two
    have opposite consequences for whether a write happens here. So the
    site is neither attributed nor waved through -- it is
    unclassifiable, and requirement 6 reports it. Recognizing each
    operator individually is the open-ended path this arc rejects."""
    if index == 0:
        return False
    closing = tokens[index - 1]
    if closing.kind != "punc" or closing.text != ")":
        return False
    depth, j = 0, index - 1
    while j >= 0:
        token = tokens[j]
        if token.kind == "punc" and token.text == ")":
            depth += 1
        elif token.kind == "punc" and token.text == "(":
            depth -= 1
            if depth == 0:
                break
        j -= 1
    if j < 0:
        return False
    # An empty group vacuously qualifies, which is harmless: `()` can
    # never be applying a primitive in code that compiles.
    return all(tokens[k].kind == "punc" for k in range(j + 1, index - 1))


def line_indents(code: str) -> list[int | None]:
    """Indent column per 1-BASED line (index 0 unused), `None` for a
    blank line. `in_head_position` reads it to tell a continuation from
    a new statement."""
    return [None] + [None if not line.strip()
                     else len(line) - len(line.lstrip())
                     for line in code.split("\n")]


def in_head_position(tokens: list[Token], index: int,
                     indents: list[int | None] | None = None) -> bool:
    """True unless something is plainly APPLYING to `tokens[index]`.

    `withLogging writeIORef (fieldOne env) 1` hands the primitive to
    `withLogging`; reading the tokens after it as its own arguments
    invents a write, and hides the accessor's pass-on residue entry
    behind a phantom inline use. What can apply to it is an identifier
    or a closing bracket -- but a newline does not end an application,
    and layout does not end a statement with any token, so the token
    alone cannot decide it:

    * a KEYWORD applies to nothing, wherever it sits, which is what
      makes `else writeIORef (...) ...` and the `do` opening a block
      both head position;
    * on the SAME line, an identifier or closing bracket is applying;
    * across lines, LAYOUT decides. A continuation is indented past the
      line that opened the expression (`withLogging` on one line, the
      primitive indented under it), while a sibling statement starts at
      the same column or further left.

    Without `indents` the across-lines case answers True, which keeps a
    write visible rather than dropping it silently."""
    if index == 0:
        return True
    previous = tokens[index - 1]
    if previous.kind == "id":
        if previous.text in _HASKELL_KEYWORDS:
            return True
    elif not (previous.kind == "punc" and previous.text in (")", "]")):
        return True
    if previous.line == tokens[index].line:
        return False
    if indents is None:
        return True
    # A token's own line is never blank, so `or 0` is a totality guard
    # rather than a branch worth its own case.
    here = indents[tokens[index].line] or 0
    there = indents[previous.line] or 0
    return here <= there


def _past_primitive_parentheses(tokens: list[Token], index: int) -> int:
    """Index of the first token after `tokens[index]` that is not a
    `)` closing a `(` written immediately before the primitive.

    `(writeIORef) (accessor handle) v` is the same application as the
    unparenthesized form -- parentheses around a function name change
    nothing -- so the closers have to be stepped over before the value
    argument can be found.

    Two conditions keep that from inventing an application. Only closers
    balanced by openers DIRECTLY preceding the primitive are consumed,
    so `foo (writeIORef ref v)` is untouched; and the outermost of those
    openers must itself sit in head position -- nothing applying to it
    on its left -- because in `withLogging (writeIORef) (accessor
    handle) v` the primitive is an ARGUMENT being passed on, not the
    function being applied, and what that callee does with it is exactly
    the indirection D-5 reports rather than attributes."""
    openers = 0
    k = index - 1
    while k >= 0 and tokens[k].kind == "punc" and tokens[k].text == "(":
        openers += 1
        k -= 1
    if openers and k >= 0 and (tokens[k].kind == "id"
                               or tokens[k].text in (")", "]")):
        openers = 0
    j = index + 1
    while j < len(tokens) and tokens[j].kind == "punc":
        if tokens[j].text == "@":
            # `(writeIORef @Int) (accessor handle) v` -- the type
            # application sits INSIDE the parentheses, so it has to be
            # stepped over before the closer can be.
            j = _skip_type_atom(tokens, j + 1)
            continue
        if openers > 0 and tokens[j].text == ")":
            openers -= 1
            j += 1
            continue
        break
    return j


def _infix_left_operand_head(tokens: list[Token], index: int) -> int | None:
    """Token index of the head identifier of a BACKTICKED primitive's
    left operand -- ``(accessor handle) `writeIORef` value`` -- or
    `None`.

    Haskell lets any two-argument function be written infix, so this is
    the same direct write as the prefix form with the arguments swapped,
    and a scan that only looked to the RIGHT of the primitive would miss
    it silently. `tokens[index]` is the primitive itself, so its
    backticks sit at `index - 1` and `index + 1`. The operand must be a
    application, for exactly the reason `_first_argument_head` requires
    one. It need not be PARENTHESIZED, since a backtick operator binds
    looser than application: ``fieldOne env `writeIORef` 1`` is the
    same write. `_operand_head` finds the head either way, so a
    trailing `)` that closes an ARGUMENT
    (``fkFieldOne (toFakeCapability env) `writeIORef` v``) is not
    mistaken for one closing the whole operand."""
    if (index == 0 or tokens[index - 1].kind != "punc"
            or tokens[index - 1].text != "`"):
        return None
    if (index + 1 >= len(tokens)
            or tokens[index + 1].kind != "punc"
            or tokens[index + 1].text != "`"):
        return None
    head = _operand_head(tokens, index - 2)
    if head is None or head >= index - 1 or tokens[head].kind != "id":
        return None
    return _applied_head(tokens, head)


def _operand_head(tokens: list[Token], last: int) -> int | None:
    """Index of the head identifier of the application ENDING at
    `tokens[last]`, or `None`.

    Walks left over ATOMS only -- an identifier, or a balanced `(`/`[`
    group -- so an operator, a `$`, a comma or an equals ends the
    operand where it stands. `(fieldOne env)`, `fieldOne env` and
    `fkFieldOne (toFakeCapability env)` therefore all resolve to their
    own head, which is the point: whether the operand is parenthesized
    says nothing about where its head is, and a trailing `)` may be
    closing an ARGUMENT rather than the whole operand.

    When the walk ends having consumed nothing but one group, that group
    IS the operand, so its head lies inside it and the search descends
    (peeling any redundant nesting on the way)."""
    head: int | None = None
    group_open: int | None = None
    j = last
    while j >= 0:
        token = tokens[j]
        if token.kind == "id":
            head, group_open = j, None
            j -= 1
            continue
        if token.kind == "punc" and token.text in (")", "]"):
            depth = 0
            k = j
            while k >= 0:
                current = tokens[k]
                if current.kind == "punc" and current.text in (")", "]"):
                    depth += 1
                elif current.kind == "punc" and current.text in ("(", "["):
                    depth -= 1
                    if depth == 0:
                        break
                k -= 1
            if k < 0:
                break
            head, group_open = None, k
            j = k - 1
            continue
        break
    if head is not None:
        return head
    if group_open is None:
        return None
    inner = group_open + 1
    while (inner < len(tokens) and tokens[inner].kind == "punc"
           and tokens[inner].text == "("):
        inner += 1
    return inner if inner < len(tokens) else None


def _opens_record_dot(tokens: list[Token], index: int) -> bool:
    """True if `tokens[index]` is an identifier IMMEDIATELY followed by
    `.` and another identifier.

    Only a lowercase head can reach this: `tokenize_haskell` already
    merges `Mod.name` into one qualified token, so an uppercase head is
    never left with a separate `.` beside it.

    Written without spaces that is `OverloadedRecordDot` field access
    (`env.fieldOne`); written with them it is composition. The scan can
    read neither as an accessor application, so rather than take the
    left operand as the argument head -- which quietly makes
    `modifyIORef' (env.fieldOne) id` a non-write -- the site is left
    unclassifiable and requirement 6 reports it. No such site exists in
    this tree: the extension is not enabled anywhere in it."""
    if tokens[index].kind != "id":
        return False
    dot, name = index + 1, index + 2
    if name >= len(tokens):
        return False
    return (tokens[dot].kind == "punc" and tokens[dot].text == "."
            and tokens[name].kind == "id"
            and tokens[dot].offset == tokens[index].offset
            + len(tokens[index].text)
            and tokens[name].offset == tokens[dot].offset + 1)


def first_argument_token(tokens: list[Token], index: int
                         ) -> tuple[int | None, bool]:
    """`(index of the first argument's head identifier, was a grouping
    token consumed)` for the mutation primitive at `tokens[index]`.

    The head is returned whether or not it is APPLIED, because naming it
    is what `classify_mutation_site` needs and being applied is what
    `_first_argument_head` needs. `grouped` says whether anything that
    OPENS an argument -- a `(`, a `$`, a `$!`, a visible type
    application -- was stepped over on the way, which is what separates
    "an argument is being formed here and I cannot read it" from "this
    primitive is not applied to anything here"."""
    j = _past_primitive_parentheses(tokens, index)
    grouped = j != index + 1
    while j < len(tokens) and tokens[j].kind == "punc":
        token = tokens[j]
        if token.text == "@":
            # A visible type application (`writeIORef @Int (ref) v`,
            # legal under GHC2024's default `TypeApplications`) is not
            # the value argument. Skip its type atom -- an identifier,
            # or one balanced group -- and keep looking.
            j = _skip_type_atom(tokens, j + 1)
            grouped = True
            continue
        if token.text in ("$", "("):
            grouped = True
            j += 1
            # `$!` is the strict sibling of `$` and groups identically;
            # the tokenizer splits it, so its `!` is stepped over here.
            if (token.text == "$" and j < len(tokens)
                    and tokens[j].kind == "punc" and tokens[j].text == "!"):
                j += 1
            continue
        break
    if j < len(tokens) and tokens[j].kind == "id":
        if _opens_record_dot(tokens, j):
            return None, True
        return j, grouped
    return None, grouped


def _first_argument_head(tokens: list[Token], index: int) -> int | None:
    """Token index of the head identifier of `tokens[index]`'s first
    argument, when that argument is an APPLICATION -- `prim (accessor
    handle) ...` or `prim $ accessor handle`. Otherwise `None`.

    __Requiring the application is a type argument, not a heuristic.__
    Every accessor here projects out of a handle -- `EngineEnv -> IORef
    a`, or `XCapability -> IORef a` -- so it cannot itself BE the
    `IORef` a mutation primitive takes. A BARE identifier in that
    position therefore never denotes the accessor; it denotes some
    local binding that happens to share its name, exactly like
    `src/Unit/Thread/Movement.hs`'s `utsRef` parameter. That is decided
    by SHAPE, without modelling Haskell's binding forms at all -- see
    `SHADOW_EXEMPTIONS` for the one residual case and why it is a
    checked-in list rather than a scope analysis."""
    head, grouped = first_argument_token(tokens, index)
    if head is None or not grouped:
        return None
    return _applied_head(tokens, head)


class MutationSite(NamedTuple):
    """One mutation-primitive occurrence and what the scan made of it.

    `kind` is exactly one of:

    * `"write"` -- an APPLIED, in-scope, non-exempt accessor: attributed
      to `field`.
    * `"other"` -- a nameable head that is not this boundary's business:
      a local `IORef`, an unapplied accessor, an accessor the module
      cannot reach, an exempted shadow, or the primitive used as a
      VALUE rather than applied to anything here.
    * `"unclassifiable"` -- an argument is plainly being formed and the
      scan cannot name its head. This BLOCKS (requirement 6): it is how
      a spelling outside the recognized set fails loudly instead of
      silently dropping a write.
    """
    relpath: str
    line: int
    module: str
    kind: str
    field: str | None


def classify_mutation_site(tokens: list[Token], index: int
                           ) -> tuple[str, int | None]:
    """`(kind, head token index)` for the mutation primitive at
    `tokens[index]`, before scope is consulted -- `"applied"`,
    `"bare"`, `"value"` or `"unclassifiable"`.

    Every occurrence lands in exactly one of the four, which is what
    makes the recognized-form list a closed set rather than an
    aspiration."""
    head, grouped = first_argument_token(tokens, index)
    if head is not None:
        applied = grouped and _applied_head(tokens, head) is not None
        return ("applied" if applied else "bare"), head
    if (index > 0 and tokens[index - 1].kind == "punc"
            and tokens[index - 1].text == "`"):
        operand = _operand_head(tokens, index - 2)
        if operand is None:
            return "unclassifiable", None
        applied = _infix_left_operand_head(tokens, index) is not None
        return ("applied" if applied else "bare"), operand
    return ("unclassifiable" if grouped else "value"), None


class WriteScan(NamedTuple):
    """Everything one pass over the production tree establishes."""
    writes: dict[str, set[str]]
    residue: list[Occurrence]
    sites: list[MutationSite]
    suppressed: frozenset[tuple[str, str]]


def scan_capability_writes(
    sources: dict[str, str], live_fields: list[str], *,
    permanent: frozenset[str] = PERMANENT_IMPORTERS,
    definer: str = PERMANENT_DEFINER,
    exemptions: dict[tuple[str, str], str] | None = None,
) -> WriteScan:
    """Pure core of the CMA-1 scan.

    Both a RAW `EngineEnv` accessor (a narrow-import consumer) and a
    CAPABILITY-record accessor canonicalize onto the same `EngineEnv`
    field, so the two consumer shapes are one boundary. Accessor AND
    mutation primitive are each recognized qualified (`State.fieldOne`,
    `Ref.writeIORef`) as readily as bare.

    Two rules decide an attribution, and neither models Haskell's
    binding forms: the identifier must be in scope in that module under
    the exact spelling used (`parse_imports`/`imports_name`), and it
    must head an APPLIED argument of the primitive -- the first argument
    of a prefix application (`_first_argument_head`), or the left
    operand of a backticked infix one (`_infix_left_operand_head`).
    `SHADOW_EXEMPTIONS` covers the residue of that: a module that binds
    a name matching an accessor AND applies it to a handle.

    EVERY mutation-primitive occurrence is classified exactly once
    (`classify_mutation_site`), and a site whose argument the scan
    cannot read is recorded as `unclassifiable` for `main` to fail on --
    requirement 6, and what keeps the recognized-form list closed.

    `permanent`/`definer` are SS6.1's cohort (D-4), excluded from the
    write map -- their authority is not what this boundary constrains.
    They are parameters, like `exemptions`, so the self-test can drive
    small synthetic fixtures instead of the real ~200-module tree.

    The residue is every remaining CAPABILITY-accessor use -- a helper
    argument, a context-record field, a queue/`TVar`/`MVar` handle, a
    point-free composition -- i.e. exactly what the write scan cannot
    attribute (D-5). A direct `readIORef` application to a known
    accessor is an inline READ, not a pass-on. Occurrences are counted
    individually, never deduplicated to field/module pairs. An
    accessor's own defining capability module is excluded, because its
    record declaration, export list and projection are declarations
    rather than uses."""
    exempt = set(permanent) | {definer}
    shadows = SHADOW_EXEMPTIONS if exemptions is None else exemptions
    accessors = capability_accessor_map(sources, live_fields)
    raw_fields = set(live_fields)

    writes: dict[str, set[str]] = {field: set() for field in live_fields}
    residue: list[Occurrence] = []
    sites: list[MutationSite] = []
    suppressed: set[tuple[str, str]] = set()

    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        declarations = parse_imports(text)
        code = prepared_source(text)
        tokens = tokenize_haskell(code)
        indents = line_indents(code)

        def resolve(name: str) -> tuple[str, str, str] | None:
            """`(EngineEnv field, owning module, base accessor name)` for
            an occurrence spelled `name` here, or `None` when it names no
            accessor this module can reach under that exact spelling --
            `Other.fieldOne` is not this field, and neither is a bare
            `fieldOne` in a module that imports the owner `qualified` or
            `hiding` it."""
            qualifier, _, base = name.rpartition(".")
            if base in raw_fields:
                owners: tuple[tuple[str, str, str], ...] = (
                    (base, STATE_MODULE, ENGINE_ENV_TYPE),)
            else:
                owners = accessors.get(base, ())
                if not owners:
                    return None
            # One selector name can belong to several capability
            # records; the module's own imports say which one it means,
            # so every candidate is offered the scope test rather than
            # the first arbitrarily winning.
            for field, owner, record in owners:
                if not qualifier and module == owner:
                    return field, owner, base
                if imports_name(declarations, owner, base, qualifier, record):
                    return field, owner, base
            return None

        inline_heads: set[int] = set()
        for index, token in enumerate(tokens):
            if token.kind != "id":
                continue
            # A mutation primitive is just as much itself under a
            # qualifier (`Ref.writeIORef`, from
            # `import qualified Data.IORef as Ref`), and missing one
            # would be a SILENT hole in the gate -- but it must be the
            # `Data.IORef` one, resolved through this module's own
            # imports, or a local homonym would fabricate a write.
            primitive = resolve_primitive(declarations, token.text)
            if primitive is None:
                continue
            if not in_head_position(tokens, index, indents):
                # Being passed on, not applied: no inline use to record,
                # and the accessor beside it stays residue. Unless what
                # precedes it is an operator SECTION, which may well be
                # applying it -- unreadable either way, so it blocks.
                if primitive in IOREF_WRITE_PRIMITIVES:
                    sites.append(MutationSite(
                        relpath, token.line, module,
                        "unclassifiable"
                        if after_operator_section(tokens, index) else "other",
                        None))
                continue
            head = _first_argument_head(tokens, index)
            if head is None:
                head = _infix_left_operand_head(tokens, index)
            if head is not None:
                inline_heads.add(head)
            if primitive not in IOREF_WRITE_PRIMITIVES:
                continue

            kind, candidate = classify_mutation_site(tokens, index)
            if kind == "unclassifiable":
                sites.append(MutationSite(
                    relpath, token.line, module, "unclassifiable", None))
                continue
            field = None
            if kind == "applied" and candidate is not None:
                resolved = resolve(tokens[candidate].text)
                if resolved is not None:
                    field = resolved[0]
            if field is None or module in exempt:
                sites.append(
                    MutationSite(relpath, token.line, module, "other", field))
                continue
            if (module, field) in shadows:
                suppressed.add((module, field))
                sites.append(
                    MutationSite(relpath, token.line, module, "other", field))
                continue
            writes[field].add(module)
            sites.append(
                MutationSite(relpath, token.line, module, "write", field))

        for index, token in enumerate(tokens):
            if token.kind != "id":
                continue
            resolved = resolve(token.text)
            if resolved is None:
                continue
            field, owner, base = resolved
            if (not owner.startswith(CAPABILITY_MODULE_PREFIX)
                    or module == owner or index in inline_heads):
                continue
            residue.append(
                Occurrence(relpath, token.line, base, field, module))

    residue.sort()
    sites.sort()
    return WriteScan(writes, residue, sites, frozenset(suppressed))


def audit_mutation_sites(sites: list[MutationSite]) -> list[str]:
    """Requirement 6: no mutation-primitive occurrence may go
    unclassified.

    This is what makes the recognized-form list in
    docs/engineenv_capability_inventory.md SS6.5 a CLOSED set. Without
    it, a spelling the scan does not model -- a new operator, an
    unfamiliar grouping -- silently drops the write and the map keeps
    claiming a guarantee it no longer provides. With it, the gate stops
    and names the site instead."""
    return [
        f"{site.relpath}:{site.line} mutates an `IORef` through an "
        f"expression this audit cannot read -- every mutation site must "
        f"classify (docs/{INVENTORY_PATH.name} SS6.5's recognized write "
        f"forms). Extend the scan and that list together, or restate the "
        f"site in a recognized form; do NOT leave it unread, because an "
        f"unread site is an unenforced field"
        for site in sites if site.kind == "unclassifiable"]


def audit_shadow_exemptions(
    suppressed: frozenset[tuple[str, str]], live_fields: list[str], *,
    exemptions: dict[tuple[str, str], str] | None = None,
) -> list[str]:
    """Requirement 7's other half: each exemption must name a live
    field, carry a real reason, and still be doing something.

    Checked in both directions like every other list in this file -- a
    stale exemption is a suppression nobody is watching any more, which
    is exactly the silent hole the map exists to close."""
    shadows = SHADOW_EXEMPTIONS if exemptions is None else exemptions
    fields = set(live_fields)
    violations: list[str] = []
    for (module, field), reason in sorted(shadows.items()):
        if field not in fields:
            violations.append(
                f"SHADOW_EXEMPTIONS names `{field}` for `{module}`, which "
                f"is not a live `EngineEnv` field -- remove the stale "
                f"entry")
            continue
        if not reason or not reason.strip() or _is_placeholder(reason):
            violations.append(
                f"the SHADOW_EXEMPTIONS entry for `{module}`/`{field}` "
                f"carries no real reason -- an exemption suppresses a "
                f"detected write, so it states why that write is a local "
                f"binding rather than the field")
            continue
        if (module, field) not in suppressed:
            violations.append(
                f"`{module}` is exempted from writing `{field}` but no "
                f"such write is detected any more -- remove the stale "
                f"entry, the same way the writing-module map is checked "
                f"in both directions")
    return violations


def audit_writer_modules(
    writes: dict[str, set[str]], live_fields: list[str], *,
    declared: dict[str, frozenset[str]] | None = None,
) -> list[str]:
    """Pure core of the both-directions map check: the map's keys equal
    the live `EngineEnv` field set, every detected write is declared,
    and every declared module still writes what it is mapped to."""
    mapping = CAPABILITY_WRITER_MODULES if declared is None else declared
    audit_name = Path(__file__).name
    violations: list[str] = []

    for field in sorted(set(live_fields) - set(mapping)):
        violations.append(
            f"`{field}` is a live `EngineEnv` field with no entry in "
            f"CAPABILITY_WRITER_MODULES (tools/{audit_name}) -- every field "
            f"carries a writing-module set, `frozenset()` included, so a new "
            f"field cannot arrive unmapped (docs/{INVENTORY_PATH.name} "
            f"SS6.4 step 11, SS6.5)")
    for field in sorted(set(mapping) - set(live_fields)):
        violations.append(
            f"CAPABILITY_WRITER_MODULES maps `{field}`, which is not a live "
            f"`EngineEnv` field -- remove the stale key")

    for field in sorted(set(mapping) & set(live_fields)):
        allowed = set(mapping[field])
        actual = writes.get(field, set())
        for module in sorted(actual - allowed):
            violations.append(
                f"`{module}` writes `{field}` but is not in that field's "
                f"CAPABILITY_WRITER_MODULES set (tools/{audit_name}) -- "
                f"either the write belongs somewhere else, or the map grows "
                f"deliberately in the same change; see "
                f"docs/{INVENTORY_PATH.name} SS6.5")
        for module in sorted(allowed - actual):
            violations.append(
                f"`{module}` is mapped as a writer of `{field}` but no "
                f"longer writes it -- remove the stale entry, the same way "
                f"RENDER_MAIN_ONLY_MODULES is checked in both directions")
    return violations


def format_residue(residue: list[Occurrence]) -> list[str]:
    """The non-blocking pass-on report (D-5), one line per SOURCE
    OCCURRENCE -- never deduplicated, never resolved to an originating
    module. This count is the evidence CMA-2's pilot and CMA-3's verdict
    both turn on: a small residue means a textual gate is nearly
    sufficient, a large one argues for a mechanism that travels with the
    handle. It is printed on EVERY run, ahead of every blocking check,
    so a failure elsewhere never costs the measurement."""
    lines = [
        f"capability-accessor pass-on residue: {len(residue)} use(s) the "
        f"direct-write scan cannot attribute (non-blocking, reported not "
        f"resolved -- design decision D-5):"
    ]
    lines.extend(
        f"  - {item.relpath}:{item.line} `{item.accessor}` "
        f"(-> `{item.field}`) in `{item.module}`"
        for item in residue)
    return lines


def main() -> int:
    engine_env_source = (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8")
    inventory_text = INVENTORY_PATH.read_text(encoding="utf-8")
    live_fields = extract_record_fields(engine_env_source, ENGINE_ENV_PATTERN)
    production_sources = scan_production_sources(REPO_ROOT)

    # The pass-on residue is non-blocking evidence (D-5), so it is
    # printed FIRST -- before any check that can `return 1` -- and the
    # measurement survives a failure anywhere below it.
    scan = scan_capability_writes(production_sources, live_fields)
    field_writes, residue = scan.writes, scan.residue
    for line in format_residue(residue):
        print(line)
    print()

    violations = audit(engine_env_source, inventory_text)
    if violations:
        print(f"{len(violations)} EngineEnv capability-inventory violation(s):")
        for v in violations:
            print(f"  - {v}")
        print(f"\nAdd or fix a classification row for each item above in "
              f"{INVENTORY_PATH.relative_to(REPO_ROOT)} SS5 (see SS2 for the "
              f"capability/thread-role/lifecycle vocabulary).")
        return 1

    total_violations = audit_field_total(live_fields, inventory_text)
    if total_violations:
        print(f"{len(total_violations)} SS1 field-total/field-span "
              f"violation(s):")
        for v in total_violations:
            print(f"  - {v}")
        return 1

    unrestricted = classify_production_sources(production_sources)
    doc_temporary = parse_temporary_boundary(inventory_text)
    ratchet_violations = audit_ratchet(unrestricted, doc_temporary)
    if ratchet_violations:
        print(f"{len(ratchet_violations)} SS6 full-access ratchet violation(s):")
        for v in ratchet_violations:
            print(f"  - {v}")
        return 1

    permanent_violations = audit_permanent_boundary(inventory_text)
    if permanent_violations:
        print(f"{len(permanent_violations)} SS6.1 permanent-allowlist "
              f"violation(s):")
        for v in permanent_violations:
            print(f"  - {v}")
        return 1

    save_load_violations = audit_save_load_projection(
        production_sources, (REPO_ROOT / "synarchy.cabal").read_text(encoding="utf-8"))
    if save_load_violations:
        print(f"{len(save_load_violations)} save-load capability record "
              f"violation(s):")
        for v in save_load_violations:
            print(f"  - {v}")
        return 1

    boundary_violations = audit_render_boundary(production_sources)
    if boundary_violations:
        print(f"{len(boundary_violations)} SS3 main-render boundary "
              f"violation(s):")
        for v in boundary_violations:
            print(f"  - {v}")
        return 1

    input_violations = audit_input_boundary(production_sources)
    if input_violations:
        print(f"{len(input_violations)} SS7.3 LuaThread input boundary "
              f"violation(s):")
        for v in input_violations:
            print(f"  - {v}")
        return 1

    site_violations = audit_mutation_sites(scan.sites)
    if site_violations:
        print(f"{len(site_violations)} unclassifiable mutation site(s):")
        for v in site_violations:
            print(f"  - {v}")
        return 1

    exemption_violations = audit_shadow_exemptions(
        scan.suppressed, live_fields)
    if exemption_violations:
        print(f"{len(exemption_violations)} SHADOW_EXEMPTIONS violation(s):")
        for v in exemption_violations:
            print(f"  - {v}")
        return 1

    writer_violations = audit_writer_modules(field_writes, live_fields)
    if writer_violations:
        print(f"{len(writer_violations)} SS5 writing-module map "
              f"violation(s):")
        for v in writer_violations:
            print(f"  - {v}")
        return 1

    total_fields = len(live_fields)
    mapped_fields = sum(1 for m in CAPABILITY_WRITER_MODULES.values() if m)
    mapped_pairs = sum(len(m) for m in CAPABILITY_WRITER_MODULES.values())
    temporary_total = sum(len(m) for m in TEMPORARY_CEILING.values())
    print(f"engine-env capability-inventory audit: {total_fields} EngineEnv "
          f"field(s) all classified and agreeing with SS1's marked field "
          f"total and `{live_fields[0]}`-through-`{live_fields[-1]}` span, {len(unrestricted) + 1} full-access "
          f"modules (incl. the {PERMANENT_DEFINER} definer) within the SS6 "
          f"ratchet, all permanent (SS6.1 documented set == the checked-in "
          f"constants; {temporary_total} temporary), "
          f"{len(RENDER_MAIN_ONLY_MODULES)} MainRender module(s) "
          f"holding the full render capability and no non-owner naming "
          f"`engineStateRef` (SS3), {len(INPUT_LUA_ONLY_MODULES)} LuaThread "
          f"module(s) holding the full input capability and no non-owner "
          f"naming `inputBarrierNextRef`/`currentKeyDownRef` (SS7.3), "
          f"{mapped_fields}/{total_fields} field(s) carrying a non-empty "
          f"writing-module map covering {mapped_pairs} field-module pair(s) "
          f"with no undeclared or stale entry (SS5) over "
          f"{len(scan.sites)} classified mutation site(s) and "
          f"{len(SHADOW_EXEMPTIONS)} shadow exemption(s), and "
          f"{len(residue)} reported pass-on residue use(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
