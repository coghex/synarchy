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
also pins each field's direct WRITING MODULES -- SS5's Writers cells
are prose validated for grammar and citation presence only, so until
that check the doc could claim a field has no writers at all and a
change falsifying it passed every gate. Since issue #2036 that scanner
is its own module, tools/engine_env_capability_writers.py, which owns
`CAPABILITY_WRITER_MODULES`, `SHADOW_EXEMPTIONS`, the Haskell
tokenizer and import resolver, the capability-accessor map and its
#2059 fail-closed completeness audit, mutation-site classification,
the scan, its three blocking checks and the pass-on residue report.
This file stays the ONE gate CI and tools/ci-local.sh run: `main`
below scans the production tree once, prints the residue FIRST (design
decision D-5 -- ahead of every blocking check, so a failure elsewhere
never costs the measurement), runs its own checks, then the writer
module's, and exits 1 on the first failing group. The inputs and
helpers both halves read -- the repository anchors, the live-field
derivation, `scan_production_sources`, SS6.1's permanent set, the
Haskell comment/import/module-name helpers and the projection
canonicalizer -- live in tools/engine_env_capability_common.py, which
neither half duplicates and which imports neither of them; the writer
module imports only that, and this file imports both. Names from both
are re-exported here, so `from engine_env_capability_audit import ...`
keeps resolving everything the aggregate self-test needs.

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

sys.path.insert(0, str(Path(__file__).resolve().parent))
# The shared substrate (#2036): imported by NAME so the symbols stay
# addressable from this module, the aggregate's public surface.
from engine_env_capability_common import (  # type: ignore  # noqa: F401
    ENGINE_ENV_FILE, ENGINE_ENV_PATTERN, INVENTORY_PATH, PERMANENT_DEFINER,
    PERMANENT_IMPORTERS, REPO_ROOT, STATE_MODULE, _import_chunks,
    _is_placeholder, _strip_haskell_comments, extract_record_fields,
    module_identifier, parse_projection_bindings, scan_production_sources,
)
# The SS5 writing-module scanner (#1892, extracted by #2036): the entry
# points `main` calls, plus the two checked-in authorities the summary
# line counts.
from engine_env_capability_writers import (  # type: ignore  # noqa: F401
    CAPABILITY_WRITER_MODULES, SHADOW_EXEMPTIONS,
    audit_capability_projection_completeness, audit_mutation_sites,
    audit_shadow_exemptions, audit_writer_modules,
    discover_capability_records, format_residue, scan_capability_writes,
)


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
# SS6.1's permanent set -- `PERMANENT_DEFINER` + `PERMANENT_IMPORTERS`
# -- lives in tools/engine_env_capability_common.py since #2036, because
# the writer scanner's D-4 exemption reads the same constants this
# ratchet and `audit_permanent_boundary` compare against the live
# importers. What is checked-in HERE is the ratchet's own state: the
# temporary ceiling below.

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


_IMPORT_HEAD_RE = re.compile(r"^import\s+(?:qualified\s+)?([A-Za-z][A-Za-z0-9_.']*)")
_EXPLICIT_ENGINEENV_RE = re.compile(r"EngineEnv\s*\(\s*\.\.\s*\)")


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

    projection_violations = audit_capability_projection_completeness(
        production_sources, live_fields)
    if projection_violations:
        print(f"{len(projection_violations)} capability projection "
              f"completeness violation(s):")
        for v in projection_violations:
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
    capability_records = discover_capability_records(production_sources)
    projected_fields = sum(
        len(parse_projection_bindings(production_sources[entry.relpath],
                                      entry.projection))
        for entry in capability_records if entry.projection is not None)
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
          f"{len(capability_records)} capability record(s) whose "
          f"{projected_fields} projected field(s) all canonicalize onto a "
          f"live EngineEnv accessor (SS2.1), "
          f"{mapped_fields}/{total_fields} field(s) carrying a non-empty "
          f"writing-module map covering {mapped_pairs} field-module pair(s) "
          f"with no undeclared or stale entry (SS5) over "
          f"{len(scan.sites)} classified mutation site(s) and "
          f"{len(SHADOW_EXEMPTIONS)} shadow exemption(s), and "
          f"{len(residue)} reported pass-on residue use(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
