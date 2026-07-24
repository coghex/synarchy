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

Usage:
  python3 tools/engine_env_capability_audit.py
Exit codes: 0 = every live EngineEnv field is validly classified and
the SS6 ratchet holds, 1 = one or more violations found.
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
# SS6 full-access ratchet (issue #889, EngineEnv capability split E1)
# ===========================================================================
#
# docs/engineenv_capability_inventory.md SS6.1's permanent modules -- a
# hard, checked-in allowlist. `Engine.Core.State` itself (the definer,
# which imports nothing and so can never appear in a live importer
# scan) is the 25th permanent module; PERMANENT_IMPORTERS below holds
# only the 24 modules that actually IMPORT it.
PERMANENT_DEFINER = "Engine.Core.State"

PERMANENT_IMPORTERS = frozenset({
    "Engine.Core.Monad",
    "Engine.Core.Init",
    "Engine.Core.Defaults",
    "Engine.Loop", "Engine.Loop.Frame", "Engine.Loop.Headless",
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
TEMPORARY_CEILING: dict[str, frozenset[str]] = {
    "core-init": frozenset({
        "Engine.Graphics.Vulkan.Command.Record", "Engine.Scripting.Lua.API.Log",
    }),
    "render-gpu-asset": frozenset({
        "Building.HitTest", "Building.Render", "Engine.Asset.Manager",
        "Engine.Graphics.Font.Draw", "Engine.Graphics.Font.Load",
        "Engine.Graphics.Font.Upload", "Engine.Graphics.Vulkan.Command.Sprite",
        "Engine.Graphics.Vulkan.Command.Text", "Engine.Graphics.Vulkan.Framebuffer",
        "Engine.Graphics.Vulkan.Init", "Engine.Graphics.Vulkan.MSAA",
        "Engine.Graphics.Vulkan.Offscreen", "Engine.Graphics.Vulkan.Pipeline",
        "Engine.Graphics.Vulkan.Pipeline.Bindless", "Engine.Graphics.Vulkan.Recreate",
        "Engine.Graphics.Vulkan.Swapchain", "Engine.Graphics.Vulkan.Sync",
        "Engine.Graphics.Vulkan.Texture.Bindless",
        "Engine.Graphics.Vulkan.Texture.DefaultFaceMap", "Engine.Graphics.Window.GLFW",
        "Engine.Scene.Batch.Text", "Engine.Scene.Render",
        "Engine.Scripting.Lua.API.Camera", "Engine.Scripting.Lua.API.Config",
        "Engine.Scripting.Lua.API.Graphics", "Engine.Scripting.Lua.API.Input",
        "Engine.Scripting.Lua.API.Items.Render", "Engine.Scripting.Lua.API.Screenshot",
        "Engine.Scripting.Lua.API.Text", "Engine.Scripting.Lua.API.UI.Placement",
        "Engine.Scripting.Lua.API.WorldQuery.Pick", "Engine.Scripting.Lua.API.YamlTextures",
        "Engine.Scripting.Lua.Message.Texture", "Engine.Scripting.Lua.Message.Video",
        "Engine.Scripting.Lua.Message.WorldTexture", "Structure.Render", "UI.Render",
        "Unit.HitTest", "World.Render", "World.Render.BloodQuads",
        "World.Render.CursorQuads", "World.Render.GroundItemQuads",
        "World.Render.Quads", "World.Render.SpoilQuads", "World.Render.Zoom.Quads",
    }),
    "input-lua-transport": frozenset({
        "Engine.Input.Callback", "Engine.Input.Thread", "Engine.Input.Thread.Char",
        "Engine.Input.Thread.Dispatch", "Engine.Input.Thread.Keyboard",
        "Engine.Input.Thread.Mouse.Activation", "Engine.Input.Thread.Scroll",
        "Engine.Scripting.Lua.API.InputInject", "Engine.Scripting.Lua.API.Keybinds",
        "World.Log", "World.Thread.Helpers",
    }),
    "world-sim-render-handoff": frozenset({
        "Blood.Impact", "Engine.Scripting.Lua.API.Blood", "Engine.Scripting.Lua.API.Chop",
        "Engine.Scripting.Lua.API.Construct", "Engine.Scripting.Lua.API.Core",
        "Engine.Scripting.Lua.API.Flora", "Engine.Scripting.Lua.API.Forage.Crop",
        "Engine.Scripting.Lua.API.Forage.Lookup", "Engine.Scripting.Lua.API.Forage.Query",
        "Engine.Scripting.Lua.API.Plant", "Engine.Scripting.Lua.API.Structure",
        "Engine.Scripting.Lua.API.Till", "Engine.Scripting.Lua.API.World.Clock",
        "Engine.Scripting.Lua.API.World.Cursor", "Engine.Scripting.Lua.API.World.Designation",
        "Engine.Scripting.Lua.API.World.Edit", "Engine.Scripting.Lua.API.World.GenConfig",
        "Engine.Scripting.Lua.API.World.Lifecycle", "Engine.Scripting.Lua.API.World.Query",
        "Engine.Scripting.Lua.API.World.Tools", "Engine.Scripting.Lua.API.WorldQuery.Chunk",
        "Engine.Scripting.Lua.API.WorldQuery.Climate", "Engine.Scripting.Lua.API.WorldQuery.Fluid",
        "Engine.Scripting.Lua.API.WorldQuery.Lookup", "Engine.Scripting.Lua.API.WorldQuery.River",
        "Engine.Scripting.Lua.API.WorldQuery.Terrain", "Sim.Thread", "Unit.LineOfSight",
        "Unit.Render", "Unit.Thread.Movement.PathAdvance", "World.Render.Zoom.Background",
        "World.Thread", "World.Thread.ChunkLoading", "World.Thread.Command",
        "World.Thread.Command.Basic", "World.Thread.Command.Cursor.Chop",
        "World.Thread.Command.Cursor.Construct", "World.Thread.Command.Cursor.Mine",
        "World.Thread.Command.Cursor.Plant", "World.Thread.Command.Cursor.Select",
        "World.Thread.Command.Cursor.Till", "World.Thread.Command.Edit.Fluid",
        "World.Thread.Command.Edit.Structure", "World.Thread.Command.Edit.Sync",
        "World.Thread.Command.Edit.Terrain", "World.Thread.Command.Edit.Vegetation",
        "World.Thread.Command.Init", "World.Thread.Command.Location",
        "World.Thread.Command.Texture", "World.Thread.Command.Time",
        "World.Thread.Command.UI", "World.Thread.Cursor", "World.Thread.Time",
    }),
    "units-buildings-combat": frozenset({
        "Building.Thread.Command", "Combat.Resolution", "Combat.Resolution.Events",
        "Combat.Resolution.Wear", "Combat.Thread", "Combat.Wounds.Tick",
        "Engine.Input.State", "Engine.Scripting.Lua.API.ActionOutcome",
        "Engine.Scripting.Lua.API.Buildings.Materials", "Engine.Scripting.Lua.API.Buildings.Progress",
        "Engine.Scripting.Lua.API.Buildings.Query", "Engine.Scripting.Lua.API.Buildings.Selection",
        "Engine.Scripting.Lua.API.Buildings.Spawn", "Engine.Scripting.Lua.API.Buildings.Yaml",
        "Engine.Scripting.Lua.API.Combat", "Engine.Scripting.Lua.API.Craft.Bill",
        "Engine.Scripting.Lua.API.Craft.Execute", "Engine.Scripting.Lua.API.Equipment.Accessory",
        "Engine.Scripting.Lua.API.Equipment.Render", "Engine.Scripting.Lua.API.Equipment.Slot",
        "Engine.Scripting.Lua.API.Forage.Harvest", "Engine.Scripting.Lua.API.Items.Ground",
        "Engine.Scripting.Lua.API.Power", "Engine.Scripting.Lua.API.Units.Cargo",
        "Engine.Scripting.Lua.API.Units.Combat", "Engine.Scripting.Lua.API.Units.Equipment",
        "Engine.Scripting.Lua.API.Units.Inventory", "Engine.Scripting.Lua.API.Units.List",
        "Engine.Scripting.Lua.API.Units.Medical", "Engine.Scripting.Lua.API.Units.Query",
        "Engine.Scripting.Lua.API.Units.Selection", "Engine.Scripting.Lua.API.Units.Spawn",
        "Engine.Scripting.Lua.API.Units.Stats", "Engine.Scripting.Lua.API.Units.Survival",
        "Engine.Scripting.Lua.API.Units.Yaml", "Unit.Selection", "Unit.Thread",
        "Unit.Thread.Command", "Unit.Thread.Command.Lifecycle", "Unit.Thread.Command.Motion",
        "Unit.Thread.Command.Pose", "Unit.Thread.Command.Spawn", "Unit.Thread.Movement",
        "Unit.Thread.Movement.Climb", "World.Thread.Command.Cursor.Common",
        "World.Thread.Command.Edit.Dig", "World.Thread.Discovery", "World.Thread.ItemTemp",
        "World.Thread.Power",
    }),
    "content-registries": frozenset({
        "Engine.Scripting.Lua.API.Craft.Recipe", "Engine.Scripting.Lua.API.Equipment.Class",
        "Engine.Scripting.Lua.API.Infection", "Engine.Scripting.Lua.API.Items.Defs",
        "Engine.Scripting.Lua.API.Locations", "Engine.Scripting.Lua.API.LootTables",
        "Engine.Scripting.Lua.API.Repair", "Engine.Scripting.Lua.API.Substance",
        "Engine.Scripting.Lua.API.WorldQuery.Location",
    }),
    "ui-hud-events": frozenset({
        "Engine.Input.Thread.Mouse", "Engine.PlayerEvent.Emit", "Engine.Scripting.Lua.API.Focus",
        "Engine.Scripting.Lua.API.PlayerEvent", "Engine.Scripting.Lua.API.UI.Element",
        "Engine.Scripting.Lua.API.UI.Focus", "Engine.Scripting.Lua.API.UI.Hierarchy",
        "Engine.Scripting.Lua.API.UI.Page", "Engine.Scripting.Lua.API.UI.Property",
        "Engine.Scripting.Lua.API.UI.TextInput", "Engine.Scripting.Lua.API.UI.Tooltip",
        "Engine.Scripting.Lua.Message.Scene", "UI.Tooltip.State",
    }),
    "save-load-coordination": frozenset(),
}

PRODUCTION_DIRS = ("src", "app")
STATE_MODULE = "Engine.Core.State"

_IMPORT_LINE_RE = re.compile(r"^import\b")
_IMPORT_HEAD_RE = re.compile(r"^import\s+(?:qualified\s+)?([A-Za-z][A-Za-z0-9_.']*)")
_EXPLICIT_ENGINEENV_RE = re.compile(r"EngineEnv\s*\(\s*\.\.\s*\)")
_BLOCK_COMMENT_RE = re.compile(r"\{-.*?-\}", re.DOTALL)


def _strip_haskell_comments(text: str) -> str:
    """Strip `{- -}` block comments (newline-count preserved, so line
    numbers/column-0 checks downstream stay meaningful) and `--` line
    comments."""
    text = _BLOCK_COMMENT_RE.sub(lambda m: "\n" * m.group(0).count("\n"), text)
    lines = []
    for line in text.split("\n"):
        idx = line.find("--")
        lines.append(line[:idx] if idx != -1 else line)
    return "\n".join(lines)


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
    sources: dict[str, str] = {}
    for base in PRODUCTION_DIRS:
        for path in sorted((repo_root / base).rglob("*.hs")):
            relpath = str(path.relative_to(repo_root))
            sources[relpath] = path.read_text(encoding="utf-8", errors="replace")
    return classify_production_sources(sources)


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
    violations = audit(engine_env_source, inventory_text)
    if violations:
        print(f"{len(violations)} EngineEnv capability-inventory violation(s):")
        for v in violations:
            print(f"  - {v}")
        print(f"\nAdd or fix a classification row for each item above in "
              f"{INVENTORY_PATH.relative_to(REPO_ROOT)} SS5 (see SS2 for the "
              f"capability/thread-role/lifecycle vocabulary).")
        return 1

    unrestricted = scan_production_unrestricted_importers(REPO_ROOT)
    doc_temporary = parse_temporary_boundary(inventory_text)
    ratchet_violations = audit_ratchet(unrestricted, doc_temporary)
    if ratchet_violations:
        print(f"{len(ratchet_violations)} SS6 full-access ratchet violation(s):")
        for v in ratchet_violations:
            print(f"  - {v}")
        return 1

    total_fields = len(extract_record_fields(engine_env_source, ENGINE_ENV_PATTERN))
    print(f"engine-env capability-inventory audit: {total_fields} EngineEnv "
          f"field(s) all classified, {len(unrestricted) + 1} full-access "
          f"modules (incl. the {PERMANENT_DEFINER} definer) within the SS6 "
          f"ratchet")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
