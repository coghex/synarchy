#!/usr/bin/env python3
"""SS6 unrestricted-access governance (issues #889 and #899, EngineEnv
capability splits E1 and E8; extracted from
tools/engine_env_capability_audit.py by issue #2064).

Since issue #889 (E1) this contract enforces the SS6 full-access
ratchet: every production (`src/`/`app/`) Haskell module that imports
`Engine.Core.State` either with an explicit `EngineEnv(..)` or as a
bare import (no import list -- both grant unrestricted field-level
access, see SS6's own methodology) must be either one of SS6.1's
permanent modules (a hard, checked-in allowlist) or one of SS6.2's
individually-assigned temporary modules (a checked-in, strict,
shrink-only ceiling established by #889 and cross-checked against
SS6.2's own documented accounting). A module newly gaining unrestricted
access fails this ratchet even if SS6.2 is ALSO edited to document it
-- growing the checked-in ceiling itself (in this file) is the only way
to admit a new temporary full-access module, and doing so without a
matching SS6.2 update fails the doc/ceiling consistency check below.
Both the temporary ceiling AND the permanent allowlist are checked in
BOTH directions: a module also fails the ratchet if it is listed in the
checked-in ceiling (and/or SS6.2) or in PERMANENT_IMPORTERS (SS6.1) but
no longer has live unrestricted access -- a stale entry left behind by
a migration that narrowed the module without also updating its
allowlist/ceiling entry -- since both SS6.1 and SS6.2 must stay an
exact, exhaustive mirror of the live full-access set, never merely an
upper bound on it. `test/` sources remain outside this ratchet entirely
(SS6.3's test-only exception).

Since issue #899 (E8, the epic's final child) that ceiling is __EMPTY__
and the boundary is PERMANENT-ONLY: the live unrestricted production
importer set equals PERMANENT_IMPORTERS exactly, so there is no legal
path left for a module to take unrestricted access -- "add the field
now, narrow it later" no longer exists. `audit_permanent_boundary`
makes that flip unforgiving: it parses SS6.1's DOCUMENTED module set
(its first column only -- Reason cells cite other module names as
context) and requires it to equal PERMANENT_DEFINER +
PERMANENT_IMPORTERS, with every row carrying a real, non-placeholder
Category AND Reason. Documentation alone cannot admit a permanent
importer, and neither can a constant change with no written
justification. E8's other record -- the save-load capability module and
its projection -- is pinned by tools/engine_env_capability_saveload.py.

SS6.4 documents the procedure for what to do instead (most new state
does not belong on `EngineEnv` at all).

SS6.1's permanent set -- `PERMANENT_DEFINER` + `PERMANENT_IMPORTERS` --
lives in tools/engine_env_capability_common.py since issue #2036,
because the writer scanner's D-4 exemption reads the same constants
this ratchet and `audit_permanent_boundary` compare against the live
importers. What is checked in HERE is the ratchet's own state: the
temporary ceiling.

`scan_production_unrestricted_importers` is a test-only convenience: it
performs its own production-tree walk, so the aggregate -- which scans
`src/` and `app/` exactly once per run (#2064 requirement 13) -- must
never call it, and does not. It stays here because the self-test drives
the ratchet through it.

Not independently a gate: `python3 tools/engine_env_capability_audit.py`
remains the one command CI and tools/ci-local.sh run.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore
    BACKTICK_RE, INVENTORY_PATH, PERMANENT_DEFINER, PERMANENT_IMPORTERS,
    SECTION_6_2_HEADING, SEPARATOR_ROW_RE, STATE_MODULE, _IMPORT_HEAD_RE,
    _import_chunks, _is_placeholder, _strip_haskell_comments,
    module_identifier, scan_production_sources,
)


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
    # Engine.Scripting.Lua.API.PlayerEvent) now reach the
    # event/notification fields (eventStoreRef, notificationCfgRef,
    # notificationOrder) through Engine.Core.Capability.Events -- see
    # SS7.7. That slice was four fields until #2285 removed the
    # write-only popupQueueRef. Neither half has an unrestricted
    # consumer left.
    "ui-hud-events": frozenset(),
    # Never had a temporary consumer: every module whose dominant field
    # usage is save/load coordination is a permanent SS6.1 whole-session
    # orchestration boundary (SS7.8). #899 (E8) added
    # Engine.Core.Capability.SaveLoad for the NON-permanent touchpoints
    # -- the per-tick `captureLocked`/`acknowledgeCurrent` sites -- and
    # narrowed `World.Thread` onto it.
    "save-load-coordination": frozenset(),
}


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


# SS6.1's heading has exactly one reader -- this owner -- so it stays
# here. SS6.2's is shared with the field-total owner (SS6.2's item 1 is
# the sentence audited for the total it used to repeat) and therefore
# lives in tools/engine_env_capability_common.py.
SECTION_6_1_HEADING = "### 6.1 Permanent (production)"
# A Modules cell that is ENTIRELY one italicized parenthetical --
# `*(...)*` spanning the whole cell -- is explanatory prose (citing
# other modules/fields for context), never a module assignment, no
# matter what backtick-quoted names it contains; see the real
# `save-load-coordination` row.
_EXPLANATORY_CELL_RE = re.compile(r"^\*\(.*\)\*$", re.DOTALL)


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
        if all(SEPARATOR_ROW_RE.fullmatch(c) for c in cells if c):
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
        if all(SEPARATOR_ROW_RE.fullmatch(c) for c in cells if c):
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
            f"PERMANENT_DEFINER constants "
            f"(tools/engine_env_capability_common.py) -- documenting a "
            f"permanent "
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
            f"tools/{Path(__file__).name}) -- a newly full-access "
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
