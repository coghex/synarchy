#!/usr/bin/env python3
"""Inventory-document and component-topology policy for the
persistence-inventory audit (issue #2124): the ONE owner of how
docs/persistence_state_inventory.md is read and what it must say.

It owns the owner-scoped Markdown classification parser, the
classification taxonomy and its validation, the `### Save components`
owner-row parser, the derivation of the REAL registered component set
from `saveComponentRegistry` and the envelope, the documented-versus-
registered correspondence check, and the `### Test coverage map`
parser and its enforcement.

It consumes extracted FACTS -- inventory text, Haskell source text the
façade already loaded, a derived id set -- and never reads a repository
file itself. Its only local import is
tools/persistence_inventory_audit_common.py (the data-only leaf), which
it needs because several diagnostics name the inventory document and
the component source files; it does not import the façade, and the
scanners (tools/persistence_inventory_audit_haskell.py /
_lua.py) do not import it. The heading names below are the inventory
document's own topology and are read by the façade's `audit()` for its
root-owner, Lua-module and reference diagnostics too, so they live here
exactly once.
"""
from __future__ import annotations

import re
from collections.abc import Set as AbstractSet

import persistence_inventory_audit_common as common

# The `### ` heading text every Lua registration is classified under
# (docs/persistence_state_inventory.md SS7, "Lua persistence registry").
LUA_OWNER_HEADING = "Lua persistence registry"
# The B2 save-component wire contract (#760): the on-disk save is now a
# set of independently-versioned Haskell components riding inside the B1
# envelope, NOT the positional SaveData/WorldPageSave aggregate. The
# authoritative list of registered component ids is discovered from the
# Haskell source persistence_inventory_audit_common.py names (every
# `ComponentId "..."` literal defined there),
# and the inventory's `### Save components` section must classify each.
# A component owner classified as persistent MUST be registered; an owner
# classified rebuilt/reset/excluded needs no registration (requirement 5).
SAVE_COMPONENTS_HEADING = "Save components"
# The `### ` heading the inventory classifies every persistent
# SamePageRef/CrossPageRef DTO field under (issue #764) -- see
# persistence_inventory_audit_haskell.find_typed_reference_fields for
# the discovery side.
REFERENCE_FIELD_OWNER = "Typed persistent references"
# The `### ` heading the inventory classifies every Lua `kind = "..."`
# reference-kind literal under (issue #764) -- see
# persistence_inventory_audit_lua.find_lua_reference_kinds for the
# discovery side.
LUA_REFERENCE_KIND_OWNER = "Lua reference kinds"
# The `### ` heading of the SS12 test coverage map (issue #767).
COVERAGE_MAP_HEADING = "Test coverage map"
BACKTICK_RE = re.compile(r"`([^`]+)`")
OWNER_HEADING_RE = re.compile(r"^###\s+(.+?)\s*$")

# A `ComponentId "..."` literal DEFINED anywhere (e.g. in Types.hs) does
# NOT mean the component is registered -- the authoritative registry is
# the `saveComponentRegistry` list (Component.hs) plus whatever the
# envelope wires in directly (metadata). The registered set is therefore
# DERIVED by tracing that membership, not by grepping id literals (#760
# round-4 review: an id could be defined + documented but never
# registered and still pass a literal-grep audit).
# `<ident> = ComponentId "<literal>"` -- maps an id identifier to its
# on-disk string literal.
COMPONENT_ID_BINDING_RE = re.compile(r'(\w+)\s*=\s*ComponentId\s+"([^"]+)"')
# The `saveComponentRegistry = [ ... ]` list body (the `= [` distinguishes
# the definition from the `∷ [RegisteredComponent]` type signature; the
# list contains no nested `]`).
SAVE_REGISTRY_BLOCK_RE = re.compile(r"saveComponentRegistry\s*=\s*\[(.*?)\]", re.S)
# `registerComponent <codecName>` entries inside that list body.
REGISTER_COMPONENT_RE = re.compile(r"registerComponent\s+(\w+)")
# A component spec tuple `(<idIdent>, <ver>, True|False, ...)` -- how the
# envelope registers the metadata component alongside the gameplay set.
ENVELOPE_SPEC_ID_RE = re.compile(
    r"\(\s*(\w+)\s*,\s*\w+\s*,\s*(?:True|False)\s*,")
# Classifications that mean "this state is written to the save" and so
# require a registered component to own it.
PERSISTENT_CLASSIFICATIONS = (
    "Persist exactly",
    "Persist as identity/reference",
)


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


def is_valid_classification(cell_text: str) -> bool:
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


def parse_component_owner_rows(
        inventory_text: str) -> list[tuple[str, str | None, str]]:
    """Every row under the `### Save components` heading as
    (owner name, declared ComponentId or None, classification cell text).

    The section's table is `| Component | ComponentId | Classification |
    ... |`; the ComponentId + Classification columns are found by their
    own header labels (so extra trailing columns are tolerated).
    """
    rows: list[tuple[str, str | None, str]] = []
    in_section = False
    comp_idx: int | None = None
    class_idx: int | None = None
    for line in inventory_text.splitlines():
        heading = OWNER_HEADING_RE.match(line)
        if heading:
            in_section = heading.group(1) == SAVE_COMPONENTS_HEADING
            comp_idx = None
            class_idx = None
            continue
        if not in_section or not line.startswith("|"):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if class_idx is None and comp_idx is None:
            comp_idx = (cells.index("ComponentId")
                        if "ComponentId" in cells else _NO_CLASSIFICATION_COLUMN)
            class_idx = (cells.index("Classification")
                         if "Classification" in cells else _NO_CLASSIFICATION_COLUMN)
            continue
        if class_idx == _NO_CLASSIFICATION_COLUMN:
            continue
        names = BACKTICK_RE.findall(cells[0]) if cells else []
        if not names:
            continue  # separator row
        cid: str | None = None
        if comp_idx is not None and comp_idx >= 0 and comp_idx < len(cells):
            cid_matches = BACKTICK_RE.findall(cells[comp_idx])
            cid = cid_matches[0] if cid_matches else None
        classification = (cells[class_idx]
                          if class_idx < len(cells) else "")
        rows.append((names[0], cid, classification))
    return rows


def derive_registered_component_ids(
        registry_list_source: str,
        codec_source: str,
        id_types_source: str,
        envelope_source: str = "") -> set[str]:
    """Derive the set of ACTUALLY-registered component id string literals
    from real registry membership, NOT from every `ComponentId "..."`
    literal that happens to be defined (#760 round-4 review).

    The trace:
      1. `saveComponentRegistry = [ registerComponent <codec> ... ]`
         names the registered codecs (Component.hs).
      2. each `<codec> = componentCodec ComponentSpec { csComponent =
         <idIdent>, ... }` names its component id identifier
         (Session/Page/Entities/Knowledge.hs).
      3. `<idIdent> = ComponentId "<literal>"` resolves it to the on-disk
         literal (Types.hs).
    The envelope additionally registers components as direct spec tuples
    (`(<idIdent>, <ver>, True, ...)`, currently just metadata) that live
    OUTSIDE the gameplay registry list; those are legitimately registered
    too, so they are traced from the envelope source.

    A component id literal that is DEFINED (in Types.hs) but reached by
    NONE of these paths is intentionally excluded -- that is exactly the
    "defined + documented but never wired into decode/assembly" gap the
    audit must catch.
    """
    id_by_ident = dict(COMPONENT_ID_BINDING_RE.findall(id_types_source))
    registered: set[str] = set()
    block_m = SAVE_REGISTRY_BLOCK_RE.search(registry_list_source)
    if block_m:
        for codec in REGISTER_COMPONENT_RE.findall(block_m.group(1)):
            m = re.search(
                rf"{re.escape(codec)}\s*=\s*componentCodec\s+ComponentSpec\b"
                rf"\s*\{{[^{{}}]*?csComponent\s*=\s*(\w+)",
                codec_source, re.S)
            if m is None:
                raise ValueError(
                    f"saveComponentRegistry registers '{codec}', but no "
                    f"`{codec} = componentCodec ComponentSpec {{ csComponent "
                    f"= ... }}` definition was found in any of "
                    f"{common.COMPONENT_CODEC_FILES} -- a component declared in a "
                    f"file this scan never looked at, or one that hand-rolls "
                    f"the 'ComponentCodec' record instead of going through "
                    f"the shared construction (issue #1093), would otherwise "
                    f"be silently absent from the registered set, taking its "
                    f"required `### Save components` row with it")
            lit = id_by_ident.get(m.group(1))
            if lit is None:
                raise ValueError(
                    f"codec '{codec}' names component id identifier "
                    f"'{m.group(1)}', which has no `= ComponentId \"...\"` "
                    f"binding in {common.COMPONENT_ID_TYPES_FILE}")
            registered.add(lit)
    for ident in ENVELOPE_SPEC_ID_RE.findall(envelope_source):
        lit = id_by_ident.get(ident)
        if lit is not None:
            registered.add(lit)
    return registered


def parse_coverage_map_component_names(inventory_text: str) -> set[str]:
    """Every backtick-quoted first-column name under the
    `### Test coverage map` heading (docs/persistence_state_inventory.md
    SS12, issue #767 requirement 3) -- a save ComponentId
    (e.g. `core-session`) or a Lua persistence module reference
    (`lua.unit_ai`), one row per persistent §10/§7 owner."""
    names: set[str] = set()
    in_section = False
    for line in inventory_text.splitlines():
        heading = OWNER_HEADING_RE.match(line)
        if heading:
            in_section = heading.group(1) == COVERAGE_MAP_HEADING
            continue
        if not in_section or not line.startswith("|"):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if not cells or set(cells[0]) <= {"-", ":"}:
            continue  # separator row
        names.update(BACKTICK_RE.findall(cells[0]))
    return names


def find_coverage_map_violations(
        inventory_text: str) -> list[str]:
    """Requirement 3 (issue #767, save-overhaul D1): every persistent
    §10 save component and every `Persist exactly` §7 Lua persistence
    module must have a row in the `### Test coverage map` heading (SS12)
    naming its owning component, canonical inspection path, round-trip
    assertion, reset/rebuild assertion, and focused test -- an
    inventory entry with a classification decision but no contract
    coverage entry fails here, mirroring how a missing classification
    decision itself already fails `find_component_registration_violations`/
    the façade's root-owner-field checks."""
    violations: list[str] = []
    covered = parse_coverage_map_component_names(inventory_text)

    for name, cid, classification in parse_component_owner_rows(inventory_text):
        if _classification_core(classification) not in PERSISTENT_CLASSIFICATIONS:
            continue  # rebuilt/reset/excluded owners need no coverage-map row
        if cid is None:
            continue  # already reported by find_component_registration_violations
        if cid not in covered:
            violations.append(
                f"save component {name!r} (ComponentId {cid!r}) is classified "
                f"persistent but has no row under the "
                f"'### {COVERAGE_MAP_HEADING}' heading in {common.INVENTORY_PATH.name} "
                f"-- add a coverage-map entry naming its canonical inspection "
                f"path, round-trip assertion, and focused test "
                f"(docs/persistence_contract.md requirement 3)")

    classified_lua = parse_classified_names(inventory_text).get(LUA_OWNER_HEADING, {})
    for name, classification in classified_lua.items():
        if _classification_core(classification) != "Persist exactly":
            continue  # reset-hook/excluded Lua modules need no coverage-map row
        lua_ref = f"lua.{name}"
        if lua_ref not in covered:
            violations.append(
                f'Lua save module "{name}" is classified persistent but has '
                f"no row (as {lua_ref!r}) under the "
                f"'### {COVERAGE_MAP_HEADING}' heading in {common.INVENTORY_PATH.name} "
                f"-- add a coverage-map entry naming its canonical inspection "
                f"path, round-trip assertion, and focused test "
                f"(docs/persistence_contract.md requirement 3)")

    return violations


def find_component_registration_violations(
        inventory_text: str, registered: AbstractSet[str]) -> list[str]:
    """The #760 registry linkage check: every save-component owner the
    inventory classifies as PERSISTENT must map to an ACTUALLY-registered
    `ComponentId` (see `derive_registered_component_ids`), while an owner
    classified rebuilt/reset/excluded requires no registration; and
    conversely every registered component must have a persistent-classified
    inventory row (so a new component owner can't land without an explicit
    decision).
    """
    violations: list[str] = []
    documented: set[str] = set()
    for name, cid, classification in parse_component_owner_rows(inventory_text):
        core = _classification_core(classification)
        if core not in PERSISTENT_CLASSIFICATIONS:
            continue  # rebuilt/reset/excluded owners need no registration
        if cid is None:
            violations.append(
                f"save component {name!r} is classified persistent "
                f"({classification!r}) but its row declares no ComponentId "
                f"under the '### {SAVE_COMPONENTS_HEADING}' heading in "
                f"{common.INVENTORY_PATH.name}")
            continue
        documented.add(cid)
        if cid not in registered:
            violations.append(
                f"save component {name!r} is classified persistent "
                f"({classification!r}) but its ComponentId {cid!r} is not "
                f"registered in the Haskell save-component registry "
                f"(saveComponentRegistry in {common.COMPONENT_REGISTRY_LIST_FILE}, "
                f"or the envelope's own component set)")
    for cid in sorted(registered):
        if cid not in documented:
            violations.append(
                f"registered save component {cid!r} has no persistent-"
                f"classified row under the '### {SAVE_COMPONENTS_HEADING}' "
                f"heading in {common.INVENTORY_PATH.name} -- add a classification "
                f"decision for it (see docs/persistence_contract.md)")
    return violations
