#!/usr/bin/env python3
"""SS5 writing-module scanner for the EngineEnv capability audit
(issue #1892, capability mutation-authority epic #1890 -- CMA-1;
extracted from tools/engine_env_capability_audit.py by issue #2036).

`tools/engine_env_capability_audit.py` remains the gate CI and
`tools/ci-local.sh` run; this module is the half of it that pins each
`EngineEnv` field's direct WRITING MODULES, and it is reached only
through that aggregate. SS5's Writers cells are prose validated for
grammar and citation presence only, so until this check the doc could
claim a field has no writers at all and a change falsifying it passed
every gate -- the drift class #1669 closed for the field COUNT, still
open for the ownership CLAIMS. `CAPABILITY_WRITER_MODULES` is a
checked-in, both-directions map of the same shape as the
structural-boundary owner's `RENDER_MAIN_ONLY_MODULES`
(tools/engine_env_capability_boundaries.py since issue #2064): an
undeclared write fails, a stale entry fails, and the map's KEYS must
equal the live field set. It scans
DIRECT `IORef` mutation only, through the raw `EngineEnv` accessor and
through any capability-record accessor projecting it alike; SS6.1's
permanent cohort is exempt (design decision D-4); and every
capability-accessor use the scan cannot attribute -- a handle passed
to a helper, stored in a context record, or handed to a
queue/`TVar`/`MVar` -- is printed as the non-blocking pass-on residue
(D-5), which the aggregate prints ahead of every blocking check so a
failure elsewhere never costs the measurement. See SS6.5 of the
inventory doc and docs/capability_mutation_authority_design.md.

Since issue #2059 the ownership map behind that scan is also derived
FAIL-CLOSED. The map is built entirely from the live projections, so
anything the parser failed to read simply was not in it -- and a write
through the missing selector then resolved to no field, was filed as
`other`, and left the writing-module map, the residue and requirement
6's closed-form check while the gate still exited 0. Two changes close
that: projection right-hand sides are canonicalized STRUCTURALLY
(`canonical_projection_accessor`, in
tools/engine_env_capability_common.py), so semantically inert grouping
-- `(accessor env)`, `(accessor) env`, `wrapper ((accessor env))` --
canonicalizes exactly as its ungrouped spelling does; and
`audit_capability_projection_completeness` requires every field of
every live capability record to reach a live `EngineEnv` accessor,
naming the module, projection and field when one does not. Widening
what canonicalizes stays deliberately bounded (no dataflow, type or
scope analysis); what changed is that an unread binding now FAILS
instead of disappearing.

What this module OWNS, top to bottom: the recognized mutation and
read primitives and the modules they must come from; the two
checked-in authorities, `SHADOW_EXEMPTIONS` and
`CAPABILITY_WRITER_MODULES`; the Haskell tokenizer and import
resolver (`tokenize_haskell`, `parse_imports`, `imports_name`); the
capability-record discovery and accessor canonicalization
(`discover_capability_records`, `capability_accessor_map`) and its
fail-closed completeness audit; mutation-site classification
(`classify_mutation_site` and the head/argument helpers under it);
the scan itself (`scan_capability_writes`); the three blocking checks
(`audit_mutation_sites`, `audit_shadow_exemptions`,
`audit_writer_modules`); and the residue report (`format_residue`).
What it READS from tools/engine_env_capability_common.py, and does not
own: the inventory-doc anchor, SS6.1's permanent set, comment
stripping, import chunking, module naming and the projection
canonicalizer. It imports nothing from the aggregate.

Every function here is pure over the `{relpath: source_text}` map and
the ordered live-field list the aggregate hands it, so the focused
self-test (tools/test_engine_env_capability_writers.py) drives it
against synthetic trees, and the production tree is scanned exactly
once per audit run, by the aggregate.

Not independently a gate: `python3 tools/engine_env_capability_audit.py`
is the one command that runs this, and adding a second invocation to
CI or `tools/ci-local.sh` would fail tools/ci_parity_audit.py's
command-set comparison for nothing.
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import NamedTuple

from engine_env_capability_common import (  # type: ignore
    ALIAS_PRESERVING_WRAPPERS, ENGINE_ENV_TYPE, INVENTORY_PATH,
    PERMANENT_DEFINER, PERMANENT_IMPORTERS, STATE_MODULE,
    _CHAR_LITERAL_RE, _import_chunks, _is_placeholder,
    _strip_haskell_comments, canonical_projection_accessor,
    extract_record_fields, module_identifier,
    parse_projection_binding_expressions, parse_projection_bindings,
)

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
    # The six workers dropped off this list in #2283: the fail-stop
    # transition they each used to write at the end of their own crash
    # callback is now `Engine.Core.Thread`'s, performed through
    # `requestEngineCleanup` on the bare `IORef` the `WorkerSpec`
    # carries, before any of their reporting runs. Nothing in a worker
    # writes `lifecycleRef` directly any more.
    "lifecycleRef": frozenset({
        "Engine.Loop.Mode",
        "Engine.Scripting.Lua.API.Core",
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
    # #2020: published once, from the single Vulkan-init funnel both
    # the windowed and the offscreen device-creation paths pass through.
    "maxImageDimensionRef": frozenset({
        "Engine.Graphics.Vulkan.Init",
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
        "Engine.Scripting.Lua.API.Construct.Payment",
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


# One discovery feeds BOTH the accessor map and the completeness gate
# below (issue #2059), so the map can never quietly describe a smaller
# set of records than the gate checks -- which is the shape the
# original hole took: `capability_accessor_map` was the only thing
# that read the projections, and anything it failed to read simply was
# not there.
#
# A capability type is recognized by its NAME and its `data`/`newtype`
# keyword alone, never by the shape of its body. GHC2024 enables
# `GADTs`, so `data XCapability where XCapability ∷ { ... } → XCapability`
# is a legal respelling of the same record, and a `newtype` is legal
# for a one-field one -- matching only `data X = X { ... }` left both
# undiscovered, which is the SAME silent omission one level up:
# neither the accessor map nor the completeness gate saw the record at
# all, so a direct write through its selector was filed as `other` and
# the audit exited 0. Recognizing the declaration is therefore
# separated from reading its fields: a capability type whose record
# block this audit cannot read is a violation, not a skip.
# Any layout COLUMN, because a module's body need not start at column
# zero, but only the two plain declaration keywords: a `data`/`newtype`
# `instance` or `family` naming a capability type is deliberately NOT
# read here. It is a form SS2.1's convention does not describe, so the
# backstop below reports it loudly instead -- "detect and fail" rather
# than a modelling branch nothing in the tree exercises.
_CAPABILITY_TYPE_DECL_PATTERN = (
    r"^[ \t]*(?:data|newtype)\s+%s(?![A-Za-z0-9_'])")
_CAPABILITY_TYPE_DECL_RE = re.compile(
    _CAPABILITY_TYPE_DECL_PATTERN
    % r"(?P<record>[A-Z][A-Za-z0-9_']*Capability)", re.MULTILINE)

# The fail-closed BACKSTOP for this whole discovery. Everything above
# recognizes the spellings this audit models; this recognizes that a
# capability type was DECLARED at all, by looking only for the
# `data`/`newtype` keyword and a `<Name>Capability` type name in the
# same declaration head. `[^\n=]` keeps it inside that head, so a field
# whose TYPE is a capability (`{ x ∷ RenderCapability }`, after the
# `=` or on a later line) is never mistaken for a declaration of one.
#
# Any loose match the strict pattern did not produce is a form this
# audit cannot read, and it is reported rather than skipped. That is
# what makes the discovery closed the way SS6.5's recognized write
# forms are closed: the NEXT unmodelled spelling -- whatever it turns
# out to be -- fails loudly instead of quietly taking a record out of
# the accessor map, so no legal respelling can leave a selector
# unenforced while the gate exits 0.
_LOOSE_CAPABILITY_DECL_RE = re.compile(
    r"(?<![A-Za-z0-9_'])(?:data|newtype)(?![A-Za-z0-9_'])[^\n=]{0,160}?"
    r"(?<![A-Za-z0-9_'])(?P<record>[A-Z][A-Za-z0-9_']*Capability)"
    r"(?![A-Za-z0-9_'])")


def _declaration_span(code: str, start: int) -> str:
    """`code` from the start of `start`'s LINE through the end of that
    declaration: its own line plus every following line that is blank
    or indented strictly PAST the declaration's own layout column,
    which is Haskell's layout rule for one item of a block.

    The column is read from the declaration rather than assumed to be
    zero, because a module whose body is uniformly indented puts every
    top-level declaration at the same non-zero column -- and treating
    column zero as the boundary there would run one declaration's span
    to the end of the file.

    Field extraction is bounded to this span so a declaration carrying
    no record block of its own cannot borrow the braces of a LATER
    declaration and report that one's fields as its own."""
    line_start = code.rfind("\n", 0, start) + 1
    lines = code[line_start:].split("\n")
    # The column is the declaration KEYWORD's, not the match's: the
    # pattern anchors before the leading whitespace, so measuring from
    # `start` would read every indented declaration as column zero and
    # run its span to the end of the file.
    column = len(lines[0]) - len(lines[0].lstrip())
    span = [lines[0]]
    for line in lines[1:]:
        if line.strip() and len(line) - len(line.lstrip()) <= column:
            break
        span.append(line)
    return "\n".join(span)


# Every field-carrying constructor's block is read, not just the
# first. A sum of records -- `data X = A { f ∷ … } | B { f ∷ …, g ∷ … }`,
# or a GADT declaring one record constructor per line -- puts EVERY
# constructor's selectors in one scope, so reading only the first block
# left `g` unenumerated and therefore unchecked: the completeness gate
# had nothing to say about it, and a projection binding it through
# anything this audit cannot read took it out of the accessor map
# silently, which is the exact failure mode #2059 exists to close.
#
# The shared field parser is CALLED per block rather than paraphrased,
# so grouped declarations (`{ a, b ∷ Int }`) and split
# name/signature lines keep behaving identically to every other record
# this tree parses.
_RECORD_BLOCK_HEAD = "data CapabilityRecordBlock = CapabilityRecordBlock "
_RECORD_BLOCK_PATTERN = (
    r"^data CapabilityRecordBlock = CapabilityRecordBlock\b")


def _record_blocks(span: str) -> list[str]:
    """Every top-level `{ ... }` block in one declaration's span, in
    source order -- one per field-carrying constructor."""
    blocks: list[str] = []
    depth = 0
    start = 0
    for index, character in enumerate(span):
        if character == "{":
            if depth == 0:
                start = index
            depth += 1
        elif character == "}" and depth > 0:
            depth -= 1
            if depth == 0:
                blocks.append(span[start:index + 1])
    return blocks


def capability_record_fields(source_text: str, record: str) -> list[str]:
    """The field names `record`'s own declaration brings into scope,
    whichever legal syntax declares it -- `data X = X { ... }`,
    `newtype X = X { ... }`, the GADT `data X where X ∷ { ... } → X`,
    or a SUM of record constructors. All of them put the same kind of
    selector in scope, so all of them must be read; a name declared by
    more than one constructor is one selector and is reported once, in
    first-declaration order.

    Raises `ValueError` when the declaration is absent or carries no
    record block at all, which the completeness audit reports rather
    than treating as a record with no fields."""
    code = _strip_haskell_comments(source_text)
    declaration = _CAPABILITY_TYPE_DECL_RE.search(code)
    while declaration is not None and declaration.group("record") != record:
        declaration = _CAPABILITY_TYPE_DECL_RE.search(code, declaration.end())
    if declaration is None:
        raise ValueError(
            f"no `data` or `newtype` declaration of `{record}` was found")
    blocks = _record_blocks(_declaration_span(code, declaration.start()))
    if not blocks:
        raise ValueError(
            f"`{record}`'s declaration carries no record block of its "
            f"own")
    fields: list[str] = []
    for block in blocks:
        for field in extract_record_fields(_RECORD_BLOCK_HEAD + block,
                                           _RECORD_BLOCK_PATTERN):
            if field not in fields:
                fields.append(field)
    return fields


def _capability_projection_re(record: str) -> re.Pattern[str]:
    """`to<Something> ∷ EngineEnv → <record>`, the SS2.1 projection
    signature, ASCII and Unicode arrows alike."""
    return re.compile(
        r"^[ \t]*(to[A-Za-z0-9_']*)\s*(?:∷|::)\s*"
        r"(?:[A-Z][A-Za-z0-9_']*\.)*EngineEnv\s*(?:→|->)\s*"
        rf"{re.escape(record)}(?![A-Za-z0-9_'])", re.MULTILINE)


class CapabilityRecord(NamedTuple):
    """One `Engine.Core.Capability.*` record declaration and the
    projection that builds it. `projection` is `None` when the module
    declares the record but no `EngineEnv → <record>` signature was
    found -- a state that is itself a violation, never a skip."""
    module: str
    relpath: str
    record: str
    projection: str | None


def discover_capability_records(sources: dict[str, str]
                                ) -> list[CapabilityRecord]:
    """Every `<Name>Capability` record declared under
    `Engine.Core.Capability.*`, paired with its projection, in module
    then declaration order.

    Comments are stripped first, so a Haddock example showing a record
    or a signature is not mistaken for the real declaration.

    A declaration this pattern cannot read is NOT here -- it is
    reported by `undiscovered_capability_declarations`, which the
    completeness audit fails on. Read the two together: this answers
    "what did we understand?", that one answers "did we understand
    everything?", and only the pair is fail-closed."""
    records: list[CapabilityRecord] = []
    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        if not module.startswith(CAPABILITY_MODULE_PREFIX):
            continue
        code = _strip_haskell_comments(text)
        for declaration in _CAPABILITY_TYPE_DECL_RE.finditer(code):
            record = declaration.group("record")
            signature = _capability_projection_re(record).search(code)
            records.append(CapabilityRecord(
                module, relpath, record,
                signature.group(1) if signature else None))
    return records


def undiscovered_capability_declarations(sources: dict[str, str]
                                         ) -> list[tuple[str, str, str]]:
    """`(module, relpath, record)` for every capability type a
    `data`/`newtype` declaration head names that
    `discover_capability_records` did NOT produce.

    This is the backstop that makes the discovery a CLOSED set rather
    than a list of spellings that happened to be thought of. Every hole
    #2059 has closed had the same shape -- a legal declaration the
    pattern did not match, so the record reached neither the accessor
    map nor the completeness gate and a direct write through its
    selector was filed as `other` while the audit exited 0. Naming the
    keyword and the type is enough to know a capability record is
    THERE; whether this audit can read its fields is a separate
    question, and the honest answer to "no" is to fail."""
    missed: list[tuple[str, str, str]] = []
    discovered = {(entry.relpath, entry.record)
                  for entry in discover_capability_records(sources)}
    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        if not module.startswith(CAPABILITY_MODULE_PREFIX):
            continue
        code = _strip_haskell_comments(text)
        seen: set[str] = set()
        for match in _LOOSE_CAPABILITY_DECL_RE.finditer(code):
            record = match.group("record")
            if record in seen or (relpath, record) in discovered:
                continue
            seen.add(record)
            missed.append((module, relpath, record))
    return missed


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
    boundary checks are where a mis-bound projection is caught, and
    `audit_capability_projection_completeness` is what stops such a
    skip from being SILENT."""
    fields = set(live_fields)
    candidates: dict[str, set[tuple[str, str, str]]] = {}
    for entry in discover_capability_records(sources):
        if entry.projection is None:
            continue
        for capability_field, accessor in parse_projection_bindings(
                sources[entry.relpath], entry.projection).items():
            if accessor in fields:
                candidates.setdefault(capability_field, set()).add(
                    (accessor, entry.module, entry.record))
    return {name: tuple(sorted(owners, key=lambda entry: entry[1]))
            for name, owners in candidates.items()}


def audit_capability_projection_completeness(
    sources: dict[str, str], live_fields: list[str],
) -> list[str]:
    """Issue #2059's fail-closed half: every field of every live
    capability record must canonicalize onto a live `EngineEnv` field,
    or the audit STOPS and names the module, projection and field.

    `capability_accessor_map` is the whole ownership map behind
    `CAPABILITY_WRITER_MODULES` enforcement, the SS6.5 residue and
    requirement 6's closed-form safety check. Before this check, a
    field the parser could not read was simply absent from that map:
    every direct write through the selector resolved to no field, was
    filed as `other`, and disappeared from all three while the gate
    exited 0. A silent omission is therefore indistinguishable from a
    field nobody writes -- so there must be none, and each of the three
    ways one can arise is reported here:

    * the record's DECLARATION is in a form the discovery pattern
      cannot read, which loses the record entirely
      (`undiscovered_capability_declarations`);
    * the record's projection signature is not found at all, which
      loses every one of its fields at once;
    * a declared field has no binding the canonicalizer can read
      (`canonical_projection_accessor`) -- an unrecognized wrapper, an
      operator, a record update, or no binding in the construction;
    * a binding canonicalizes onto a name that is NOT a live
      `EngineEnv` field, which `capability_accessor_map` discards at
      the same cost (the reviewer's amendment to requirement 2).

    This does not widen what canonicalizes. Reading MORE spellings is
    `canonical_projection_accessor`'s job and stays deliberately
    bounded; this check only refuses to let an unread one pass as
    nothing."""
    fields = set(live_fields)
    violations: list[str] = []
    for module, relpath, record in undiscovered_capability_declarations(
            sources):
        violations.append(
            f"`{module}` declares `{record}` in a form this audit cannot "
            f"read ({relpath}) -- the `data`/`newtype` declaration is "
            f"there, but `discover_capability_records` did not produce "
            f"the record, so it reaches neither the capability accessor "
            f"map nor the checks below and every direct write through "
            f"one of its selectors would be filed as `other`. Teach "
            f"`_CAPABILITY_TYPE_DECL_PATTERN` the spelling, or restate "
            f"the declaration in one it reads; do NOT leave it "
            f"undiscovered, because an undiscovered record is an "
            f"unenforced one")
    for entry in discover_capability_records(sources):
        source = sources[entry.relpath]
        if entry.projection is None:
            violations.append(
                f"`{entry.module}` declares `{entry.record}` but no "
                f"`to... ∷ EngineEnv → {entry.record}` projection was "
                f"found ({entry.relpath}) -- without it EVERY selector of "
                f"the record is absent from the capability accessor map, "
                f"so every direct write through one is filed as `other` "
                f"and silently leaves SS5 writing-module enforcement. Give "
                f"the record its SS2.1 projection, or teach "
                f"`discover_capability_records` the spelling")
            continue
        try:
            declared = capability_record_fields(source, entry.record)
        except ValueError as error:
            violations.append(
                f"`{entry.module}`'s `{entry.record}` declares no record "
                f"block this audit can read ({entry.relpath}): {error} -- "
                f"SS2.1 requires a record whose every field projects an "
                f"`EngineEnv` handle, and a declaration whose selectors "
                f"cannot be enumerated puts every one of them outside "
                f"SS5's writing-module map")
            continue
        expressions = parse_projection_binding_expressions(
            source, entry.projection)
        for field in declared:
            expression = expressions.get(field)
            if expression is None:
                violations.append(
                    f"`{entry.module}`'s `{entry.projection}` binds no "
                    f"readable right-hand side for `{entry.record}`'s "
                    f"`{field}` ({entry.relpath}) -- a field the parser "
                    f"cannot pair with an `EngineEnv` accessor is missing "
                    f"from the capability accessor map, which silently "
                    f"exempts every direct write through `{field}` from "
                    f"SS5's writing-module map")
                continue
            accessor = canonical_projection_accessor(expression)
            if accessor is None:
                violations.append(
                    f"`{entry.module}`'s `{entry.projection}` binds "
                    f"`{field}` as `{expression}`, which this audit "
                    f"cannot canonicalize onto an `EngineEnv` accessor "
                    f"({entry.relpath}) -- SS2.1 requires every field to "
                    f"be the live handle an accessor names, spelled "
                    f"`accessor env` or `wrapper (accessor env)` for a "
                    f"named alias-preserving wrapper "
                    f"({', '.join(sorted(ALIAS_PRESERVING_WRAPPERS))}), "
                    f"with grouping optional. Restate the binding in a "
                    f"recognized form, or extend "
                    f"`canonical_projection_accessor` and SS2.1 together; "
                    f"do NOT leave it unread, because an unread binding "
                    f"is an unenforced field")
                continue
            if accessor not in fields:
                violations.append(
                    f"`{entry.module}`'s `{entry.projection}` binds "
                    f"`{field}` from `{accessor}`, which is not a live "
                    f"`EngineEnv` field ({entry.relpath}) -- "
                    f"`capability_accessor_map` drops a binding it cannot "
                    f"canonicalize onto the live record, so a renamed or "
                    f"mistyped accessor would take `{field}` out of SS5's "
                    f"writing-module map without failing anything")
    return violations


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

