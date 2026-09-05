#!/usr/bin/env python3
"""Checked-in authority and policy for the SS5 writing-module scanner
(issue #1892, capability mutation-authority epic #1890 -- CMA-1;
extracted from tools/engine_env_capability_writers.py by issue #2230).

This owner holds every decision the scan is measured AGAINST and no
mechanism for measuring it: the recognized `IORef` and read-only-ref
primitives and the modules they must reach a consumer from, the
capability-module prefix, and the two checked-in authorities
`SHADOW_EXEMPTIONS` and `CAPABILITY_WRITER_MODULES` -- together with
the two blocking checks that read them, `audit_shadow_exemptions` and
`audit_writer_modules`. Keeping the map and its both-directions check
in one module is deliberate: a stale entry and an undeclared write are
the same authority question asked in opposite directions.

It imports nothing from the other three writer owners or from the
facade -- `engine_env_capability_writers.py` re-exports this surface
and is the only module the aggregate audit and the focused self-test
import. Dependencies run one way: authority, then syntax and
projections over it, then scan over all three.
"""
from __future__ import annotations

from engine_env_capability_common import (  # type: ignore
    INVENTORY_PATH, _is_placeholder,
)

# Every diagnostic below names the STABLE FACADE path rather than this
# owner's own filename (issue #2230 requirement 19). `audit_writer_modules`
# computed `Path(__file__).name` while it lived in the facade, where that
# resolved to exactly this string; from here it would resolve to
# `engine_env_capability_writer_authority.py` and silently send a reader
# looking for `CAPABILITY_WRITER_MODULES` to a file the rest of the
# repository never names. The map is reached through the facade, so the
# facade is what a violation cites.
WRITER_FACADE_FILENAME = "engine_env_capability_writers.py"


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
# REPORTS, as the non-blocking residue
# (`engine_env_capability_writer_scan.py`).
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
# the residue classification in
# `engine_env_capability_writer_scan.py`.
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
        "Unit.Thread",
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
        # #2328: the strike's own writes moved into
        # Combat.Resolution.Admission.commitIfAdmitted, which applies
        # them in the SAME transaction that re-checks the strike's
        # preconditions. Combat.Resolution now only READS the manager
        # (its snapshot early-out); Wear still writes it for weapon and
        # armour wear.
        "Combat.Resolution.Admission",
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
        # #2328: combat resolution no longer draws from the shared pool
        # at all. Combat.Thread splits a strike stream off it ONCE at
        # worker startup -- the only write -- and carries that stream in
        # its loop state, so a refused strike advances nothing here.
        "Combat.Thread",
        "Combat.Wounds.Tick",
        "Engine.Scripting.Lua.API.Forage.Harvest",
        "Engine.Scripting.Lua.API.Units.Stats",
        "Unit.Thread.Command.Spawn",
        "Unit.Thread.Movement.Climb",
    }),
    # #2297: medical treatment draws from its OWN generator, not the
    # shared stat pool -- a treatment commits or refuses in one
    # unit-manager transaction, so it has to claim a generator without
    # advancing anything a refusal would then have to unwind. One
    # writer by contract; the inventory row says why.
    "treatRNGRef": frozenset({
        "Engine.Scripting.Lua.API.Units.Medical",
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
        # #2326: a page-bound placement whose binding went stale is
        # dropped on the world thread, and that drop retires the
        # footprint claim `building.spawn` took for it.
        "World.Thread.Command.BoundSpawn",
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
    audit_name = WRITER_FACADE_FILENAME
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
