-- Script loading + shutdown bookkeeping for scripts/init.lua (#543).
-- Owns every long-lived script id game.init registers and the
-- teardown order game.shutdown tears them back down in.
local M = {}

local shellScriptId = nil
local uiScriptId = nil
local debugScriptId = nil
local debugAnimPanelScriptId = nil
local unitManagerScriptId = nil
local unitInfoPanelScriptId = nil
local unitInfoV2ScriptId = nil
local unitDragSelectScriptId = nil
local unitResourcesScriptId = nil
local unitAiScriptId = nil
local buildToolScriptId = nil
local buildingSpawnScriptId = nil
local tileEditorScriptId = nil
local locationStamperScriptId = nil
local pauseScriptId = nil
local buildingInfoPanelScriptId = nil
local itemInfoPanelScriptId = nil
local cargoInventoryPanelScriptId = nil
local itemContentsPanelScriptId = nil
local craftingPanelScriptId = nil
local plantPanelScriptId = nil
local popupScriptId = nil
local eventLogScriptId = nil
local combatLogScriptId = nil
local injuryLogScriptId = nil
local thoughtLogScriptId = nil
local unitLogScriptId = nil
local previewScriptId = nil
local isPreview = false

function M.isPreview()
    return isPreview
end

function M.load()
    -- A structurally distinct thread topology (window + Vulkan, no
    -- world/unit/sim/combat threads) with its own minimal Lua entry
    -- point. Skip every normal gameplay/UI script below -- none of it
    -- has anything to attach to in this profile.
    if engine.getBootProfile() == "preview" then
        isPreview = true
        previewScriptId = engine.loadScript("scripts/preview_manager.lua", 0.1)
        return
    end

    isPreview = false

    -- UI widget introspection oracle (F3, #645): a plain query module
    -- with no per-tick work of its own, so a require (not loadScript)
    -- is enough. Wiring the bare `ui` global here — rather than as a
    -- side effect inside registry.lua — keeps requiring the module
    -- elsewhere side-effect-free.
    ui = require("scripts.ui.registry")

    -- Initialize debug
    debugScriptId = engine.loadScript("scripts/debug.lua", 0.1)

    -- Debug anim panel — pops a green-text overlay listing the
    -- selected unit's animations. 0.1s tick is enough to react to
    -- selection changes without burning CPU.
    debugAnimPanelScriptId = engine.loadScript(
        "scripts/debug_anim_panel.lua", 0.1)

    -- Initialize shell
    shellScriptId = engine.loadScript("scripts/shell.lua", 0.5)

    -- Initialize unit manager (loads unit definitions from YAML)
    unitManagerScriptId = engine.loadScript("scripts/unit_manager.lua", 0.1)

    -- Initialize unit info panel (shown when a unit is selected).
    -- Ticks at 0.1s — slow enough to be cheap, fast enough to feel
    -- responsive when selection changes via click.
    unitInfoPanelScriptId = engine.loadScript("scripts/unit_info_panel.lua", 0.1)

    -- Unit info v2 (the new full-height right-edge pane). Owns unit
    -- info display while active; unit_info_panel above auto-suppresses
    -- its push path when this module is loaded. 30 Hz tick — needed
    -- for the portrait sprite to track the unit's animation smoothly.
    unitInfoV2ScriptId = engine.loadScript("scripts/unit_info_v2.lua", 0.03)

    -- Drag-box selection: ticks at 0.03s so the rect tracks the
    -- mouse smoothly without hammering every frame.
    unitDragSelectScriptId = engine.loadScript(
        "scripts/unit_drag_select.lua", 0.03)

    -- Unit resources (stamina drain/regen, collapse-on-low-stamina).
    -- 0.1s tick is enough — stamina changes on the order of seconds.
    unitResourcesScriptId = engine.loadScript(
        "scripts/unit_resources.lua", 0.1)

    -- Unit AI (utility-based decision loop). Same tick as resources;
    -- per-unit nextActionAt gates actual decisions to ~1s cadence
    -- with jitter, so this isn't doing meaningful work every tick.
    unitAiScriptId = engine.loadScript(
        "scripts/unit_ai.lua", 0.1)

    -- Build tool: drives the popup picker + placement ghost preview.
    -- 0.03s tick so the ghost tracks the mouse smoothly.
    buildToolScriptId = engine.loadScript(
        "scripts/build_tool.lua", 0.03)

    -- Building spawn sequencer: watches placed buildings, spawns the
    -- starting unit roster one-at-a-time after the appear anim
    -- finishes. 0.2s tick (5Hz) — only needs to react fast enough to
    -- feel snappy when the previous unit clears the spawn tile.
    buildingSpawnScriptId = engine.loadScript(
        "scripts/building_spawn.lua", 0.2)

    -- Tile editor: arena-only delete-tile popup. Event-driven — its
    -- per-tick update() is a no-op. We still loadScript so the engine
    -- broadcasts (onSetInfoText) reach the module.
    tileEditorScriptId = engine.loadScript("scripts/tile_editor.lua", 0.1)

    -- Location stamper (#89): on the engine's onWorldReady broadcast it
    -- materializes the placed-location overlay into geometry via the #88
    -- builders, stamping each as its chunk loads. Loaded so onWorldReady +
    -- update() reach it.
    locationStamperScriptId = engine.loadScript("scripts/location_stamper.lua", 0.1)

    -- Pause: owns the engine.setPaused flag + world.setTimeScale
    -- snapshot. No per-tick work; loaded so engine broadcasts (none
    -- needed today) and require()s from game scripts share state.
    pauseScriptId = engine.loadScript("scripts/pause.lua", 1.0)

    -- Building info watcher: mirrors unit_info_panel. Polls
    -- building.getSelected each tick and pushes a building schema
    -- to the HUD info panel.
    buildingInfoPanelScriptId = engine.loadScript(
        "scripts/building_info_panel.lua", 0.1)

    -- Ground-item info watcher: same pattern for items lying in the
    -- world (selection outline is engine-side; this drives the panel).
    itemInfoPanelScriptId = engine.loadScript(
        "scripts/item_info_panel.lua", 0.1)

    -- Cargo inventory popup: floating tabbed list shown by
    -- right-click → "Contents" on a storage-capable building.
    -- 0.2s tick — only refreshes on content-hash change (deposits
    -- / withdrawals), and the cargo's auto-deposit cadence is
    -- already ~1s, so polling cheaper than this is wasted.
    cargoInventoryPanelScriptId = engine.loadScript(
        "scripts/cargo_inventory_panel.lua", 0.2)

    -- Item contents popup: the unit-carried analogue of the cargo
    -- panel — right-click "Contents" on a container item (first-aid
    -- kit / toolbox) in a unit's inventory. Same cheap content-hash
    -- refresh cadence.
    itemContentsPanelScriptId = engine.loadScript(
        "scripts/item_contents_panel.lua", 0.2)

    -- Crafting station bills popup (#330): the player-facing view onto
    -- a station's craft-bill queue — right-click "Bills" on a building
    -- offering a craft operation. 0.2s tick; the module throttles its
    -- own queue-progress refresh to ~1s internally.
    craftingPanelScriptId = engine.loadScript(
        "scripts/crafting_panel.lua", 0.2)

    -- Planting screen (#335): the suitability-sorted crop catalogue the
    -- plant tool opens on a tilled tile. Same 0.2s tick as the other
    -- HUD popups (only its search-box polling needs it).
    plantPanelScriptId = engine.loadScript(
        "scripts/plant_panel.lua", 0.2)

    -- Popup: receives engine.emitEvent broadcasts (onShowPopup) and
    -- renders OK-dismissable popups. Slow tick (1.0s) — render work
    -- is event-driven on click/broadcast, the tick is just here so
    -- the script is registered with the Lua thread for broadcast
    -- delivery. ui_manager calls popup.bootstrap once fonts/textures
    -- load.
    popupScriptId = engine.loadScript("scripts/popup.lua", 1.0)

    -- Event log panel: large modal-layer history view, opened via the
    -- top-left HUD button. No broadcasts subscribed yet; the slow
    -- tick keeps the module registered for future use (e.g. live
    -- refresh on new events).
    eventLogScriptId = engine.loadScript("scripts/event_log.lua", 1.0)

    -- Combat log panel: sibling to event_log, surfaced via right-
    -- click on the HUD log icon. update() drains combat.drainEvents
    -- every tick, groups into battles, renders rows. 0.1s gives
    -- combat events sub-second latency in the log without burning
    -- CPU.
    combatLogScriptId = engine.loadScript("scripts/combat_log.lua", 0.1)

    -- Injury log panel: sibling to combat_log, third HUD log mode.
    -- update() drains injury.drainEvents every tick (falls / hazards /
    -- wound-caused deaths), groups by injured unit, renders rows. MUST
    -- run even when hidden so the engine injury stream is drained and
    -- doesn't grow unbounded.
    injuryLogScriptId = engine.loadScript("scripts/injury_log_panel.lua", 0.1)

    -- Thought log store: sibling data source for unit_log.lua's Thought
    -- tab (#351) — no standalone panel of its own. update() drains
    -- thought.drainEvents every tick into per-unit rings; MUST run even
    -- when no unit-log panel is open, same reason as injury above.
    thoughtLogScriptId = engine.loadScript("scripts/thought_log.lua", 0.1)

    -- Per-unit log panel: opened from the unit-info "Log" button. It owns
    -- no stream (reads the other logs' stores); the tick just re-renders
    -- it live while it's open. 0.3s is plenty for a passive viewer.
    unitLogScriptId = engine.loadScript("scripts/unit_log.lua", 0.3)

    -- Initialize UI (which loads the main menu)
    uiScriptId = engine.loadScript("scripts/ui_manager.lua", 0.1)
end

function M.shutdown()
    if previewScriptId then
        engine.killScript(previewScriptId)
    end
    if debugScriptId then
        engine.killScript(debugScriptId)
    end
    if debugAnimPanelScriptId then
        engine.killScript(debugAnimPanelScriptId)
    end
    if unitInfoV2ScriptId then
        engine.killScript(unitInfoV2ScriptId)
    end
    if unitResourcesScriptId then
        engine.killScript(unitResourcesScriptId)
    end
    if unitAiScriptId then
        engine.killScript(unitAiScriptId)
    end
    if buildToolScriptId then
        engine.killScript(buildToolScriptId)
    end
    if buildingSpawnScriptId then
        engine.killScript(buildingSpawnScriptId)
    end
    if cargoInventoryPanelScriptId then
        engine.killScript(cargoInventoryPanelScriptId)
    end
    if itemContentsPanelScriptId then
        engine.killScript(itemContentsPanelScriptId)
    end
    if craftingPanelScriptId then
        engine.killScript(craftingPanelScriptId)
    end
    if plantPanelScriptId then
        engine.killScript(plantPanelScriptId)
    end
    if shellScriptId then
        engine.killScript(shellScriptId)
    end
    if unitManagerScriptId then
        engine.killScript(unitManagerScriptId)
    end
    if unitInfoPanelScriptId then
        engine.killScript(unitInfoPanelScriptId)
    end
    if unitDragSelectScriptId then
        engine.killScript(unitDragSelectScriptId)
    end
    if uiScriptId then
        engine.killScript(uiScriptId)
    end
    if tileEditorScriptId then
        engine.killScript(tileEditorScriptId)
    end
    if locationStamperScriptId then
        engine.killScript(locationStamperScriptId)
    end
    if pauseScriptId then
        engine.killScript(pauseScriptId)
    end
    if buildingInfoPanelScriptId then
        engine.killScript(buildingInfoPanelScriptId)
    end
    if itemInfoPanelScriptId then
        engine.killScript(itemInfoPanelScriptId)
    end
    if popupScriptId then
        engine.killScript(popupScriptId)
    end
    if eventLogScriptId then
        engine.killScript(eventLogScriptId)
    end
    if combatLogScriptId then
        engine.killScript(combatLogScriptId)
    end
    if injuryLogScriptId then
        engine.killScript(injuryLogScriptId)
    end
    if thoughtLogScriptId then
        engine.killScript(thoughtLogScriptId)
    end
    if unitLogScriptId then
        engine.killScript(unitLogScriptId)
    end
end

return M
