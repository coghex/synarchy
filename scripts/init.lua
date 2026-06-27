-- Main initialization script
local game = {}

local shellScriptId = nil
local uiScriptId = nil
local debugScriptId = nil
local unitManagerScriptId = nil
local unitInfoPanelScriptId = nil
local unitDragSelectScriptId = nil
local unitResourcesScriptId = nil
local unitAiScriptId = nil
local buildToolScriptId = nil
local buildingSpawnScriptId = nil
local tileEditorScriptId = nil
local pauseScriptId = nil
local buildingInfoPanelScriptId = nil
local popupScriptId = nil
local eventLogScriptId = nil
local combatLogScriptId = nil
local injuryLogScriptId = nil
local unitLogScriptId = nil

function game.init(scriptId)
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

    -- Per-unit log panel: opened from the unit-info "Log" button. It owns
    -- no stream (reads the other logs' stores); the tick just re-renders
    -- it live while it's open. 0.3s is plenty for a passive viewer.
    unitLogScriptId = engine.loadScript("scripts/unit_log.lua", 0.3)

    -- Initialize UI (which loads the main menu)
    uiScriptId = engine.loadScript("scripts/ui_manager.lua", 0.1)
end

function game.update(dt)
    -- Lazily resolve any structure texture-palette ids that need a runtime
    -- handle (after a save/load). No-op in steady state.
    require("scripts.structures").resolvePending()
end

-- Mouse-button constants (match Engine.Scripting.Lua.Thread::LuaMouseDownEvent)
local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

function game.onMouseDown(button, x, y)
    -- Only handle clicks that reach us — UI hit-tests run earlier in
    -- the input thread; if a UI element ate the click, this never fires.
    local debugOverlay = require("scripts.debug")

    -- Debug overlay's parallel hit-test gets first crack. If a debug
    -- rect (spawn button / list entry) eats the click, we stop here
    -- so the click can't fall through into selection / tile-cursor.
    if debugOverlay.tryClaimClick(button, x, y) then
        return
    end

    -- Debug anim panel (per-selection). Sits to the LEFT of the
    -- info-v2 pane and lists clickable animation names. Same
    -- parallel hit-test pattern as debug.lua so clicks on anim rows
    -- don't fall through into deselect-on-empty.
    local debugAnimPanel = require("scripts.debug_anim_panel")
    if debugAnimPanel.tryClaimClick(button, x, y) then
        return
    end

    -- Build tool gets first crack at mouse clicks when in placement
    -- mode, so the placement click doesn't fall through into unit
    -- selection / tile-cursor.
    local buildTool = require("scripts.build_tool")
    if buildTool.handleMouseDown(button, x, y) then
        return
    end

    -- Mine tool claims clicks while the mine tool mode is active
    -- (anchor / commit / cancel), so they don't fall through into
    -- unit selection.
    local mineTool = require("scripts.mine_tool")
    if mineTool.handleMouseDown(button, x, y) then
        return
    end

    if button == MOUSE_LEFT then
        -- Debug spawn mode: if armed, this click is a spawn, not a
        -- selection. Spawn at the hovered tile and stay armed.
        --
        -- Debug-spawned units always get faction "debug" — that
        -- tag means "player-controlled AND has no friendly-fire
        -- restrictions". Lets the user spawn two acolytes (or
        -- acolyte + bear, etc.) and make them fight for testing.
        -- Production unit sources still pass their canonical
        -- faction (portal spawns → "player"; world-gen wildlife
        -- spawns → "wildlife").
        if debugOverlay.armedDef then
            local gx, gy = world.getHoverTile()
            if gx and gy then
                unit.spawn(debugOverlay.armedDef, gx + 0.5, gy + 0.5,
                           nil, "debug")
            end
            return
        end

        -- Debug item-spawn mode: arms an item def; the click drops
        -- the item onto the ground exactly under the cursor (float
        -- coords from the fractional hover position; resting height
        -- derives from terrain at render). Tile-center fallback
        -- covers the no-hover edge case.
        if debugOverlay.armedItemDef then
            local hx, hy = world.getHoverPos()
            if hx and hy then
                item.spawnGround(debugOverlay.armedItemDef, hx, hy)
            else
                local gx, gy = world.getHoverTile()
                if gx and gy then
                    item.spawnGround(debugOverlay.armedItemDef,
                                     gx + 0.5, gy + 0.5)
                end
            end
            return
        end

        -- Debug fluid-spawn mode: arms a kind ("water" / "lava"); the
        -- click places one tile of that fluid on top of the column.
        if debugOverlay.armedFluidType then
            local gx, gy = world.getHoverTile()
            if gx and gy then
                local hud = require("scripts.hud")
                local worldId = (hud and hud.worldId) or "test_arena"
                world.setFluidTile(worldId, math.floor(gx), math.floor(gy),
                                   debugOverlay.armedFluidType)
            end
            return
        end

        -- Debug terrain-placement mode: arms a material id; the click
        -- raises the column at the hover tile one z of that material
        -- (WeAddTile through the edit log — persists like any edit).
        if debugOverlay.armedTerrainId then
            local gx, gy = world.getHoverTile()
            if gx and gy then
                local hud = require("scripts.hud")
                local worldId = (hud and hud.worldId) or "test_arena"
                world.addTile(worldId, math.floor(gx), math.floor(gy),
                              debugOverlay.armedTerrainId)
            end
            return
        end

        -- Debug location-stamp mode: arms a location def name; the click
        -- stamps that premade structure (room/outpost/...) anchored at the
        -- hover tile (world.setCell terrain edits + content spawns).
        if debugOverlay.armedLocation then
            local gx, gy = world.getHoverTile()
            if gx and gy then
                local hud = require("scripts.hud")
                local worldId = (hud and hud.worldId) or "test_arena"
                local locations = require("scripts.locations")
                locations.stamp(debugOverlay.armedLocation,
                                math.floor(gx), math.floor(gy), worldId)
            end
            return
        end

        -- Debug structure-placement mode: arms a kind (wall/floor/ceiling/
        -- post). Floor/ceiling/post place on the clicked tile; a wall goes
        -- in the clicked QUARTER of the tile (→ its diamond edge).
        if debugOverlay.armedStructure then
            -- Derive the tile from the FRACTIONAL hover position (floor), NOT
            -- getHoverTile: the latter rounds in a ~0.17-tile-shifted space, so
            -- near a tile border it disagrees with the quarter-corner/edge frac
            -- (computed from getHoverPos) → posts landed on the wrong tile and
            -- the floor-gate flaked. floor(hx,hy) keeps tile + corner consistent.
            local hx, hy = world.getHoverPos()
            if hx and hy then
                local structures = require("scripts.structures")
                structures.placeKind(math.floor(hx), math.floor(hy),
                                     debugOverlay.armedStructure, hx, hy)
            end
            return
        end

        local id = unit.hitTestAt(x, y)
        local shift = engine.isKeyDown("LeftShift")
                      or engine.isKeyDown("RightShift")
        if id then
            -- Hit a unit. Shift adds to the current selection;
            -- otherwise replace. The unit_info_panel watcher will
            -- see the change next tick and push unit info into the
            -- HUD panel + clear any tile cursor.
            if shift then
                local current = unit.getSelected() or {}
                local seen = {}
                local merged = {}
                for _, uid in ipairs(current) do
                    if not seen[uid] then
                        seen[uid] = true
                        table.insert(merged, uid)
                    end
                end
                if not seen[id] then table.insert(merged, id) end
                unit.setSelection(merged)
            else
                unit.select(id)
            end
            -- Selecting a unit takes over the info panel — deselect
            -- any building/item so the panel doesn't flicker between
            -- schemas.
            building.deselect()
            item.deselect()
        else
            -- No unit hit. Try a ground item (click priority:
            -- units > items > buildings — moving things win).
            local gid = item.hitTestAt(x, y)
            if gid then
                item.select(gid)
                if not shift then
                    unit.deselectAll()
                    building.deselect()
                end
            else
                -- No item. Try a building.
                local bid = building.hitTestAt(x, y)
                if bid then
                    building.select(bid)
                    item.deselect()
                    if not shift then unit.deselectAll() end
                else
                    -- Click missed everything. With Shift held, keep
                    -- the current selection (so shift-dragging from
                    -- empty terrain can extend it). Otherwise deselect.
                    if not shift then
                        unit.deselectAll()
                        building.deselect()
                        item.deselect()
                    end
                end
            end
        end
    elseif button == MOUSE_RIGHT then
        -- Right-click is a cancel for debug spawn mode (highest priority).
        if debugOverlay.armedDef then
            debugOverlay.clearArmed()
            return
        end
        if debugOverlay.armedItemDef then
            debugOverlay.clearArmedItem()
            return
        end
        if debugOverlay.armedTerrainId then
            debugOverlay.clearArmedTerrain()
            return
        end
        if debugOverlay.armedLocation then
            debugOverlay.clearArmedLocation()
            return
        end
        if debugOverlay.armedStructure then
            debugOverlay.clearArmedStructure()
            return
        end
        -- Storage building right-click → "Contents" menu, regardless
        -- of unit selection. Move commands still work on non-cargo
        -- tiles. building.hitTestAt takes framebuffer pixel coords
        -- via the same conversion the tile-menu branch uses below.
        do
            local cargoBid = building.hitTestAt(x, y)
            if cargoBid then
                local cap = building.getStorageCapacity(cargoBid)
                local activity = building.getActivity(cargoBid)
                if cap and cap > 0 and activity == "built" then
                    local fbW, fbH = engine.getFramebufferSize()
                    local ww, wh   = engine.getWindowSize()
                    local mx, my   = x, y
                    if ww and wh and ww > 0 and wh > 0 then
                        mx = x * (fbW / ww)
                        my = y * (fbH / wh)
                    end
                    local contextMenu =
                        require("scripts.ui.context_menu")
                    local cargoPanel =
                        require("scripts.cargo_inventory_panel")
                    contextMenu.show({
                        { label = "Contents",
                          callback = function()
                              cargoPanel.openFor(cargoBid, mx, my)
                          end },
                    }, mx, my)
                    return
                end
            end
        end
        -- Right-click on a unit → Info / Attack context menu.
        -- Runs before the move-when-selected branch so right-clicking
        -- a unit doesn't get interpreted as a move-to-tile order.
        -- "Attack" only appears when at least one player-faction unit
        -- is selected; it's greyed when every player attacker shares
        -- the target's faction (no friendly-fire by default — the
        -- force-attack override lands in a later phase).
        do
            local targetUid = unit.hitTestAt(x, y)
            if targetUid then
                local fbW, fbH = engine.getFramebufferSize()
                local ww, wh   = engine.getWindowSize()
                local mx, my   = x, y
                if ww and wh and ww > 0 and wh > 0 then
                    mx = x * (fbW / ww)
                    my = y * (fbH / wh)
                end
                local contextMenu = require("scripts.ui.context_menu")
                local selectedUids = unit.getSelected() or {}
                local targetFac    = unit.getFaction(targetUid)
                local items = {
                    { label = "Info",
                      callback = function()
                          unit.select(targetUid)
                          -- Mirror the left-click unit-selection path:
                          -- selecting a unit takes over the info panel,
                          -- so clear any building/item selection to keep
                          -- the shared HUD panel from flickering schemas.
                          building.deselect()
                          item.deselect()
                      end },
                }
                -- Filter selection down to player-commandable
                -- attackers (faction "player" or "debug"), excluding
                -- the target itself.
                local attackers = {}
                for _, uid in ipairs(selectedUids) do
                    local fac = unit.getFaction(uid)
                    if uid ~= targetUid
                       and (fac == "player" or fac == "debug") then
                        table.insert(attackers, uid)
                    end
                end
                if #attackers > 0 then
                    -- Friendly check: every attacker shares the
                    -- target's faction → Attack greyed. Debug-faction
                    -- attackers bypass this check entirely (the whole
                    -- point of "debug" is no friendly-fire restriction
                    -- so the player can stage acolyte-vs-acolyte
                    -- fights in the debug overlay).
                    local allFriendly = true
                    for _, uid in ipairs(attackers) do
                        local fac = unit.getFaction(uid)
                        if fac == "debug" or fac ~= targetFac then
                            allFriendly = false
                            break
                        end
                    end
                    local unitAi = require("scripts.unit_ai")
                    table.insert(items, {
                        label    = "Attack",
                        enabled  = not allFriendly,
                        callback = function()
                            -- Player order → committed (holds far longer
                            -- before futility breaks it; soft, not absolute).
                            for _, uid in ipairs(attackers) do
                                unitAi.commandAttack(uid, targetUid, true)
                            end
                        end,
                    })
                end
                -- Treat bleeding: a selected unit that KNOWS bleed-control
                -- dresses the target's worst bleeding wound, drawing
                -- bandages from a first-aid kit carried by the medic OR
                -- any other selected unit (e.g. the technomule standing
                -- by). Greyed until both a kit and a bleeding wound exist.
                do
                    local medic
                    for _, uid in ipairs(selectedUids) do
                        if uid ~= targetUid
                           and unit.getKnowledge(uid, "bleed_control") then
                            medic = uid; break
                        end
                    end
                    if medic then
                        local function hasBandages(uid)
                            for _, it in ipairs(unit.getInventory(uid) or {}) do
                                if it.kind == "container" then
                                    for _, r in ipairs(unit.getItemContents(
                                                  uid, it.defName) or {}) do
                                        if r.defName == "bandage"
                                           and (r.count or 0) > 0 then
                                            return true
                                        end
                                    end
                                end
                            end
                            return false
                        end
                        local kitOwner
                        if hasBandages(medic) then
                            kitOwner = medic
                        else
                            for _, uid in ipairs(selectedUids) do
                                if hasBandages(uid) then kitOwner = uid; break end
                            end
                        end
                        local bleeding = false
                        for _, w in ipairs(unit.getWounds(targetUid) or {}) do
                            if (w.bandage or 1) > 0.02
                               and w.kind ~= "concussion" then
                                bleeding = true; break
                            end
                        end
                        table.insert(items, {
                            label   = "Treat bleeding",
                            enabled = (kitOwner ~= nil) and bleeding,
                            callback = function()
                                local res = unit.treatBleeding(
                                    medic, targetUid, kitOwner)
                                if res then
                                    local msg, cat
                                    if res.ok then
                                        local pct = math.floor(
                                            (res.seep or 0) * 100 + 0.5)
                                        msg = (pct <= 0)
                                            and "Bleeding stopped"
                                            or string.format(
                                                "Bleeding cut to %d%%", pct)
                                        if (res.bandagesUsed or 1) > 1 then
                                            msg = msg .. string.format(
                                                " (%d bandages used)",
                                                res.bandagesUsed)
                                        end
                                        cat = "unit_event"
                                    else
                                        msg = "Treatment failed: "
                                              .. (res.message or "")
                                        cat = "unit_warning"   -- red; a failed job
                                    end
                                    -- Tag the patient so it shows in their Log.
                                    engine.emitEventForUnit(cat, msg, targetUid)
                                end
                            end,
                        })
                        -- Treat infection: administer antibiotics (the CURE)
                        -- to an infected wound. Greyed until the target has an
                        -- infected wound AND a kit with antibiotics is on the
                        -- medic or another selected unit.
                        local function hasAntibiotics(uid)
                            for _, it in ipairs(unit.getInventory(uid) or {}) do
                                if it.kind == "container" then
                                    for _, r in ipairs(unit.getItemContents(
                                                  uid, it.defName) or {}) do
                                        if r.defName == "antibiotics"
                                           and (r.fill or 0) > 0 then
                                            return true
                                        end
                                    end
                                end
                            end
                            return false
                        end
                        -- The cure needs INFECTION-CONTROL knowledge, a
                        -- different skill from bleed-control; resolve a medic
                        -- for it independently (may be the same acolyte, who
                        -- typically knows both).
                        local infMedic
                        for _, uid in ipairs(selectedUids) do
                            if uid ~= targetUid
                               and unit.getKnowledge(uid, "infection_control") then
                                infMedic = uid; break
                            end
                        end
                        local abOwner
                        if infMedic and hasAntibiotics(infMedic) then
                            abOwner = infMedic
                        else
                            for _, uid in ipairs(selectedUids) do
                                if hasAntibiotics(uid) then abOwner = uid; break end
                            end
                        end
                        local infected = false
                        for _, w in ipairs(unit.getWounds(targetUid) or {}) do
                            if (w.infection or 0) >= 0.1 then
                                infected = true; break
                            end
                        end
                        table.insert(items, {
                            label   = "Treat infection",
                            enabled = (infMedic ~= nil) and (abOwner ~= nil)
                                      and infected,
                            callback = function()
                                local res = unit.treatInfection(
                                    infMedic, targetUid, abOwner)
                                if res then
                                    local msg, cat
                                    if res.ok then
                                        local pct = math.floor(
                                            (res.infection or 0) * 100 + 0.5)
                                        msg = (pct <= 0)
                                            and "Infection cleared"
                                            or string.format(
                                                "Infection cut to %d%%", pct)
                                        cat = "unit_event"
                                    else
                                        msg = "Treatment failed: "
                                              .. (res.message or "")
                                        cat = "unit_warning"
                                    end
                                    engine.emitEventForUnit(cat, msg, targetUid)
                                end
                            end,
                        })
                    end
                end
                contextMenu.show(items, mx, my)
                return
            end
        end
        -- Right-click on a ground item → context menu. With units
        -- selected: Info / Pick up / Move here. Without: just Info.
        -- Pick up dispatches the NEAREST selected unit; capacity is
        -- checked at the moment of pickup (it can change en route).
        do
            local gid = item.hitTestAt(x, y)
            if gid then
                local fbW, fbH = engine.getFramebufferSize()
                local ww, wh   = engine.getWindowSize()
                local mx, my   = x, y
                if ww and wh and ww > 0 and wh > 0 then
                    mx = x * (fbW / ww)
                    my = y * (fbH / wh)
                end
                local contextMenu = require("scripts.ui.context_menu")
                local menuItems = {
                    { label = "Info",
                      callback = function()
                          item.select(gid)
                          unit.deselectAll()
                          building.deselect()
                      end },
                }
                local selUids = unit.getSelected() or {}
                if #selUids > 0 then
                    local ipos = nil
                    for _, g in ipairs(item.listGround() or {}) do
                        if g.id == gid then ipos = g; break end
                    end
                    if ipos then
                        local unitAi = require("scripts.unit_ai")
                        table.insert(menuItems, {
                            label = "Pick up",
                            callback = function()
                                local best, bestUid = math.huge, nil
                                for _, uid in ipairs(selUids) do
                                    local info = unit.getInfo(uid)
                                    if info then
                                        local d = (info.gridX - ipos.x) ^ 2
                                                + (info.gridY - ipos.y) ^ 2
                                        if d < best then
                                            best, bestUid = d, uid
                                        end
                                    end
                                end
                                if bestUid then
                                    unitAi.commandPickup(bestUid, gid)
                                end
                            end })
                        table.insert(menuItems, {
                            label = "Move here",
                            callback = function()
                                for _, uid in ipairs(selUids) do
                                    unitAi.commandMove(uid, ipos.x, ipos.y)
                                end
                            end })
                    end
                end
                contextMenu.show(menuItems, mx, my)
                return
            end
        end

        -- Right-click is a move order when units are selected.
        -- hud.onMouseDown also fires on right-click and clears the
        -- tile cursor — that's fine, it doesn't touch unit selection.
        local selected = unit.getSelected()
        if selected and #selected > 0 then
            local gx, gy = world.getHoverTile()
            if gx and gy then
                local tx = gx + 0.5
                local ty = gy + 0.5
                local unitAi = require("scripts.unit_ai")
                for _, uid in ipairs(selected) do
                    -- Route through the AI so the command becomes a
                    -- utility-scored candidate that high-priority
                    -- needs (thirst, etc.) can interrupt and resume.
                    -- No explicit speed → the "ordered" regime (a
                    -- sustainable push above comfort). A hard-coded
                    -- fast speed here exhausts the unit's stamina and
                    -- collapses it mid-move.
                    unitAi.commandMove(uid, tx, ty)
                end
            end
        else
            -- No selection → open the tile context menu. Capture the
            -- right-clicked tile NOW (the cursor moves once the menu
            -- opens) and stash it in the callbacks. Currently a one-
            -- item menu ("Info") as a smoke test of the right-click +
            -- context-menu plumbing; per-target providers replace
            -- this hardcoded list later.
            local gx, gy = world.getHoverTile()
            if gx and gy then
                local hud = require("scripts.hud")
                local contextMenu = require("scripts.ui.context_menu")
                local fbW, fbH = engine.getFramebufferSize()
                local ww, wh = engine.getWindowSize()
                local mx, my = x, y
                if ww and wh and ww > 0 and wh > 0 then
                    mx = x * (fbW / ww)
                    my = y * (fbH / wh)
                end
                local tileX, tileY = gx, gy
                contextMenu.show({
                    { label = "Info",
                      callback = function()
                          -- Drive the HUD toolbar widget so the bottom-
                          -- left icon flips to the info tool. The
                          -- toggle's onChange runs world.setToolMode +
                          -- the build/tile_editor side effects, so this
                          -- single call replaces a manual setToolMode.
                          local toggle = require("scripts.ui.toggle")
                          toggle.applyOptionByName(
                              hud.toolToggleId, "tool_info")
                          -- selectTile uses the direct tile-coord
                          -- select API — the cursor's pixel-hover state
                          -- has already moved to the menu, so the
                          -- usual hover+select would pick the wrong
                          -- tile.
                          world.selectTile(hud.worldId, tileX, tileY)
                          local tileEditor =
                              require("scripts.tile_editor")
                          tileEditor.onTileSelected(tileX, tileY)
                      end },
                }, mx, my)
            end
        end
    end
end

-- onMouseUp fires on EVERY physical release — unlike onMouseDown,
-- which only fires for presses that reached the game world (UI
-- elements and the tooltip lock eat theirs). UI widget drags (slider
-- knob, scrollbar tab) start from a UI click callback and depend on
-- the unconditional release to end. downRoute says where the matching
-- press went: "game" (onMouseDown fired), "ui" (a UI element ate it),
-- or "swallowed" (tooltip lock / minimized window). To pair strictly
-- with onMouseDown, guard on downRoute == "game".
function game.onMouseUp(button, x, y, downRoute)
end

function game.onKeyDown(key)
    -- Space toggles pause first — works regardless of tool mode or
    -- selection state, and shouldn't be eaten by a tool's local
    -- handler.
    if key == "Space" then
        require("scripts.pause").toggle()
        return
    end

    -- Player-events Escape cascade. Hardcoded to Escape (not
    -- routed through engine.isActionDown) because the user said
    -- the escape binding is fixed; only alphanumerics are
    -- rebindable.
    --   Shift+Esc  → dismiss every active popup + flush pending
    --   Esc        → dismiss topmost popup, OR close event log,
    --                OR fall through to selection-clear below
    if key == "Escape" then
        -- Context menu takes priority — close it before any popup /
        -- selection-cleanup runs. Matches "Escape cancels the topmost
        -- transient UI thing" intuition.
        local contextMenu = require("scripts.ui.context_menu")
        if contextMenu.handleEscape() then return end

        -- Cargo inventory popup is next-most-transient.
        local cargoPanel = require("scripts.cargo_inventory_panel")
        if cargoPanel.handleKeyDown(key) then return end

        -- Item contents popup (unit-carried container) — same tier.
        local itemContents = require("scripts.item_contents_panel")
        if itemContents.handleKeyDown(key) then return end

        local popup = require("scripts.popup")
        local shift = engine.isKeyDown("LeftShift")
                      or engine.isKeyDown("RightShift")
        if shift then
            if popup.dismissAll() > 0 then return end
        else
            if popup.dismissTopmost() then return end
        end
        local eventLog = require("scripts.event_log")
        if eventLog.isVisible() then
            eventLog.hide()
            return
        end
        local combatLog = require("scripts.combat_log")
        if combatLog.isVisible() then
            combatLog.hide()
            return
        end
        local injuryLog = require("scripts.injury_log_panel")
        if injuryLog.isVisible() then
            injuryLog.hide()
            return
        end
        local unitLog = require("scripts.unit_log")
        if unitLog.isVisible() then
            unitLog.hide()
            return
        end
        -- (fall through to the existing unit-deselect path below)
    end

    -- User-rebindable: toggle the currently-selected log panel
    -- (event or combat, matching the HUD icon's mode). Default
    -- bind is L. engine.isActionDown is true while the action's
    -- key is held; the press-transition broadcast makes "down"
    -- coincide with "just pressed" for this handler.
    if engine.isActionDown("toggleEventLog") then
        local hud = require("scripts.hud")
        if hud.logMode == "combat" then
            require("scripts.combat_log").toggle()
        else
            require("scripts.event_log").toggle()
        end
        return
    end

    -- Build tool's Esc cancels placement before the default Esc
    -- handler clears unit selection.
    local buildTool = require("scripts.build_tool")
    if buildTool.handleKeyDown(key) then
        return
    end
    -- Mine tool's Esc cancels a pending designation anchor.
    local mineTool = require("scripts.mine_tool")
    if mineTool.handleKeyDown(key) then
        return
    end
    -- ESC clears any active unit selection.
    -- Doesn't conflict with shell/UI focus: those modes consume ESC
    -- earlier in the input thread (LuaFocusLost / LuaUIEscape) and
    -- never reach this broadcast.
    if key == "Escape" then
        local selected = unit.getSelected()
        if #selected > 0 then
            unit.deselectAll()
        end
        -- ESC also clears the active cursor (tile/chunk) selection so it
        -- obeys the same cancel semantics as unit selection (#180). Both
        -- the world-tile (zoomed-in) and zoom-map chunk (zoomed-out)
        -- selections are cleared unconditionally rather than gating on
        -- hud.currentView: a selection made in one view persists in the
        -- engine cursor state across a zoom (and through the mid-fade
        -- band where currentView is "none"), so a view-gated clear would
        -- miss it there (e.g. select a tile, stop in the fade zone, press
        -- Escape — the old selection would survive). clearWorld/
        -- ZoomCursorSelect is idempotent (no-op when nothing is
        -- selected), so clearing both is safe. The cursor poller tears
        -- down the dependent tile/chunk info panel — and, via the empty
        -- onSetInfoText broadcast, the tile-editor popup — on the next
        -- tick. Gated on hud.visible like onMouseDown so a menu-open Esc
        -- never acts on a hidden world.
        local hud = require("scripts.hud")
        if hud and hud.visible and hud.worldId then
            world.clearWorldCursorSelect(hud.worldId)
            world.clearZoomCursorSelect(hud.worldId)
        end
    end
end

function game.onKeyUp(key)
end

function game.shutdown()
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
    if pauseScriptId then
        engine.killScript(pauseScriptId)
    end
    if buildingInfoPanelScriptId then
        engine.killScript(buildingInfoPanelScriptId)
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
    if unitLogScriptId then
        engine.killScript(unitLogScriptId)
    end
end

return game
