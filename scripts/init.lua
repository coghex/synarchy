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

function game.init(scriptId)
    -- Initialize debug
    debugScriptId = engine.loadScript("scripts/debug.lua", 0.1)

    -- Initialize shell
    shellScriptId = engine.loadScript("scripts/shell.lua", 0.5)

    -- Initialize unit manager (loads unit definitions from YAML)
    unitManagerScriptId = engine.loadScript("scripts/unit_manager.lua", 0.1)

    -- Initialize unit info panel (shown when a unit is selected).
    -- Ticks at 0.1s — slow enough to be cheap, fast enough to feel
    -- responsive when selection changes via click.
    unitInfoPanelScriptId = engine.loadScript("scripts/unit_info_panel.lua", 0.1)

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

    -- Initialize UI (which loads the main menu)
    uiScriptId = engine.loadScript("scripts/ui_manager.lua", 0.1)
end

function game.update(dt)
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

    -- Build tool gets first crack at mouse clicks when in placement
    -- mode, so the placement click doesn't fall through into unit
    -- selection / tile-cursor.
    local buildTool = require("scripts.build_tool")
    if buildTool.onMouseDown(button, x, y) then
        return
    end

    if button == MOUSE_LEFT then
        -- Debug spawn mode: if armed, this click is a spawn, not a
        -- selection. Spawn at the hovered tile and stay armed.
        if debugOverlay.armedDef then
            local gx, gy = world.getHoverTile()
            if gx and gy then
                unit.spawn(debugOverlay.armedDef, gx + 0.5, gy + 0.5)
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
        else
            -- Click missed all units. With Shift held, keep the
            -- current selection (so shift-dragging from empty terrain
            -- can extend it). Otherwise deselect.
            if not shift then
                unit.deselectAll()
            end
        end
    elseif button == MOUSE_RIGHT then
        -- Right-click is a cancel for debug spawn mode (highest priority).
        if debugOverlay.armedDef then
            debugOverlay.clearArmed()
            return
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
                    unitAi.commandMove(uid, tx, ty, 2.0)
                end
            end
        end
    end
end

function game.onMouseUp(button, x, y)
end

function game.onKeyDown(key)
    -- Build tool's Esc cancels placement before the default Esc
    -- handler clears unit selection.
    local buildTool = require("scripts.build_tool")
    if buildTool.onKeyDown(key) then
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
    end
end

function game.onKeyUp(key)
end

function game.shutdown()
    if debugScriptId then
        engine.killScript(debugScriptId)
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
end

return game
